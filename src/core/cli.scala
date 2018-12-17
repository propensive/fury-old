/*
  Fury, version 0.1.2. Copyright 2018 Jon Pretty, Propensive Ltd.

  The primary distribution site is: https://propensive.com/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required  by applicable  law or  agreed to  in writing,  software  distributed  under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
  express  or  implied.  See  the  License for  the specific  language  governing  permissions and
  limitations under the License.
                                                                                                  */
package fury

import exoskeleton._
import guillotine._
import mitigation._

case class EarlyCompletions() extends Exception

sealed class ExitStatus(val code: Int)
case object Done extends ExitStatus(0)
case object Abort extends ExitStatus(1)

object NoCommand { def unapply(cli: Cli[CliParam[_]]): Boolean = cli.args.isEmpty }

abstract class Cmd(val cmd: String, val description: String) {
  def unapply(cli: Cli[_]): Option[Cli[_]] = cli.args.headOption.flatMap { head =>
    if(head == cmd) Some(cli.copy(args = cli.args.tail)) else None
  }

  def completions: List[Cmd] = Nil
  def default: Option[Cmd] = None
}

case class CliParam[T: Param.Extractor](shortName: Char, longName: Symbol, description: String) {
  val param: SimpleParam[T] = Param[T](shortName, longName)
}

object Cli {
  
  def asCompletion[H <: CliParam[_]](menu: => Menu[Cli[H], _])(cli: Cli[H]) = {
    val newCli = Cli[H](cli.output, ParamMap(cli.args.suffix.map(_.value).tail: _*),
        cli.args(Args.ParamNoArg).opt, cli.optCompletions, cli.env)
  
    menu(newCli, newCli)
  }

  def escape(s: String) = s.replaceAll(":", "\\:")

  sealed trait Completion { def output: List[String] }
  
  case class CmdCompletion(id: Int, description: String,
      options: List[MenuStructure[_]]) extends Completion {
    
    def output: List[String] = List {
      val commands = options.map { cmd =>
        escape(s"${cmd.command.name}:'${cmd.description.string(Theme.NoColor)}'")
      }.join(" ")
      s"$id:${escape(description)}:(($commands))"
    }
  }
  
  case class OptCompletion[T](arg: CliParam[_], hints: String) extends Completion {
    def output: List[String] = {
      List(
        str"--${arg.longName.name}[${arg.description}]:${arg.longName.name}:$hints",
        str"-${arg.shortName}[${arg.description}]:${arg.longName.name}:$hints"
      )
    }
  }

}

object Descriptor {
  implicit def noDescription[T]: Descriptor[T] = new Descriptor[T] {
    def describe(value: T): UserMsg = msg""
    
    override def wrap(show: StringShow[T], xs: Traversable[T]): String =
      str"(${xs.map(show.show).map(_.replaceAll(":", "\\\\:")).join(" ")})"
  }
}

/** a Descriptor is a typeclass for providing a human-readable description of something */
trait Descriptor[T] {
  
  def describe(value: T): UserMsg
  
  def wrap(show: StringShow[T], xs: Traversable[T]): String = {
    val options = xs.map { elem =>
      s"""${Cli.escape(show.show(elem))}\\:${Cli.escape(describe(elem).string(Theme.NoColor))}"""
    }.join(" ")
    s"(($options))"
  }
}

case class Cli[+Hinted <: CliParam[_]]
              (output: java.io.PrintStream,
               args: ParamMap,
               command: Option[Int],
               optCompletions: List[Cli.OptCompletion[_]],
               env: Environment) {
 
  class Io private[Cli]() {

    def apply[T](param: CliParam[T])
                (implicit ev: Hinted <:< param.type)
                : Result[T, ~ | MissingArg | InvalidArgValue] = args.get(param.param)

    def print(msg: UserMsg): Unit = output.print(msg.string(config.theme))
    
    def println(msg: UserMsg): Unit = output.println(msg.string(config.theme))

    def save[T: OgdlWriter](value: T, path: Path): Unit = {
      val stringBuilder: StringBuilder = new StringBuilder()
      Ogdl.serialize(stringBuilder, implicitly[OgdlWriter[T]].write(value))
      val content: String = stringBuilder.toString
      Result.rescue[java.io.IOException](FileWriteError(path)) {
        val writer = new java.io.BufferedWriter(new java.io.FileWriter(path.javaPath.toFile))
        writer.write(content).unit
        writer.write('\n').unit
        writer.close()
      }.unit // FIXME: Don't discard the result
    }
    
    def await(success: Boolean = true): ExitStatus = {
      output.flush()
      if(success) Done else Abort
    }
  }

  def peek[T](param: CliParam[T]): Option[T] = args.get(param.param).opt

  lazy val layout: Result[Layout, ~ | FileNotFound] =
    env.workDir.ascribe(FileNotFound(Path("/"))).map { pwd =>
      Layout(Path(env.variables("HOME")), Path(pwd))
    }

  lazy val config: Config = layout.flatMap(Config.read()(env, _)).opt.getOrElse(Config())

  def next: Option[String] = args.prefix.headOption.map(_.value)
  
  def completion: Boolean = command.isDefined

  def prefix(str: String): Cli[Hinted] = copy(args = ParamMap((str :: args.args.to[List]): _*))

  def tail: Cli[Hinted] = copy(args = args.tail)

  def defaultTo(defaultCmd: Cmd) = copy(args = args + defaultCmd.cmd)
  
  def abort(msg: UserMsg): ExitStatus = {
    write(msg)
    Abort
  }
  
  lazy val shell: Shell = Shell()(env)

  def opt[T: Default](param: CliParam[T]): Result[Option[T], ~] = Answer(args(param.param).opt)

  def hint[T: StringShow: Descriptor](arg: CliParam[_], hints: Traversable[T]): Answer[_ <: Cli[Hinted with arg.type]] = {
    val newHints = Cli.OptCompletion(arg, implicitly[Descriptor[T]].wrap(implicitly[StringShow[T]],
        hints))
    
    Answer(copy(optCompletions = newHints :: optCompletions))
  }

  def hint(arg: CliParam[_]) =
    Answer(copy(optCompletions = Cli.OptCompletion(arg, "()") :: optCompletions))

  private[this] def write(msg: UserMsg): Unit = {
    output.println(msg.string(config.theme))
    output.flush()
  }

  def io(): Result[Io, ~ | EarlyCompletions] = {
    val io = new Io()
    if(completion) {
      io.println(optCompletions.flatMap(_.output).mkString("\n"))
      io.await()
      Result.abort(EarlyCompletions())
    } else Answer(io)
  }

  def completeCommand(cmd: MenuStructure[_]): Result[Nothing, ~ | UnknownCommand | EarlyCompletions] =
    command.map { no =>
      val name = if(no == 1) "Command" else "Subcommand"
      val optCompletions = List(Cli.CmdCompletion(no - 1, name, cmd match {
        case act: Action[_] => Nil
        case menu: Menu[_, _] => menu.items.filter(_.show).to[List]
      }))
      val io = new Io()
      io.println(optCompletions.flatMap(_.output).mkString("\n"))
      io.await()
      Result.abort(EarlyCompletions())
    }.getOrElse {
      args.prefix.headOption.map { arg =>
        Result.abort(UnknownCommand(arg.value))
      }.getOrElse(Result.abort(UnknownCommand("")))
    }
  
}

case class Completions(completions: List[Cli.OptCompletion[_]] = Nil) {
  def hint[T: StringShow: Descriptor](arg: CliParam[_], hints: Traversable[T]): Completions = {
    val newHints = Cli.OptCompletion(arg, implicitly[Descriptor[T]].wrap(implicitly[StringShow[T]],
        hints))
    
    copy(completions = newHints :: completions)
  }

  def hint(arg: CliParam[_]): Completions =
    copy(completions = Cli.OptCompletion(arg, "()") :: completions)
}
