/*
  Fury, version 0.4.0. Copyright 2018-19 Jon Pretty, Propensive Ltd.

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
import fury.io._
import fury.error._
import fury.ogdl._
import guillotine._

import scala.util._

case class EarlyCompletions() extends FuryException

sealed class ExitStatus(val code: Int)
case object Done  extends ExitStatus(0)
case object Abort extends ExitStatus(1)

object NoCommand { def unapply(cli: Cli[CliParam[_]]): Boolean = cli.args.isEmpty }

abstract class Cmd(val cmd: String, val description: String) {

  def unapply(cli: Cli[_]): Option[Cli[_]] = cli.args.headOption.flatMap { head =>
    if (head == cmd) Some(cli.copy(args = cli.args.tail)) else None
  }

  def completions: List[Cmd] = Nil
  def default: Option[Cmd]   = None
}

case class CliParam[T: Param.Extractor](shortName: Char, longName: Symbol, description: String) {
  val param: SimpleParam[T] = Param[T](shortName, longName)
}

object Cli {

  def asCompletion[H <: CliParam[_]](menu: => Menu[Cli[H], _])(cli: Cli[H]) = {
    val newCli = Cli[H](
        cli.output,
        ParamMap(cli.args.suffix.map(_.value).tail: _*),
        cli.args(Args.ParamNoArg).opt,
        cli.optCompletions,
        cli.env)

    menu(newCli, newCli)
  }

  def escape(s: String) = s.replaceAll(":", "\\:")

  sealed trait Completion { def output: List[String] }

  case class CmdCompletion(id: Int, description: String, options: List[MenuStructure[_]])
      extends Completion {

    def output: List[String] = List {
      val commands = options.map { cmd =>
        escape(s"${cmd.command.name}:'${cmd.description.string(Theme.NoColor)}'")
      }.join(" ")
      s"$id:${escape(description)}:(($commands))"
    }
  }

  case class OptCompletion[T](arg: CliParam[_], hints: String) extends Completion {

    def output: List[String] =
      List(
          str"--${arg.longName.name}[${arg.description}]:${arg.longName.name}:$hints",
          str"-${arg.shortName}[${arg.description}]:${arg.longName.name}:$hints"
      )
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

object Io {

  def silent(config: Config): Io = new Io(new java.io.PrintStream(int => ()), config)

}

class Io(private[this] val output: java.io.PrintStream, config: Config) {

  def print(msg: UserMsg): Unit = output.print(msg.string(config.theme))

  def println(msg: UserMsg): Unit = output.println(msg.string(config.theme))

  def save[T: OgdlWriter](value: T, path: Path): Unit = {
    val stringBuilder: StringBuilder = new StringBuilder()
    Ogdl.serialize(stringBuilder, implicitly[OgdlWriter[T]].write(value))
    val content: String = stringBuilder.toString
    Outcome
      .rescue[java.io.IOException](FileWriteError(path)) {
        val writer = new java.io.BufferedWriter(new java.io.FileWriter(path.javaPath.toFile))
        writer.write(content).unit
        writer.write('\n').unit
        writer.close()
      }
      .unit // FIXME: Don't discard the result
  }

  def await(success: Boolean = true): ExitStatus = {
    output.flush()
    if (success) Done else Abort
  }

}

case class Cli[+Hinted <: CliParam[_]](
    output: java.io.PrintStream,
    args: ParamMap,
    command: Option[Int],
    optCompletions: List[Cli.OptCompletion[_]],
    env: Environment) {

  class Invocation private[Cli] () {

    def apply[T](param: CliParam[T])(implicit ev: Hinted <:< param.type): Outcome[T] =
      args.get(param.param).toTry

    def io(): Outcome[Io] = Success(new Io(output, config))

  }

  def read(): Outcome[Invocation] = {
    val io: Io = new Io(output, config)
    if (completion) {
      io.println(optCompletions.flatMap(_.output).mkString("\n"))
      io.await()
      Failure(EarlyCompletions())
    } else Success(new Invocation())
  }

  def peek[T](param: CliParam[T]): Option[T] = args.get(param.param).opt

  lazy val layout: Outcome[Layout] =
    env.workDir.ascribe(FileNotFound(Path("/"))).map { pwd =>
      Layout(Path(env.variables("HOME")), Path(pwd), env)
    }

  lazy val config: Config = layout.flatMap(Config.read()(env, _)).toOption.getOrElse(Config())

  def next: Option[String] = args.prefix.headOption.map(_.value)

  def completion: Boolean = command.isDefined

  def prefix(str: String): Cli[Hinted] = copy(args = ParamMap((str :: args.args.to[List]): _*))

  def tail: Cli[Hinted] = copy(args = args.tail)

  def abort(msg: UserMsg): ExitStatus = {
    if (!completion) write(msg)
    Abort
  }

  def opt[T: Default](param: CliParam[T]): Outcome[Option[T]] = Success(args(param.param).opt)

  def hint[T: StringShow: Descriptor](
      arg: CliParam[_],
      hints: Traversable[T]
    ): Outcome[Cli[Hinted with arg.type]] = {
    val newHints =
      Cli.OptCompletion(arg, implicitly[Descriptor[T]].wrap(implicitly[StringShow[T]], hints))

    Success(copy(optCompletions = newHints :: optCompletions))
  }

  def hint(arg: CliParam[_]) =
    Success(copy(optCompletions = Cli.OptCompletion(arg, "()") :: optCompletions))

  private[this] def write(msg: UserMsg): Unit = {
    output.println(msg.string(config.theme))
    output.flush()
  }

  def completeCommand(cmd: MenuStructure[_]): Outcome[Nothing] =
    command.map { no =>
      val name = if (no == 1) "Command" else "Subcommand"
      val optCompletions = List(Cli.CmdCompletion(no - 1, name, cmd match {
        case act: Action[_]   => Nil
        case menu: Menu[_, _] => menu.items.filter(_.show).to[List]
      }))
      val io = new Io(output, config)
      io.println(optCompletions.flatMap(_.output).mkString("\n"))
      io.await()
      Failure(EarlyCompletions())
    }.getOrElse {
      args.prefix.headOption.map { arg =>
        Failure(UnknownCommand(arg.value))
      }.getOrElse(Failure(UnknownCommand("")))
    }

}

case class Completions(completions: List[Cli.OptCompletion[_]] = Nil) {

  def hint[T: StringShow: Descriptor](arg: CliParam[_], hints: Traversable[T]): Completions = {
    val newHints =
      Cli.OptCompletion(arg, implicitly[Descriptor[T]].wrap(implicitly[StringShow[T]], hints))

    copy(completions = newHints :: completions)
  }

  def hint(arg: CliParam[_]): Completions =
    copy(completions = Cli.OptCompletion(arg, "()") :: completions)
}
