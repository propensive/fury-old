/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.7.14. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
   ║                                                                                                           ║
   ║ The primary distribution site is: https://propensive.com/                                                 ║
   ║                                                                                                           ║
   ║ Licensed under  the Apache License,  Version 2.0 (the  "License"); you  may not use  this file  except in ║
   ║ compliance with the License. You may obtain a copy of the License at                                      ║
   ║                                                                                                           ║
   ║     http://www.apache.org/licenses/LICENSE-2.0                                                            ║
   ║                                                                                                           ║
   ║ Unless required  by applicable law  or agreed to in  writing, software  distributed under the  License is ║
   ║ distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. ║
   ║ See the License for the specific language governing permissions and limitations under the License.        ║
   ╚═══════════════════════════════════════════════════════════════════════════════════════════════════════════╝
*/
package fury.core

import fury.strings._, fury.io._, fury.model._

import exoskeleton._
import guillotine._

import scala.util._

import language.higherKinds

case class EarlyCompletions() extends FuryException

sealed class ExitStatus(val code: Int)
case object Done  extends ExitStatus(0)
case object Abort extends ExitStatus(1)
case object Continuation extends ExitStatus(91)

object NoCommand { def unapply(cli: Cli[CliParam[_]]): Boolean = cli.args.isEmpty }

abstract class Cmd(val cmd: String, val description: String) {

  def unapply(cli: Cli[_]): Option[Cli[_]] = cli.args.headOption.flatMap { head =>
    if(head == cmd) Some(cli.copy(args = cli.args.tail)) else None
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
      cli.stdout,
      ParamMap(cli.args.suffix.map(_.value).tail: _*),
      cli.args(Args.ParamNoArg).toOption,
      cli.optCompletions,
      cli.env,
      cli.pid
    )

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
    def output: List[String] = List(
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

case class Cli[+Hinted <: CliParam[_]](stdout: java.io.PrintWriter,
                                       args: ParamMap,
                                       command: Option[Int],
                                       optCompletions: List[Cli.OptCompletion[_]],
                                       env: Environment,
                                       pid: Pid) {

  class Call private[Cli] () {
    def apply[T](param: CliParam[T])(implicit ev: Hinted <:< param.type): Try[T] = args.get(param.param)
    def suffix: List[String] = args.suffix.to[List].map(_.value)
  }

  def cols: Int = Terminal.columns(env).getOrElse(100)

  def call()(implicit log: Log): Try[Call] = {
    if(completion) {
      stdout.println(optCompletions.flatMap(_.output).mkString("\n"))
      stdout.flush()
      Failure(EarlyCompletions())
    } else {
      log.attach(LogStyle(stdout, pid, false, true))
      Success(new Call())
    }
  }

  def peek[T](param: CliParam[T]): Option[T] = args.get(param.param).toOption

  def pwd: Try[Path] = env.workDir.ascribe(FileNotFound(Path("/"))).map(Path(_))

  lazy val newLayout: Try[Layout] = pwd.map { pwd => Layout(Path(env.variables("HOME")), pwd, env, pwd) }

  lazy val layout: Try[Layout] = pwd.flatMap { pwd => Layout.find(Path(env.variables("HOME")), pwd, env) }
  
  def next: Option[String] = args.prefix.headOption.map(_.value)
  def completion: Boolean = command.isDefined
  def prefix(str: String): Cli[Hinted] = copy(args = ParamMap((str :: args.args.to[List]): _*))
  def tail: Cli[Hinted] = copy(args = args.tail)
  def opt[T: Default](param: CliParam[T]): Try[Option[T]] = Success(args(param.param).toOption)

  def abort(msg: UserMsg): ExitStatus = {
    if(!completion) write(msg)
    Abort
  }

  def continuation(script: String): ExitStatus = {
    val pw = new java.io.PrintWriter((Installation.scriptsDir.extant() / s"exec_${pid.pid}").javaFile)
    pw.write(script)
    pw.write("\n")
    pw.close()
    Continuation
  }

  def hint[T: StringShow: Descriptor]
          (arg: CliParam[_], hints: Traversable[T])
          : Try[Cli[Hinted with arg.type]] = {
    val newHints = Cli.OptCompletion(arg, implicitly[Descriptor[T]].wrap(implicitly[StringShow[T]], hints))

    Success(copy(optCompletions = newHints :: optCompletions))
  }

  def hint(arg: CliParam[_]) = Success(copy(optCompletions = Cli.OptCompletion(arg, "()") :: optCompletions))

  private[this] def write(msg: UserMsg): Unit = {
    stdout.println(msg.string(ManagedConfig().theme))
    stdout.flush()
  }

  def completeCommand(cmd: MenuStructure[_]): Try[Nothing] =
    command.map { no =>
      val name = if(no == 1) "Command" else "Subcommand"
      val optCompletions = List(Cli.CmdCompletion(no - 1, name, cmd match {
        case act: Action[_]   => Nil
        case menu: Menu[_, _] => menu.items.filter(_.show).to[List]
      }))
      stdout.println(optCompletions.flatMap(_.output).mkString("\n"))
      stdout.flush()
      Failure(EarlyCompletions())
    }.getOrElse {
      args.prefix.headOption.fold(Failure(UnknownCommand(""))) { arg => Failure(UnknownCommand(arg.value)) }
    }

}

case class Completions(completions: List[Cli.OptCompletion[_]] = Nil) {
  def hint[T: StringShow: Descriptor](arg: CliParam[_], hints: Traversable[T]): Completions = {
    val newHints = Cli.OptCompletion(arg, implicitly[Descriptor[T]].wrap(implicitly[StringShow[T]], hints))

    copy(completions = newHints :: completions)
  }

  def hint(arg: CliParam[_]): Completions = copy(completions = Cli.OptCompletion(arg, "()") :: completions)
}
