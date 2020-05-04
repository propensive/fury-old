/*

    Fury, version 0.15.1. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

import fury.strings._, fury.io._, fury.model._

import exoskeleton._
import guillotine._

import scala.util._

import language.higherKinds

case class EarlyCompletions() extends FuryException
case class BadParams(param1: CliParam, param2: CliParam) extends FuryException
case class MissingParamChoice(param1: CliParam, param2: CliParam) extends FuryException
case class MissingParam(param: CliParam) extends FuryException
case class BadParamValue(param: CliParam, value: String) extends FuryException

sealed class ExitStatus(val code: Int)
case object Done  extends ExitStatus(0)
case object Abort extends ExitStatus(1)
case object Continuation extends ExitStatus(91)

object NoCommand { def unapply(cli: Cli): Boolean = cli.args.isEmpty }

abstract class Cmd(val cmd: String, val description: String) {

  def unapply(cli: Cli): Option[Cli] = cli.args.headOption.flatMap { head =>
    if(head == cmd) Some(Cli(cli.stdout, cli.args.tail, cli.command, cli.optCompletions, cli.env,
        cli.pid)) else None
  }

  def completions: List[Cmd] = Nil
  def default: Option[Cmd]   = None
}

object CliParam {
  def apply[T: Param.Extractor]
           (shortName: Char, longName: Symbol, description: String)
           : CliParam { type Type = T } =
    new CliParam(shortName, longName, description) {
      type Type = T
      def extractor: Param.Extractor[Type] = implicitly[Param.Extractor[T]]
    }
  
  implicit val msgShow: MsgShow[CliParam] = (p: CliParam) => UserMsg { theme =>
    theme.param(str"--${p.longName.name}")+" "+theme.gray("(")+theme.param(str"-${p.shortName.toString}")+
        theme.gray(")") }
}

abstract class CliParam(val shortName: Char,
                        val longName: Symbol,
                        val description: String) { cliParam =>
  
  case class Hinter(hints: Traversable[Type])
  
  type Type
  
  implicit def extractor: Param.Extractor[Type]

  val param: SimpleParam[Type] = Param[Type](shortName, longName)

  def hint(hints: Traversable[Type]): Hinter = Hinter(hints)
  def hint(hints: Try[Traversable[Type]]): Hinter = Hinter(hints.getOrElse(Traversable.empty))
  def hint(hints: Type*): Hinter = Hinter(hints)

  def apply()(implicit call: C#Call forSome { type C <: Cli { type Hinted <: cliParam.type } }) = call(this)
}

object Cli {

  def apply[H <: CliParam](stdout: java.io.PrintWriter,
                           args: ParamMap,
                           command: Option[Int],
                           optCompletions: List[Cli.OptCompletion],
                           env: Environment,
                           pid: Pid) =
    new Cli(stdout, args, command, optCompletions, env, pid) { type Hinted <: H }

  def asCompletion[H <: CliParam](menu: => Menu)(cli: Cli) = {
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

  case class CmdCompletion(id: Int, description: String, options: List[MenuStructure])
      extends Completion {

    def output: List[String] = List {
      val commands = options.map { cmd =>
        escape(s"${cmd.command.name}:'${cmd.description.string(Theme.NoColor)}'")
      }.join(" ")
      s"$id:${escape(description)}:(($commands))"
    }
  }

  case class OptCompletion(arg: CliParam, hints: String) extends Completion {
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

class Cli(val stdout: java.io.PrintWriter,
          val args: ParamMap,
          val command: Option[Int],
          val optCompletions: List[Cli.OptCompletion],
          val env: Environment,
          val pid: Pid) { cli =>

  type Hinted <: CliParam

  class Call private[Cli] () {
    def apply(param: CliParam)(implicit ev: Hinted <:< param.type): Try[param.Type] =
      args.get(param.param).toOption.ascribe(MissingParam(param))

    def suffix: List[String] = args.suffix.to[List].map(_.value)

    def exactlyOne(param1: CliParam, param2: CliParam)
                  (implicit ev: Hinted <:< param1.type with param2.type)
                  : Try[Either[param1.Type, param2.Type]] =
      atMostOne(param1, param2).flatMap(_.ascribe(MissingParamChoice(param1, param2)))
    
    def atMostOne(param1: CliParam, param2: CliParam)
                 (implicit ev: Hinted <:< param1.type with param2.type)
                 : Try[Option[Either[param1.Type, param2.Type]]] = {
      val left = args.get(param1.param).toOption
      val right = args.get(param2.param).toOption
      if(left.isDefined && right.isDefined) Failure(BadParams(param1, param2))
      else Success(left.map(Left(_)).orElse(right.map(Right(_))))
    }
  }

  def cols: Int = Terminal.columns(env).getOrElse(100)

  private lazy val logStyle: LogStyle = LogStyle(stdout, debug = false)

  def call()(implicit log: Log): Try[Call] = {
    if(completion) {
      stdout.println(optCompletions.flatMap(_.output).mkString("\n"))
      stdout.flush()
      Failure(EarlyCompletions())
    } else {
      log.attach(logStyle)
      Success(new Call())
    }
  }

  def askProjectAndModule(layer: Layer): Try[(Cli{
    type Hinted <: cli.Hinted with Args.ProjectArg.type with Args.ModuleArg.type
  }, Try[Project], Try[Module])] = {
    import fury.core.Args.{ ProjectArg, ModuleArg }
    for {
      cli          <- this.hint(ProjectArg, layer.projects)
      tryProject = for {
        projectId <- cli.preview(ProjectArg)(layer.main)
        project   <- layer.projects.findBy(projectId)
      } yield project

      cli          <- cli.hint(ModuleArg, tryProject.map(_.modules).getOrElse(Set.empty))
      tryModule = for {
        project  <- tryProject
        moduleId <- cli.preview(ModuleArg)(project.main)
        module   <- project.modules.findBy(moduleId)
      } yield module
    } yield (cli, tryProject, tryModule)
  }

  def forceLog(msg: UserMsg): Unit = logStyle.log(msg, System.currentTimeMillis, Log.Warn, pid)

  def action(blk: Call => Try[ExitStatus])(implicit log: Log): Try[ExitStatus] = call().flatMap(blk)

  def peek(param: CliParam): Option[param.Type] = args.get(param.param).toOption

  def preview(param: CliParam)(default: Option[param.Type] = None): Try[param.Type] = {
    val result = args.get(param.param)
    if(default.isDefined) result.recover {
      case _: exoskeleton.MissingArg => default.get
    } else result
  }

  def pwd: Try[Path] = env.workDir.ascribe(FileNotFound(Path("/"))).map(Path(_))

  lazy val newLayout: Try[Layout] = pwd.map { pwd => Layout(Path(env.variables("HOME")), pwd, env, pwd) }

  lazy val layout: Try[Layout] = pwd.flatMap { pwd => Layout.find(Path(env.variables("HOME")), pwd, env) }
  
  def next: Option[String] = args.prefix.headOption.map(_.value)
  def completion: Boolean = command.isDefined
  
  def prefix(str: String): Cli { type Hinted <: cli.Hinted } =
    Cli(stdout, ParamMap((str :: args.args.to[List]): _*), command, optCompletions, env, pid)
  
  def tail: Cli { type Hinted <: cli.Hinted } = {
    
    val newArgs = if(args.headOption.map(_.length) == Some(2)) ParamMap(args.args.head.tail +:
        args.args.tail: _*) else args.tail
    
    Cli(stdout, newArgs, command, optCompletions, env, pid)
  }
  
  def opt[T](param: CliParam)(implicit ext: Default[param.Type]): Try[Option[param.Type]] =
    Success(args(param.param).toOption)

  def abort(msg: UserMsg)(implicit log: Log): ExitStatus = {
    if(!completion){
      if(log.writersCount < 2) { log.attach(LogStyle(stdout, debug = false)) }
      log.fail(msg)
    }
    Abort
  }

  def continuation(script: String): ExitStatus = {
    val scriptFile = Installation.scriptsDir.extant() / str"exec_${pid.pid}"
    val pw = new java.io.PrintWriter(scriptFile.javaFile)
    pw.write(script)
    pw.write("\n")
    pw.close()
    Log.log(Pid(0)).info(msg"Exporting temporary script file to ${scriptFile}")
    Continuation
  }

  def hint[T: StringShow: Descriptor]
          (arg: CliParam, hints: Traversable[T])
          : Try[Cli { type Hinted <: cli.Hinted with arg.type }] = {
    val newHints = Cli.OptCompletion(arg, implicitly[Descriptor[T]].wrap(implicitly[StringShow[T]], hints))

    Success(Cli(stdout, args, command, newHints :: optCompletions, env, pid)) 
  }

  def -<(arg: CliParam)
        (implicit hinter: arg.Hinter, stringShow: StringShow[arg.Type], descriptor: Descriptor[arg.Type])
        : Cli { type Hinted <: cli.Hinted with arg.type } = {
    val newHints = Cli.OptCompletion(arg, descriptor.wrap(stringShow, hinter.hints))
    Cli(stdout, args, command, newHints :: optCompletions, env, pid)
  }

  def hint(arg: CliParam) =
    Success(Cli(stdout, args, command, Cli.OptCompletion(arg, "()"):: optCompletions, env, pid)) 

  private[this] def write(msg: UserMsg): Unit = {
    stdout.println(msg.string(ManagedConfig().theme))
    stdout.flush()
  }

  def completeCommand(cmd: MenuStructure, hasLayer: Boolean): Try[Nothing] =
    command.map { no =>
      val name = if(no == 1) "Command" else "Subcommand"
      val optCompletions = List(Cli.CmdCompletion(no - 1, name, cmd match {
        case act: Action => Nil
        case menu: Menu  => menu.items.filter(_.show).filter(!_.needsLayer || hasLayer).to[List]
      }))
      stdout.println(optCompletions.flatMap(_.output).mkString("\n"))
      stdout.flush()
      Failure(EarlyCompletions())
    }.getOrElse {
      args.prefix.headOption.fold(Failure(UnknownCommand(""))) { arg => Failure(UnknownCommand(arg.value)) }
    }

}

case class Completions(completions: List[Cli.OptCompletion] = Nil) {
  def hint[T: StringShow: Descriptor](arg: CliParam, hints: Traversable[T]): Completions = {
    val newHints = Cli.OptCompletion(arg, implicitly[Descriptor[T]].wrap(implicitly[StringShow[T]], hints))

    copy(completions = newHints :: completions)
  }

  def hint(arg: CliParam): Completions = copy(completions = Cli.OptCompletion(arg, "()") :: completions)
}
