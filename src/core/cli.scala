/*

    Fury, version 0.17.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

import fury.text._, fury.io._, fury.model._

import exoskeleton._
import guillotine._
import escritoire._
import mercator._

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

  def atMostOne(param1: CliParam, param2: CliParam)
                (implicit ev: Hinted <:< param1.type with param2.type)
                : Try[Option[Either[param1.Type, param2.Type]]] = {
    val left = args.get(param1.param).toOption
    val right = args.get(param2.param).toOption
    if(left.isDefined && right.isDefined) Failure(BadParams(param1, param2))
    else Success(left.map(Left(_)).orElse(right.map(Right(_))))
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

  def action(blk: => Try[ExitStatus])(implicit log: Log): Try[ExitStatus] = call().flatMap { _ => blk }

  def peek(param: CliParam): Option[param.Type] = args.get(param.param).toOption

  def get(param: CliParam): Try[param.Type] = args.get(param.param).toOption.ascribe(MissingParam(param))

  def preview(param: CliParam)(default: Option[param.Type] = None): Try[param.Type] = {
    val result = args.get(param.param)
    if(default.isDefined) result.recover { case _: exoskeleton.MissingArg => default.get } else result
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

  def hint(arg: CliParam): Success[Cli { type Hinted <: cli.Hinted with arg.type }] =
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

abstract class CliApi {
  import Args._

  implicit def log: Log
  def cli: Cli
  def get(arg: CliParam): Try[arg.Type] = cli.get(arg)
  def opt(arg: CliParam): Option[arg.Type] = cli.get(arg).toOption
  def has(arg: CliParam): Boolean = cli.get(arg).toOption.isDefined
  
  lazy val getLayout: Try[Layout] = cli.layout
  lazy val conf: Try[FuryConf] = getLayout >>= Layer.readFuryConf
  lazy val getLayer: Try[Layer] = conf >>= Layer.retrieve
  lazy val layerProjectOpt: Try[Option[Project]] = getLayer >>= (_.mainProject)
  lazy val layerProject: Try[Project] = layerProjectOpt.flatMap(_.ascribe(MissingParam(ProjectArg)))
  lazy val layerProjectIds: List[ProjectId] = (getLayer >> (_.projects.to[List])).getOrElse(List()).map(_.id)
  lazy val cliProject: Try[Project] = (getLayer, get(ProjectArg)) >>= (_.projects.findBy(_))
  lazy val getProject: Try[Project] = cliProject.orElse(layerProject)
  lazy val layerRepoIdOpt: Try[Option[RepoId]] = (getLayer >> (_.mainRepo))
  lazy val hierarchy: Try[Hierarchy] = getLayer >>= (_.hierarchy()(log))
  lazy val universe: Try[Universe] = hierarchy >>= (_.universe)

  def layerRepo(layer: Layer, repoIdOpt: Option[RepoId]): Try[Option[Repo]] =
    repoIdOpt.traverse(layer.repos.findBy(_))

  lazy val layerRepoOpt: Try[Option[Repo]] = (getLayer, layerRepoIdOpt) >>= layerRepo
  lazy val layerRepo: Try[Repo] = layerRepoOpt.flatMap(_.ascribe(MissingParam(RepoArg)))
  lazy val cliRepo: Try[Repo] = (getLayer, get(RepoArg)) >>= (_.repos.findBy(_))
  lazy val getRepo: Try[Repo] = cliRepo.orElse(layerRepo)
  lazy val getGitDir: Try[GitDir] = (getRepo, getLayout) >>= (_.remote.fetch(_))
  lazy val remoteGitDir: Try[RemoteGitDir] = getRepo >> (_.remote) >> (RemoteGitDir(cli.env, _))
  lazy val cliModule: Try[Module] = (getProject, get(ModuleArg)) >>= (_.modules.findBy(_))
  lazy val layerModuleOpt: Try[Option[Module]] = getProject >>= (_.mainModule)
  lazy val layerModule: Try[Module] = layerModuleOpt.flatMap(_.ascribe(MissingParam(ModuleArg)))
  lazy val projectModuleIds: List[ModuleId] = (getProject >> (_.modules.to[List])).getOrElse(List()).map(_.id)
  lazy val projectRepoIds: List[RepoId] = (getLayer >> (_.repos.to[List])).getOrElse(List()).map(_.id)
  lazy val getModule: Try[Module] = cliModule.orElse(layerModule)
  lazy val raw: Boolean = cli.get(RawArg).isSuccess
  lazy val column: Option[String] = cli.peek(ColumnArg)
  lazy val branches: Try[List[Branch]] = remoteGitDir >>= (_.branches)
  lazy val tags: Try[List[Tag]] = remoteGitDir >>= (_.tags)
  lazy val absPath: Try[Path] = (get(PathArg), getLayout) >> (_ in _.pwd)
  lazy val branchCommit: Try[Option[Commit]] = (remoteGitDir, get(BranchArg)) >>= (_.findCommit(_))
  lazy val tagCommit: Try[Option[Commit]] = (remoteGitDir, get(TagArg)) >>= (_.findCommit(_))
  lazy val refSpec: Try[Either[Branch, Tag]] = get(BranchArg).map(Left(_)).orElse(get(TagArg).map(Right(_)))
  lazy val newRepoName: Try[RepoId] = get(RemoteArg) >> (_.projectName) >>= (get(RepoNameArg).orElse(_))
  lazy val uniqueRepoName: Try[RepoId] = (getLayer, newRepoName) >>= (_.repos.unique(_))

  lazy val cliCommit: Try[Commit] =
    branchCommit.orElse(tagCommit) >>= (_.ascribe(MissingParamChoice(BranchArg, CommitArg)))
  
  def commit(layer: Layer): Try[LayerRef] = (conf, getLayout) >>= (Layer.commit(layer, _, _))
  def finish[T](value: T): ExitStatus = log.await()

  lazy val gitDestPath: Try[GitDir] = for {
    layout  <- getLayout
    absPath <- absPath
    _       <- ~absPath.mkdir()
    _       <- if(absPath.empty) Success(()) else Failure(new Exception("Non-empty directory exists"))
  } yield GitDir(absPath)(layout.env)

  def cols: Int = Terminal.columns(cli.env).getOrElse(100)

  implicit lazy val rawHints: RawArg.Hinter = RawArg.hint()
  implicit lazy val forceHints: ForceArg.Hinter = ForceArg.hint()
  implicit lazy val projectHints: ProjectArg.Hinter = ProjectArg.hint(layerProjectIds: _*)
  implicit lazy val moduleHints: ModuleArg.Hinter = ModuleArg.hint(projectModuleIds: _*)
  implicit lazy val repoHints: RepoArg.Hinter = RepoArg.hint(projectRepoIds: _*)
  implicit lazy val repoNameHints: RepoNameArg.Hinter = RepoNameArg.hint(layerRepoOpt.map(_.to[List].map(_.id)))
  implicit lazy val pathHints: PathArg.Hinter = PathArg.hint()
  implicit lazy val branchHints: BranchArg.Hinter = BranchArg.hint(branches)
  implicit lazy val tagHints: TagArg.Hinter = TagArg.hint(tags)
  implicit lazy val grabHints: GrabArg.Hinter = GrabArg.hint()
  implicit lazy val resourceHints: ResourceArg.Hinter = ResourceArg.hint()
  implicit lazy val licenseHints: LicenseArg.Hinter = LicenseArg.hint(License.standardLicenses.map(_.id))
  
  implicit lazy val defaultCompilerHints: DefaultCompilerArg.Hinter =
    DefaultCompilerArg.hint((getLayer, getLayout) >> (Javac(8) :: _.compilerRefs(_).map(BspCompiler(_))))
  
  implicit lazy val projectNameHints: ProjectNameArg.Hinter =
    ProjectNameArg.hint(getLayout >> (_.baseDir.name) >> (ProjectId(_)) >> (List(_)))
  
  implicit lazy val repoUrl: RemoteArg.Hinter =
    RemoteArg.hint(GitHub.repos(cli.peek(UnparsedRemoteArg)).map(_.map(Remote(_))))
  
  def printTable[T, S: MsgShow](table: Tabulation[T], rows: Traversable[T], id: Option[S], name: String): Unit =
    log.rawln(Tables().show(table, cols, rows, raw, column, id, name))
}
