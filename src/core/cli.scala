/*

    Fury, version 0.33.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

import fury._, text._, io._, model._

import exoskeleton._
import guillotine._
import escritoire._
import kaleidoscope._
import mercator._
import jovian._

import scala.util._

import language.higherKinds

case class EarlyCompletions() extends FuryException
case class BadParams(param1: CliParam, param2: CliParam) extends FuryException
case class MissingParamChoice(param: CliParam*) extends FuryException
case class MissingParam(param: CliParam) extends FuryException
case class BadParamValue(param: CliParam, value: String) extends FuryException

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
  
  implicit val msgShow: MsgShow[CliParam] = (p: CliParam) => Message { theme =>
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
    def describe(value: T): Message = msg""

    override def wrap(show: StringShow[T], xs: Traversable[T]): String =
      str"(${xs.map(show.show).map(_.replaceAll(":", "\\\\:")).join(" ")})"
  }
}

/** a Descriptor is a typeclass for providing a human-readable description of something */
trait Descriptor[T] {

  def describe(value: T): Message

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

  def assign(params: CliParam*) = Cli(stdout, args.assign(params.map(_.longName.name): _*).flatten, command, optCompletions,
      env, pid)

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

  def atMostOne(param1: CliParam, param2: CliParam): Try[Option[Either[param1.Type, param2.Type]]] = {
    val left = args.get(param1.param).toOption
    val right = args.get(param2.param).toOption
    if(left.isDefined && right.isDefined) Failure(BadParams(param1, param2))
    else Success(left.map(Left(_)).orElse(right.map(Right(_))))
  }
  
  def exactlyOne(param1: CliParam, param2: CliParam): Try[Either[param1.Type, param2.Type]] =
    atMostOne(param1, param2).flatMap(_.ascribe(MissingParamChoice(param1, param2)))
  
  def atLeastOne(param1: CliParam, param2: CliParam): Try[(Option[param1.Type], Option[param2.Type])] = {
    val left = args.get(param1.param).toOption
    val right = args.get(param2.param).toOption
    if(left.isDefined || right.isDefined) Success((left, right))
    else Failure(MissingParamChoice(param1, param2))
  }
  
  def cols: Int = Terminal.columns(env).getOrElse(100)

  private lazy val logStyle: LogStyle = LogStyle(debug = false)

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

  def forceLog(msg: Message): Unit = logStyle.log(msg, System.currentTimeMillis, Log.Warn, pid)

  def action(blk: => Try[ExitStatus])(implicit log: Log): Try[ExitStatus] = call().flatMap { _ => blk }

  def peek(param: CliParam): Option[param.Type] = args.get(param.param).toOption

  def get(param: CliParam): Try[param.Type] = args.get(param.param) match {
    case Success(value)                        => Success(value)
    case Failure(MissingArg(_))                => Failure(MissingParam(param))
    case Failure(InvalidArgValue(value, name)) => Failure(BadParamValue(param, value))
    case _                                     => Failure(BadParamValue(param, ""))
  }

  def preview(param: CliParam)(default: Option[param.Type] = None): Try[param.Type] = {
    val result = args.get(param.param)
    if(default.isDefined) result.recover { case _: exoskeleton.MissingArg => default.get } else result
  }

  def pwd: Try[Path] = env.workDir.ascribe(FileNotFound(path"/")).map(Path(_))

  lazy val newLayout: Try[Layout] = pwd.map { pwd => Layout(Path(env.variables("HOME")), pwd, env, pwd) }

  lazy val layout: Try[Layout] = pwd.flatMap { pwd => Layout.find(Path(env.variables("HOME")), pwd, env) }
  
  def next: Option[String] = args.parsed.prefix.headOption.map(_.value)
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

  def abort(msg: Message)(implicit log: Log): ExitStatus = {
    if(!completion){
      if(log.writersCount < 2) { log.attach(LogStyle(debug = false)) }
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
    Log().info(msg"Including temporary script file to ${scriptFile}")
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

  def -?<(arg: CliParam, condition: Try[Boolean])
        (implicit hinter: arg.Hinter, stringShow: StringShow[arg.Type], descriptor: Descriptor[arg.Type])
  : Cli { type Hinted <: cli.Hinted with arg.type } = condition match {
    case Success(true) => -<(arg)
    case _ => Cli(stdout, args, command, optCompletions, env, pid)
  }

  def hint(arg: CliParam): Success[Cli { type Hinted <: cli.Hinted with arg.type }] =
    Success(Cli(stdout, args, command, Cli.OptCompletion(arg, "()"):: optCompletions, env, pid)) 

  private[this] def write(msg: Message): Unit = {
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
      args.parsed.prefix.headOption.fold(Failure(UnknownCommand(""))) { arg => Failure(UnknownCommand(arg.value)) }
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
  
  def opt(arg: CliParam): Try[Option[arg.Type]] = cli.get(arg) match {
    case Success(value)               => Success(Some(value))
    case Failure(MissingParam(param)) => Success(None)
    case other                        => other.map(Some(_))
  }

  def has(arg: CliParam): Boolean = cli.get(arg).toOption.isDefined
  
  lazy val getLayout: Try[Layout] = cli.layout
  lazy val conf: Try[FuryConf] = getLayout >>= Layer.readFuryConf
  lazy val getLayer: Try[Layer] = (getHierarchy, getPointer) >>= (_(_))
  lazy val getPointer: Try[Pointer] = (opt(LayerArg), confPointer) >> (_.getOrElse(_))
  lazy val confPointer: Try[Pointer] = conf >> (_.path)
  lazy val getBaseLayer: Try[Layer] = conf >> (_.layerRef) >>= (Layer.get(_, None))
  lazy val layerProjectOpt: Try[Option[Project]] = getLayer >>= (_.mainProject)
  lazy val layerProject: Try[Project] = layerProjectOpt.flatMap(_.ascribe(MissingParam(ProjectArg)))
  lazy val layerProjectIds: List[ProjectId] = (getLayer >> (_.projects.to[List])).getOrElse(List()).map(_.id)
  lazy val cliProject: Try[Project] = (getLayer, get(ProjectArg)) >>= (_.projects.findBy(_))
  lazy val getProject: Try[Project] = cliProject.orElse(layerProject)
  lazy val layerRepoIdOpt: Try[Option[RepoId]] = (getLayer >> (_.mainRepo))
  lazy val layerWorkspaceIdOpt: Try[Option[WorkspaceId]] = Success(None)
  lazy val getHierarchy: Try[Hierarchy] = getBaseLayer >>= (_.hierarchy()(log))
  lazy val universe: Try[Universe] = getHierarchy >>= (_.universe)

  def layerRepoOpt(layer: Layer, repoIdOpt: Option[RepoId]): Try[Option[Repo]] =
    repoIdOpt.traverse(layer.repos.findBy(_))

  def layerWorkspaceOpt(layer: Layer, workspaceIdOpt: Option[WorkspaceId]): Try[Option[Workspace]] =
    workspaceIdOpt.traverse(layer.workspaces.findBy(_))

  def layerRepo(layer: Layer, repoId: RepoId): Try[Repo] = layer.repos.findBy(repoId)
  lazy val layerRepoOpt: Try[Option[Repo]] = (getLayer, layerRepoIdOpt) >>= layerRepoOpt
  lazy val layerWorkspaceOpt: Try[Option[Workspace]] = (getLayer, layerWorkspaceIdOpt) >>= layerWorkspaceOpt
  lazy val layerRepo: Try[Repo] = layerRepoOpt.flatMap(_.ascribe(MissingParam(RepoArg)))
  lazy val layerWorkspace: Try[Workspace] = layerWorkspaceOpt.flatMap(_.ascribe(MissingParam(WorkspaceArg)))
  lazy val cliRepo: Try[Repo] = (getLayer, get(RepoArg)) >>= (_.repos.findBy(_))
  
  lazy val cliWorkspace: Try[Option[Workspace]] = (getLayer, get(OptWorkspaceArg)) >>=
      { (l, w) => w.traverse(l.workspaces.findBy(_)) }
  
  lazy val getRepo: Try[Repo] = cliRepo.orElse(layerRepo)
  lazy val getWorkspace: Try[Workspace] = cliWorkspace.flatMap(_.fold(layerWorkspace)(Success(_)))
  lazy val getSpecifiedWorkspace: Try[Workspace] = (getLayer, get(WorkspaceArg)) >>= (_.workspaces.findBy(_))
  lazy val getGitDir: Try[GitDir] = (getRepo, getLayout) >>= (_.remote.fetch(_))
  lazy val fetchRemote: Try[GitDir] = (get(RemoteArg), getLayout) >>= (_.fetch(_))
  lazy val remoteGitDir: Try[RemoteGitDir] = getRepo >> (_.remote) >> (RemoteGitDir(cli.env, _))
  lazy val requiredModule: Try[Module] = (getProject, get(ModuleArg)) >>= (_.modules.findBy(_))
  
  lazy val cliModule: Try[Option[Module]] = (opt(ModuleArg), getProject) >>= {
    case (Some(id), p) => p.modules.findBy(id).map(Some(_))
    case (None, _) => Success(None)
  }
  
  lazy val layerModuleOpt: Try[Option[Module]] = getProject >>= (_.mainModule)
  lazy val layerModule: Try[Module] = layerModuleOpt.flatMap(_.ascribe(MissingParam(ModuleArg)))
  lazy val projectModuleIds: List[ModuleId] = (getProject >> (_.modules.to[List])).getOrElse(List()).map(_.id)
  lazy val projectRepoIds: List[RepoId] = (getLayer >> (_.repos.to[List])).getOrElse(List()).map(_.id)
  lazy val workspaceIds: List[WorkspaceId] = (getLayer >> (_.workspaces.to[List])).getOrElse(List()).map(_.id)
  lazy val getModule: Try[Module] = (cliModule, layerModule) >> (_.getOrElse(_))
  lazy val getModuleRef: Try[ModuleRef] = (getModule, getProject) >> (_.ref(_))
  lazy val getSource: Try[Source] = cli.get(SourceArg)
  lazy val getIncludeType: Try[IncludeType.Id] = cli.get(IncludeTypeArg)

  lazy val getCheckedSource: Try[Source] = cli.get(SourceArg) match {
    case Success(value) => value match {
      case src@RepoSource(repoId, path, glob) =>
        if(projectRepoIds.contains(repoId)) Success(src)
        else if(workspaceIds.contains(repoId.workspace)) Success(WorkspaceSource(repoId.workspace, path))
        else Failure(ItemNotFound(repoId))
      case src =>
        Success(src)
    }
    case failure => failure
  }

  lazy val getInclude: Try[IncludeType] = getIncludeType.flatMap {
    case ClassesDir => getDependency >> (ClassesDir(_))
    case FileRef    => get(SourceArg).map { source => FileRef(source.rootId, source.path) }
    case Jarfile    => getDependency >> (Jarfile(_))
    case JsFile     => getDependency >> (JsFile(_))
    case TarFile    => get(SourceArg).map { source => TarFile(source.rootId.workspace, source.path) }
    case TgzFile    => get(SourceArg).map { source => TgzFile(source.rootId.workspace, source.path) }
  }

  lazy val getDependency: Try[ModuleRef] = (getProject >> (_.id), get(ModuleRefArg)) >>=
      (ModuleRef.parse(_, _, true).ascribe(InvalidValue(get(ModuleRefArg).getOrElse(""))))

  lazy val optDependency: Try[Option[ModuleRef]] = if(has(ModuleRefArg)) getDependency >> (Some(_)) else ~None
  lazy val raw: Boolean = cli.get(RawArg).isSuccess
  lazy val column: Option[String] = cli.peek(ColumnArg)
  lazy val branches: Try[List[Branch]] = remoteGitDir >>= (_.branches)
  lazy val tags: Try[List[Tag]] = remoteGitDir >>= (_.tags)
  lazy val absPath: Try[Path] = (get(PathArg), getLayout) >> (_ in _.pwd)
  
  lazy val relPath: Try[Path] =
    (get(PathArg), getLayout) >> { (path, layout) => path.in(layout.pwd).relativizeTo(layout.baseDir) }
  
  lazy val relPathOpt: Try[Option[Path]] = (opt(PathArg), getLayout) >> { (path, layout) =>
    path.map { p => p.in(layout.pwd).relativizeTo(layout.baseDir) }
  }
  
  lazy val absPathOpt: Try[Option[Path]] =
    (opt(PathArg), getLayout) >> { (path, layout) => path.map { p => p.in(layout.pwd) } }
  
  
  lazy val defaultBranch: Try[Branch] = fetchRemote >>= (_.branch)
  lazy val newBranch: Try[Branch] = get(BranchArg).orElse(defaultBranch)
  lazy val newRepoName: Try[RepoId] = get(RemoteArg) >> (_.projectName) >>= (get(RepoNameArg).orElse(_))
  lazy val newWorkspaceName: Try[WorkspaceId] = get(WorkspaceNameArg)
  lazy val repoNameFromPath: Try[RepoId] = get(PathArg) >> (_.name.toLowerCase) >> (RepoId(_))
  lazy val workspaceNameFromPath: Try[WorkspaceId] = get(PathArg) >> (_.name.toLowerCase) >> (WorkspaceId(_))
  lazy val spaceNameFromPath: Try[RootId] = get(PathArg) >> (_.name.toLowerCase) >> (WorkspaceId(_): RootId)
  lazy val allCommits: Try[List[Commit]] = getGitDir >>= (_.allCommits)

  lazy val universeRepos: Try[Set[RepoSetId]] = universe >> (_.repoSets.keySet)
  lazy val universeLayers: Try[Set[ShortLayerRef]] = universe >> (_.imports.keySet)

  lazy val projectRefs: Try[Set[ProjectRef]] = universe >> (_.projectRefs)

  lazy val findUniqueRepoName: Try[RepoId] =
    (getLayer, newRepoName.orElse(repoNameFromPath)) >>= (_.repos.unique(_))
  
  lazy val findUniqueSpaceName: Try[RootId] =
    (getLayer, newWorkspaceName.orElse(spaceNameFromPath)) >>=
        (_.spaces.unique(_)(RootId.msgShow, Resolver.root))
  
  lazy val defaultBranchCommit: Try[Commit] = fetchRemote.flatMap(_.commit)

  lazy val deepModuleRefs: Try[Set[ModuleRef]] = (universe, getModuleRef) >> (_.deepDependencySearch(_))

  lazy val pathGitDir: Try[GitDir] = for {
    env    <- getLayout >> (_.env)
    path   <- relPathOpt
    gitDir <- (path >> (GitDir(_)(env))).ascribe(PathNotGitDir())
  } yield gitDir

  lazy val pathRemote: Try[Remote] = pathGitDir >>= (_.remote)
  lazy val pathBranch: Try[Branch] = pathGitDir >>= (_.branch)

  def commit(layer: Layer): Try[LayerRef] = (conf, getLayout) >>= (Layer.commit(layer, _, _))
  def commit(hierarchy: Hierarchy): Try[LayerRef] = (confPointer, getLayout) >>= (hierarchy.save(_, _))
  def finish(value: Any): ExitStatus = log.await()
  def cols: Int = Terminal.columns(cli.env).getOrElse(100)

  implicit lazy val rawHints: RawArg.Hinter = RawArg.hint()
  implicit lazy val forceHints: ForceArg.Hinter = ForceArg.hint()
  implicit lazy val projectHints: ProjectArg.Hinter = ProjectArg.hint(layerProjectIds: _*)
  implicit lazy val moduleHints: ModuleArg.Hinter = ModuleArg.hint(projectModuleIds: _*)
  implicit lazy val repoHints: RepoArg.Hinter = RepoArg.hint(projectRepoIds: _*)
  implicit lazy val optWorkspaceHints: OptWorkspaceArg.Hinter = OptWorkspaceArg.hint(workspaceIds.map(Some(_)): _*)
  implicit lazy val workspaceHints: WorkspaceArg.Hinter = WorkspaceArg.hint(workspaceIds: _*)
  implicit lazy val repoNameHints: RepoNameArg.Hinter = RepoNameArg.hint(layerRepoOpt.map(_.to[List].map(_.id)))
  
  implicit lazy val workspaceNameHints: WorkspaceNameArg.Hinter =
    WorkspaceNameArg.hint(layerWorkspaceOpt.map(_.to[List].map(_.id)))
  
  implicit lazy val includeTypeHints: IncludeTypeArg.Hinter =
    IncludeTypeArg.hint(IncludeType.ids)

  def currentDir: Try[List[Path]] = get(PathStringArg).getOrElse("") match {
    //case "/" =>
    //  Try(Path("/").childPaths.flatMap { path => if(path.directory) List(path, path / "") else List(path) })
    case str@r"\/.*" =>
      val path: Path = Path(str)
      Try((if(path.exists && path.directory) path.childPaths else path.parent.childPaths).flatMap { path =>
        if(path.exists && path.directory) List(path, path / "") else List(path)
      })
    case local =>
      getLayout >> { layout =>
        val path = layout.pwd / local
        (if(path.exists && path.directory) path.childPaths else path.parent.childPaths).flatMap { path =>
          val relative = path.relativizeTo(layout.pwd)
          if(path.directory) List(relative, relative / "") else List(relative)
        }
      }
  }

  implicit lazy val pathHints: PathArg.Hinter = PathArg.hint(currentDir)
  implicit lazy val branchHints: BranchArg.Hinter = BranchArg.hint(branches)
  implicit lazy val tagHints: TagArg.Hinter = TagArg.hint(tags)
  implicit lazy val grabHints: GrabArg.Hinter = GrabArg.hint()
  implicit lazy val allHints: AllArg.Hinter = AllArg.hint()
  implicit lazy val importHints: ImportArg.Hinter = ImportArg.hint()
  implicit lazy val resourceHints: ResourceArg.Hinter = ResourceArg.hint()
  implicit lazy val licenseHints: LicenseArg.Hinter = LicenseArg.hint(License.standardLicenses.map(_.id))
  implicit lazy val pointerHints: LayerArg.Hinter = LayerArg.hint(Nil) // FIXME
  implicit lazy val packageHints: PackageArg.Hinter = PackageArg.hint(Nil)
  implicit lazy val commitHints: CommitArg.Hinter = CommitArg.hint(allCommits)
  implicit lazy val repoSetHints: RepoSetArg.Hinter = RepoSetArg.hint(universeRepos)
  implicit lazy val layerRefHints: LayerRefArg.Hinter = LayerRefArg.hint(universeLayers)
  implicit lazy val moduleRefHints: ModuleRefArg.Hinter = ModuleRefArg.hint(deepModuleRefs.map(_.map(_.key)))
  implicit lazy val includeHints: IncludeArg.Hinter = IncludeArg.hint(getModule >> (_.includes.map(_.id)))
  implicit lazy val projectRefHints: ProjectRefArg.Hinter = ProjectRefArg.hint(projectRefs)
  implicit lazy val againstProjectHints: AgainstProjectArg.Hinter = AgainstProjectArg.hint(projectRefs)
  
  implicit lazy val defaultCompilerHints: DefaultCompilerArg.Hinter =
    DefaultCompilerArg.hint((getLayer, getLayout) >> (Javac(8) :: _.compilerRefs(_).map(BspCompiler(_))))
  
  implicit lazy val projectNameHints: ProjectNameArg.Hinter =
    ProjectNameArg.hint(getLayout >> (_.baseDir.name) >> (ProjectId(_)) >> (List(_)))
  
  implicit lazy val repoUrl: RemoteArg.Hinter =
    RemoteArg.hint(GitHub.repos(cli.peek(UnparsedRemoteArg)).map(_.map(Remote(_))))
  
  def printTable[T, S: MsgShow](table: Tabulation[T], rows: Traversable[T], id: Option[S], name: String): Unit =
    log.rawln(Tables().show(table, cols, rows, raw, column, id, name))
  
  def possibleSourceDirectories(repo: Repo, layout: Layout): Set[Source] =
    repo.sourceCandidates(layout)(_ => true).getOrElse(Set.empty)
  
  def oneOf[T](options: T*): T => Boolean = options contains _
}
