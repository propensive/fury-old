/*
  Fury, version 0.1.0. Copyright 2018 Jon Pretty, Propensive Ltd.

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

import mitigation._
import guillotine._
import escritoire._
import eucalyptus._
import gastronomy._
import magnolia._
import kaleidoscope._
import exoskeleton.{InvalidArgValue, MissingArg}

import scala.collection.mutable.HashMap
import scala.collection.immutable.{SortedSet, TreeSet}
import scala.concurrent._, ExecutionContext.Implicits.global

import scala.language.experimental.macros
import scala.language.higherKinds

import java.security.MessageDigest

object ManifestEntry {
  implicit val stringShow: StringShow[ManifestEntry] = _.key
  implicit val msgShow: MsgShow[ManifestEntry] = v => UserMsg { t => v.key }
  
  implicit val diff: Diff[ManifestEntry] =
    (l, r) => Diff.stringDiff.diff(l.pairString, r.pairString)
}

case class ManifestEntry(key: String, value: String) {
  def pairString: String = str"$key=$value"
}

object Kind {
  implicit val msgShow: MsgShow[Kind] = v => UserMsg { t => v.name }
  implicit val stringShow: StringShow[Kind] = _.name
  
  val all: List[Kind] = List(Library, Compiler, Plugin, Application)

  def unapply(str: String): Option[Kind] = all.find(_.name == str)
}

case class Kind(val name: String)
object Library extends Kind("library")
object Compiler extends Kind("compiler")
object Plugin extends Kind("plugin")
object Application extends Kind("application")

object Module {
  implicit val msgShow: MsgShow[Module] = v => UserMsg(_.module(v.id.key))
  implicit val stringShow: StringShow[Module] = _.id.key
  implicit val diff: Diff[Module] = Diff.gen[Module]

  def available(id: ModuleId, project: Project): Result[ModuleId, ~ | ModuleAlreadyExists] =
    project.modules.find(_.id == id).map { module =>
      Result.abort(ModuleAlreadyExists(module.id))
    }.getOrElse(Answer(id))
}

object Binary {
  implicit val msgShow: MsgShow[Binary] = v => UserMsg(_.binary(v.spec))
  implicit val stringShow: StringShow[Binary] = _.spec
  implicit def diff: Diff[Binary] = Diff.gen[Binary]

  def unapply(service: BinRepoId, string: String): Result[Binary, ~ | InvalidArgValue] =
    string match {
      case r"$g@([\.\-_a-zA-Z0-9]*)\:$a@([\.\-_a-zA-Z0-9]*)\:$v@([\.\-_a-zA-Z0-9]*)" =>
        Answer(Binary(service, g, a, v))
      case _ =>
        Result.abort(InvalidArgValue("binary", string))
    }

  private val compilerVersionCache: HashMap[Binary, Result[String, ~ | UnknownCompiler |
      FileNotFound | ItemNotFound | ShellFailure | FileWriteError]] = HashMap()

  private val coursierCache: HashMap[Binary, Result[List[Path], ~ | ShellFailure]] = HashMap()
}

case class Binary(binRepo: BinRepoId, group: String, artifact: String, version: String) {
  def spec = str"$group:$artifact:$version"
  
  def paths(implicit shell: Shell): Result[List[Path], ~ | ShellFailure] =
    Binary.coursierCache.getOrElseUpdate(this, shell.coursier.fetch(spec))

  def detectCompilerVersion(implicit shell: Shell)
                           : Result[String, ~ | UnknownCompiler | FileNotFound | ItemNotFound |
                               ShellFailure | FileWriteError] =
    Binary.compilerVersionCache.getOrElseUpdate(this, (for {
      path <- paths
      entries <- path.map(_.zipfileEntries).sequence
      properties <- ~entries.flatten.filter(_.name == "compiler.properties")
      line <- ~properties.flatMap { p =>
        scala.io.Source.fromInputStream(p.inputStream()).getLines
      }
    } yield line).flatMap(_.collect { case r"version.number=$version@(.*)$$" =>
      version
    }.headOption.ascribe(UnknownCompiler())))
}


case class Module(id: ModuleId,
                  kind: Kind = Library,
                  main: Option[String] = None,
                  manifest: List[ManifestEntry] = List(),
                  compiler: ModuleRef = ModuleRef.JavaRef,
                  after: SortedSet[ModuleRef] = TreeSet(),
                  params: SortedSet[Parameter] = TreeSet(),
                  sources: SortedSet[Source] = TreeSet(),
                  binaries: SortedSet[Binary] = TreeSet(),
                  resources: SortedSet[Path] = TreeSet(),
                  bloopSpec: Option[BloopSpec] = None,
                 ) {
  def ref(project: Project): ModuleRef = ModuleRef(project.id, id)
}

object BloopSpec {
  implicit val msgShow: MsgShow[BloopSpec] = v => msg"${v.org}:${v.name}"
  implicit val stringShow: StringShow[BloopSpec] = bs => s"${bs.org}:${bs.name}"
  implicit def diff: Diff[BloopSpec] = Diff.gen[BloopSpec]

  def parse(str: String): Result[BloopSpec, ~ | InvalidValue] = str match {
    case r"$org@([a-z][a-z0-9_\-\.]*):$id@([a-z][a-z0-9_\-\.]*):$version@([0-9a-z][A-Za-z0-9_\-\.]*)" =>
      Answer(BloopSpec(org, id, version))
    case _ =>
      Result.abort(InvalidValue(str))
  }
}

case class BloopSpec(org: String, name: String, version: String)

case class Projects(values: Map[ProjectId, Project] = Map()) {
  def ids: Set[ProjectId] = values.keySet
  def apply(id: ProjectId): Project = values(id)
  def ++(that: Projects): Projects = Projects(values ++ that.values)
}

case class Schemata(schema: Schema, inherited: Set[Schemata]) {
  def projects: Result[Projects, ~ | ProjectConflict] = {
    val localProjectIds = schema.projects.map(_.id)
    inherited.foldLeft(Answer(Projects()): Result[Projects, ~ | ProjectConflict]) { (projects, schemata) => projects.flatMap { projects =>
      schemata.projects.flatMap { nextProjects =>
        val conflictIds = projects.ids.intersect(nextProjects.ids).filter { id => projects(id) != nextProjects(id) } -- localProjectIds
        if(conflictIds.isEmpty) Answer(projects ++ nextProjects) else Result.abort(ProjectConflict(conflictIds))
      }.map(_ ++ Projects(schema.projects.map { p => (p.id, p) }.toMap))
    } }
  }
}

object Schema {
  implicit val msgShow: MsgShow[Schema] = v => UserMsg(_.schema(v.id.key))
  implicit val stringShow: StringShow[Schema] = _.id.key
  implicit def diff: Diff[Schema] = Diff.gen[Schema]
}

case class Schema(id: SchemaId,
                  projects: SortedSet[Project] = TreeSet(),
                  repos: SortedSet[SourceRepo] = TreeSet(),
                  imports: List[SchemaRef] = List(),
                  main: Option[ProjectId] = None) {
  
  def moduleRefs: SortedSet[ModuleRef] = projects.flatMap(_.moduleRefs)
  
  def compilerRefs(implicit layout: Layout, shell: Shell): List[ModuleRef] =
    allProjects.opt.to[List].flatten.flatMap(_.compilerRefs)

  def mainProject: Result[Option[Project], ~ | ItemNotFound] =
    main.map(projects.findBy(_)).to[List].sequence.map(_.headOption)

  def importCandidates(implicit layout: Layout): List[String] = for {
    repo   <- repos.to[List]
    dir    <- repo.directory.opt.to[List]
    ws     <- Ogdl.read[Workspace](Layout(layout.home, dir).furyConfig).opt.to[List]
    schema <- ws.schemas.to[List]
  } yield s"${repo.id.key}:${schema.id.key}"


  def allDependencies: SortedSet[ModuleRef] = for {
    project <- projects
    module <- project.modules
    dependency <- module.after
  } yield dependency

  def moduleRefStrings(project: Project)(implicit layout: Layout, shell: Shell): List[String] =
    importedSchemas.opt.to[List].flatMap(_.flatMap(_.moduleRefStrings(project))) ++
        moduleRefs.to[List].flatMap { ref =>
      if(ref.projectId == project.id) List(str"$ref", str"${ref.moduleId}")
      else List(str"$ref")
    }

  def schemata(implicit layout: Layout, shell: Shell): Result[Schemata, ~ | ItemNotFound | FileWriteError | ShellFailure | FileNotFound | ConfigFormatError | InvalidValue] =
    for {
      imports <- importedSchemas
      inherited <- imports.map(_.schemata).sequence
    } yield Schemata(this, inherited.to[Set])

  def importedSchemas(implicit layout: Layout, shell: Shell)
                     : Result[List[Schema], ~ | ItemNotFound | FileWriteError | ShellFailure |
                         FileNotFound | ConfigFormatError | InvalidValue] =
    imports.map { ref =>
      for {
        repo <- repos.findBy(ref.repo)
        repoDir <- AsyncRepos.get(repo.repo)//.await()
        workspace <- Ogdl.read[Workspace](Layout(layout.home, repoDir).furyConfig)
        resolved <- workspace.schemas.findBy(ref.schema)
      } yield resolved
    }.sequence

  def sourceRepoIds: SortedSet[RepoId] = repos.map(_.id) + RepoId.Local

  def allProjects(implicit layout: Layout, shell: Shell)
                 : Result[List[Project], ~ | ItemNotFound | FileWriteError | ShellFailure |
                     FileNotFound | ConfigFormatError | InvalidValue] =
    importedSchemas.flatMap(_.map(_.allProjects).sequence.map(_.flatten)).map(_ ++ projects.to[List])

  def allRepos(implicit layout: Layout, shell: Shell): Result[Set[SourceRepo], ~ | ShellFailure] = {
    val remote = shell.git.getRemote(layout.pwd).opt.getOrElse("")
    
    for {
      commit <- shell.git.getCommit(layout.pwd)
    } yield repos + SourceRepo(RepoId.Local, Repo(remote), RefSpec(commit), Some(layout.pwd))
  }

  def artifact(workspace: Workspace, moduleRef: ModuleRef)
              (implicit layout: Layout, shell: Shell)
              : Result[Artifact, ~ | ItemNotFound] =
    for {
      project <- projects.findBy(moduleRef.projectId)
      module <- project.modules.findBy(moduleRef.moduleId)
    } yield Artifact(workspace, this, project, module, moduleRef.intransitive)

  def unused(projectId: ProjectId) = projects.find(_.id == projectId) match {
    case None => Answer(projectId)
    case Some(m) => Result.abort(ProjectAlreadyExists(m.id))
  }
  
  def duplicate(id: String) = copy(id = SchemaId(id))

  def resolve(projectId: ProjectId)
             (implicit layout: Layout, shell: Shell)
             : Option[ResolvedProject] =
    projects.findBy(projectId).opt.map(ResolvedProject(this, _)).orElse {
      (for {
        schemas <- importedSchemas
      } yield for {
        schema <- schemas
        _ <- schema.repos.map(_.repo).map(AsyncRepos.get(_))//.await())
        project <- schema.resolve(projectId)
      } yield project).opt.flatMap(_.headOption)
    }
}

object GitTag {
  def suggested(tags: List[String]): List[String] = {
    implicit def ordering: Ordering[List[Int]] = {
      case (Nil, Nil) => 0
      case (Nil, h2 :: t2) => -1
      case (h1 :: t1, Nil) => 1
      case (h1 :: t1, h2 :: t2) =>
        if(h1 == h2) ordering.compare(t1, t2) else implicitly[Ordering[Int]].compare(h1, h2)
    }

    tags.flatMap {
      case r"$prefix@([a-z]*)$version@(([0-9]*\.)*[0-9][0-9]*)" => List((prefix, version))
      case _ => Nil
    }.groupBy(_._1).to[List].flatMap { case (prefix, version) =>
      val ver = version.map(_._2.split("\\.").to[List].map(_.toInt)).max
      ver.inits.to[List].init.map { init =>
        prefix+(init.init :+ (init.last + 1)).padTo(ver.length, 0).mkString(".")
      }
    }
  }
}

object AliasCmd {
  implicit val msgShow: MsgShow[AliasCmd] = v => UserMsg(_.module(v.key))
  implicit val stringShow: StringShow[AliasCmd] = _.key
}

case class AliasCmd(key: String)

object Alias {
  implicit val msgShow: MsgShow[Alias] = v => UserMsg(_.module(v.cmd.key))
  implicit val stringShow: StringShow[Alias] = _.cmd.key
}

case class Alias(cmd: AliasCmd, description: String, schema: Option[SchemaId], module: ModuleRef)

case class Workspace(schemas: SortedSet[Schema],
                     main: SchemaId,
                     aliases: SortedSet[Alias] = TreeSet()) { workspace =>
  
  def mainSchema: Result[Schema, ~ | ItemNotFound] = schemas.findBy(main)

  def showSchema: Boolean = schemas.size > 1

  def apply(schemaId: SchemaId): Result[Schema, ~ | ItemNotFound] =
    schemas.find(_.id == schemaId).ascribe(ItemNotFound(schemaId))

  def projects: Result[SortedSet[Project], ~ | ItemNotFound] = mainSchema.map(_.projects)
  
  def allProjects(implicit layout: Layout, shell: Shell)
                 : Result[List[Project], ~ | ItemNotFound | FileWriteError | ShellFailure |
                     FileNotFound | ConfigFormatError | InvalidValue] =
    mainSchema.flatMap(_.allProjects)
  
  def moduleRefs(implicit laayout: Layout, shell: Shell)
                : Result[List[ModuleRef], ~ | ItemNotFound | FileWriteError | ShellFailure |
                    FileNotFound | ConfigFormatError | InvalidValue] =
    allProjects.map(_.flatMap(_.moduleRefs))
  
  def compilerRefs(implicit layout: Layout, shell: Shell)
                  : Result[List[ModuleRef], ~ | ItemNotFound | FileWriteError | ShellFailure |
                      ConfigFormatError | FileNotFound | InvalidValue] =
    allProjects.map(_.flatMap(_.compilerRefs))
 
  def moduleRefStrings(project: Project)(implicit laayout: Layout, shell: Shell): List[String] =
    moduleRefs.opt.to[List].flatten.flatMap { ref =>
      if(ref.projectId == project.id) List(str"$ref", str"${ref.moduleId}")
      else List(str"$ref")
    }

  def remoteSources(artifact: Artifact)
             (implicit layout: Layout, shell: Shell)
             : Result[List[(Repo, Set[Path])], ~ | ItemNotFound | FileWriteError | InvalidValue] =
    for {
      sources  <- artifact.module.sources.map(_.remoteSource(artifact.schema)).sequence
    } yield sources.flatten.groupBy(_._1).mapValues(_.map(_._2)).to[List]
  
  def sources(schemaId: SchemaId, moduleRef: ModuleRef)
             (implicit layout: Layout, shell: Shell)
             : Result[List[Path], ~ | ItemNotFound | FileWriteError | InvalidValue] =
    for {
      schema   <- workspace(schemaId)
      resolved <- schema.resolve(moduleRef.projectId).ascribe(
                      ItemNotFound(moduleRef.projectId))
      artifact <- resolved.artifact(this, moduleRef.moduleId)
      paths    <- artifact.module.sources.to[List].map(_.dirPath(schema)).sequence
    } yield paths
}

object Workspace {
  def empty() = Workspace(SortedSet(Schema(SchemaId.default)), SchemaId.default)

  def read(file: Path)
          (implicit layout: Layout)
          : Result[Workspace, ~ | FileNotFound | MissingArg | InvalidArgValue | ConfigFormatError |
              FileWriteError | AlreadyInitialized] =
    Ogdl.read[Workspace](file).abide(fury.Workspace.empty())
}

object ModuleRef {

  implicit val stringShow: StringShow[ModuleRef] = ref => str"${ref.projectId}/${ref.moduleId}"
  implicit val entityName: EntityName[ModuleRef] = EntityName(msg"dependency")
  
  implicit val msgShow: MsgShow[ModuleRef] =
    ref => UserMsg { theme => msg"${theme.project(ref.projectId.key)}${theme.gray("/")}${theme.module(ref.moduleId.key)}".string(theme) }

  val JavaRef = ModuleRef(ProjectId("java"), ModuleId("compiler"), false)
  
  def parse(project: Project, string: String, intransitive: Boolean): Result[ModuleRef, ~ | ItemNotFound] =
    string match {
      case r"$projectId@([a-z][a-z0-9\-]*[a-z0-9])\/$moduleId@([a-z][a-z0-9\-]*[a-z0-9])" =>
        Answer(ModuleRef(ProjectId(projectId), ModuleId(moduleId), intransitive))
      case r"[a-z][a-z0-9\-]*[a-z0-9]" =>
        Answer(ModuleRef(project.id, ModuleId(string), intransitive))
      case _ =>
        Result.abort(ItemNotFound(ModuleId(string)))
    }
}

case class ModuleRef(projectId: ProjectId, moduleId: ModuleId, intransitive: Boolean = false) {
  override def equals(that: Any): Boolean = that match {
    case ModuleRef(p, m, _) => projectId == p && moduleId == m
    case _ => false
  }

  override def hashCode: Int = projectId.hashCode + moduleId.hashCode
}

object SchemaId {
  implicit val msgShow: MsgShow[SchemaId] = v => UserMsg(_.schema(v.key))
  implicit val stringShow: StringShow[SchemaId] = _.key
  
  implicit val diff: Diff[SchemaId] =
    (l, r) => Diff.stringDiff.diff(l.key, r.key)
  
  final val default = SchemaId("default")
  
  def unapply(value: String): Option[SchemaId] = value match {
    case r"[a-z0-9\-\.]*[a-z0-9]$$" =>
      Some(SchemaId(value))
    case _ =>
      None
  }
}

case class SchemaId(key: String) extends Key(msg"schema")

object ProjectId {
  implicit val msgShow: MsgShow[ProjectId] = p => UserMsg(_.project(p.key))
  implicit val stringShow: StringShow[ProjectId] = _.key
  implicit def diff: Diff[ProjectId] = (l, r) => Diff.stringDiff.diff(l.key, r.key)
}

case class ProjectId(key: String) extends Key(msg"project")

object ModuleId {
  implicit val msgShow: MsgShow[ModuleId] = m => UserMsg(_.module(m.key))
  implicit val stringShow: StringShow[ModuleId] = _.key
  implicit def diff: Diff[ModuleId] = (l, r) => Diff.stringDiff.diff(l.key, r.key)
  final val Core: ModuleId = ModuleId("core")
}

case class ModuleId(key: String) extends Key(msg"module")

object Repo {
  implicit val msgShow: MsgShow[Repo] = r => UserMsg(_.url(r.simplified))
  implicit val stringShow: StringShow[Repo] = _.simplified

  def fromString(str: String): Repo = str match {
    case "." => Repo("")
    case r"gh:$group@([A-Za-z0-9_\-\.]+)/$project@([A-Za-z0-9\._\-]+)" =>
      Repo(str"git@github.com:$group/$project.git")
    case r"gl:$group@([A-Za-z0-9_\-\.]+)/$project@([A-Za-z0-9\._\-]+)" =>
      Repo(str"git@gitlab.com:$group/$project.git")
    case r"bb:$group@([A-Za-z0-9_\-\.]+)/$project@([A-Za-z0-9\._\-]+)" =>
      Repo(str"git@bitbucket.com:$group/$project.git")
    case other =>
      Repo(other)
  }
}

object SourceRepo {
  implicit val msgShow: MsgShow[SourceRepo] = r => UserMsg(_.repo(r.id.key))
  implicit val stringShow: StringShow[SourceRepo] = _.id.key
  implicit def diff: Diff[SourceRepo] = Diff.gen[SourceRepo]
}

case class SourceRepo(id: RepoId, repo: Repo, refSpec: RefSpec, local: Option[Path]) {

  def directory(implicit layout: Layout): Result[Path, ~ | FileWriteError | InvalidValue] =
    repo.path.map(_ in layout.refsDir)

  def current(implicit shell: Shell, layout: Layout)
             : Result[RefSpec, ~ | FileWriteError | ShellFailure | InvalidValue] =
    for {
      dir <- directory
      commit <- shell.git.getCommit(dir)
    } yield RefSpec(commit)

  def sources(pred: String => Boolean, projectId: ProjectId)
             (implicit layout: Layout)
             : Result[Set[Source], ~ | FileWriteError | InvalidValue] = for {
    repoDir <- directory
    dirs    <- Answer(repoDir.findSubdirsContaining(pred).map(_.relativize(repoDir)))
    sources <- Answer(dirs.map(Source(id, _)))
  } yield sources

  def moveTo(path: Path)(implicit layout: Layout): Result[SourceRepo, ~ | FileWriteError | InvalidValue] = for {
    repoDir <- directory
    newRepoDir <- repoDir.moveTo(path)
  } yield SourceRepo(id, repo, refSpec, Some(path))

  def update(cli: Cli[_])(implicit layout: Layout): Result[Unit, ~ | FileWriteError | EarlyCompletions | ShellFailure | InvalidValue] = for {
    dir       <- directory
    oldCommit <- cli.shell.git.getCommit(dir)
    _         <- cli.shell.git.pull(dir, Some(refSpec))
    newCommit <- cli.shell.git.getCommit(dir)
    msg       <- ~(if(oldCommit != newCommit) msg"Repository ${id} updated to new commit $newCommit"
                   else msg"Repository ${id} is unchanged")
    //_         <- cli.write(msg)
  } yield ()
}

case class BinRepoId(id: String)

object BinRepoId {
  implicit val msgShow: MsgShow[BinRepoId] = v => UserMsg(_.repo(v.id))
  implicit val stringShow: StringShow[BinRepoId] = _.id
  final val Central: BinRepoId = BinRepoId("central")
}

case class Repo(url: String) {

  def hash: Digest = url.digest[Sha256]

  def simplified: String = url match {
    case r"git@github.com:$group@(.*)/$project@(.*)\.git" => s"gh:$group/$project"
    case r"git@bitbucket.com:$group@(.*)/$project@(.*)\.git" => s"bb:$group/$project"
    case r"git@gitlab.com:$group@(.*)/$project@(.*)\.git" => s"gl:$group/$project"
    case other => other
  }

  def projectName: Result[RepoId, ~ | InvalidValue] = url match {
    case r".*/$project@([^\/]*).git" => Answer(RepoId(project))
    case value => Result.abort(InvalidValue(value))
  }

  def path: Result[Path, ~ | InvalidValue] = url match {
    case r"[a-z0-9]+@$domain@([0-9a-z_\-]*):$path@([A-Za-z0-9\/]*).git" =>
      Answer(Path(s"$domain/$path"))
    case value =>
      Result.abort(InvalidValue(value))
  }
}

object SchemaRef {
  
  implicit val msgShow: MsgShow[SchemaRef] =
    v => UserMsg { theme => msg"${v.repo}${theme.gray("/")}${v.schema}".string(theme) }
  
  implicit val stringShow: StringShow[SchemaRef] = sr => str"${sr.repo}/${sr.schema}"
  implicit def diff: Diff[SchemaRef] = Diff.gen[SchemaRef]
  
  def unapply(value: String): Option[SchemaRef] = value match {
    case r"$repo@([a-z0-9\.\-]*[a-z0-9]):$schema@([a-z0-9\-\.]*[a-z0-9])$$" =>
      Some(SchemaRef(RepoId(repo), SchemaId(schema)))
    case _ =>
      None
  }
}

case class SchemaRef(repo: RepoId, schema: SchemaId)

case class ResolvedProject(schema: Schema, project: Project) {
  def artifact(workspace: Workspace, moduleId: ModuleId): Result[Artifact, ~ | ItemNotFound] =
    project.modules.findBy(moduleId).map { module =>
      Artifact(workspace, schema, project, module, false)
    }
}

sealed trait CompileEvent
case object Tick extends CompileEvent
case class StartCompile(ref: ModuleRef) extends CompileEvent
case class StopCompile(ref: ModuleRef, output: String, success: Boolean) extends CompileEvent
case class SkipCompile(ref: ModuleRef) extends CompileEvent

case class Artifact(workspace: Workspace, schema: Schema, project: Project, module: Module, intransitive: Boolean) {
  lazy val ref: ModuleRef = ModuleRef(project.id, module.id)
  def encoded: String = str"${schema.id}-${project.id}-${module.id}"

  def saveJars(cli: Cli[_])
              (io: cli.Io, dest: Path)
              (implicit shell: Shell, layout: Layout)
              : Result[cli.Io, ~ | FileWriteError | ItemNotFound | ShellFailure] = for {
    dest         <- dest.directory
    deps         <- transitiveDependencies
    dirs         <- ~deps.map(layout.classesDir(_, false))
    files        <- ~dirs.map { dir => (dir, dir.children) }.filter(_._2.nonEmpty)
    bins         <- ~deps.flatMap(_.module.binaries)
    binPaths     <- bins.map(_.paths).sequence
    binFiles     <- ~binPaths.flatten.map { bin => (bin.parent, List(bin.name)) }
    io           <- ~io.println(msg"Writing manifest file ${layout.manifestFile(this)}")
    manifestFile <- Manifest.file(layout.manifestFile(this), binFiles.map(_._2).flatten, None)
    path         <- ~(dest / str"${project.id.key}-${module.id.key}.jar")
    io           <- ~io.println(msg"Saving JAR file $path")
    io           <- ~io.map(shell.aggregatedJar(path, files, manifestFile))
    io           <- ~io.map(binPaths.flatten.map { p => p.copyTo(dest / p.name) })
  } yield io

  def classpath(implicit shell: Shell, layout: Layout)
               : Result[Set[Path], ~ | ShellFailure | ItemNotFound] = for {
    deps <- transitiveDependencies
    dirs <- ~deps.map(layout.classesDir(_, false))
    bins <- deps.flatMap(_.module.binaries).map(_.paths).sequence.map(_.flatten)
  } yield (dirs ++ bins)

  def compile(multiplexer: Multiplexer[ModuleRef, CompileEvent])
             (implicit layout: Layout, shell: Shell)
             : Future[(String, Boolean)] = {
    Future.sequence(dependencies.opt.get.map(_.compile(multiplexer))).flatMap { case inputs: List[(String, Boolean)] =>
      if(!inputs.forall(_._2)) {
        multiplexer(ref) = SkipCompile(ref)
        multiplexer.close(ref)
        Future.successful(("", false))
      } else Future { blocking {
        multiplexer(ref) = StartCompile(ref)
        val out = new StringBuilder()
        val result = if(module.kind == Application) {
          shell.bloop.run(encoded, false) { ln => out.append(ln+"\n").unit }.await()
        } else shell.bloop.compile(encoded, false) { ln => out.append(ln+"\n").unit }.await()
        val success = result == 0
        multiplexer(ref) = StopCompile(ref, out.toString, success)
        multiplexer.close(ref)
        (out.toString, success)
      } }
    }
  }

  def clean(recursive: Boolean)(implicit layout: Layout, shell: Shell): Result[Unit, ~] = Answer {
    if(recursive) dependencies.foreach(_.foreach(_.clean(true)))
    layout.classesDir.delete().unit
  }

  def allParams(implicit layout: Layout, shell: Shell): Result[List[Parameter], ~ | ItemNotFound] = {
    
    val pluginParams = for {
      dependencies <- dependencies
      plugins <- ~transitiveDependencies.opt.to[List].flatten.filter(_.module.kind == Plugin)
    } yield plugins.map { plugin => Parameter(str"Xplugin:${layout.classesDir(plugin, false)}") }

    pluginParams.map(_ ++ module.params)
  }

  def dependencyGraph(implicit layout: Layout, shell: Shell)
                     : Result[Map[ModuleRef, Set[ModuleRef]], ~ | ItemNotFound] =
    transitiveDependencies.map { after =>
      after.map { art => (art.ref, art.module.after) }.toMap
    }

  // FIXME: Handle errors
  def writePlugin()(implicit layout: Layout, fs: FsSession): Unit = if(module.kind == Plugin) {
    val file = layout.classesDir(this, true) / "scalac-plugin.xml"
    
    module.main.foreach { main =>
      file.write(str"<plugin><name>${module.id}</name><classname>${main}</classname></plugin>")
    }
  }

  def classpath()
               (implicit layout: Layout, shell: Shell)
               : Result[List[Path], ~ | ItemNotFound | ShellFailure] = for {
    dependencyArtifacts <- dependencies
    binaryPaths <- module.binaries.map(_.paths).sequence
    dependencyClasspath <- dependencyArtifacts.map(_.classpath()).sequence
    // FIXME: Clean this up
    paths     <- (for {
                   artifact <- dependencyArtifacts
                   binary   <- artifact.module.binaries
                 } yield binary.paths.map(dependencyArtifacts.map(layout.classesDir(_, true)) ::: _)).sequence
  } yield paths.flatten ++ binaryPaths.flatten ++ dependencyArtifacts.map(layout.classesDir(_, true)) ++ dependencyClasspath.flatten

  def compiler(implicit layout: Layout, shell: Shell): Result[Option[Artifact], ~ | ItemNotFound] =
    module.compiler match {
      case ModuleRef.JavaRef => Answer(None)
      case compiler => for {
        resolved <- schema.resolve(compiler.projectId).ascribe(ItemNotFound(compiler.projectId))
        artifact <- resolved.artifact(workspace, compiler.moduleId)
      } yield Some(artifact)
    }

  def dependencies(implicit layout: Layout, shell: Shell)
                  : Result[List[Artifact], ~ | ItemNotFound] =
    module.after.to[List].map { d =>
      // FIXME: Cleanup
      schema.resolve(d.projectId).ascribe(ItemNotFound(d.projectId)).flatMap(_.artifact(workspace, d.moduleId))
    }.sequence

  def transitiveDependencies(implicit layout: Layout, shell: Shell)
                            : Result[Set[Artifact], ~ | ItemNotFound] = for {
        after <- dependencies
        dep <- after.map(_.transitiveDependencies).sequence
        transitive = dep.flatten.filterNot(_.intransitive).to[Set]
      } yield transitive + this
}

object Project {
  implicit val msgShow: MsgShow[Project] = v => UserMsg(_.project(v.id.key))
  implicit val stringShow: StringShow[Project] = _.id.key
  implicit def diff: Diff[Project] = Diff.gen[Project]
  
  def available(projectId: ProjectId, workspace: Workspace): Boolean =
    !workspace.projects.opt.to[List].flatten.findBy(projectId).successful
}

case class Project(id: ProjectId,
                   modules: SortedSet[Module] = TreeSet(),
                   main: Option[ModuleId] = None,
                   license: LicenseId = License.unknown,
                   description: String = "") {
  def moduleRefs: List[ModuleRef] = modules.to[List].map(_.ref(this))
  
  def mainModule: Result[Option[Module], ~ | ItemNotFound] =
    main.map(modules.findBy(_)).to[List].sequence.map(_.headOption)


  def compilerRefs: List[ModuleRef] =
    modules.to[List].collect { case m@Module(_, Compiler, _, _, _, _, _, _, _, _, _) => m.ref(this) }

  def unused(moduleId: ModuleId) =
    modules.findBy(moduleId) match {
      case Answer(_) =>
        Result.abort(ModuleAlreadyExists(moduleId))
      case _ =>
        Answer(moduleId)
    }

  def apply(module: ModuleId): Option[Module] = modules.find(_.id == module)
}

object License {
  implicit val msgShow: MsgShow[License] = v => UserMsg(_.license(v.id.key))
  implicit val stringShow: StringShow[License] = _.id.key

  val unknown = LicenseId("unknown")
  val standardLicenses = List(
    License(LicenseId("apache-2"), "Apache License 2.0"),
    License(LicenseId("bsd3"), "BSD 3-Clause \"New\" or \"Revised\" license"),
    License(LicenseId("bsd2"), "BSD 2-Clause \"Simplified\" or \"FreeBSD\" license"),
    License(LicenseId("gpl"), "GNU General Public License (GPL)"),
    License(LicenseId("lgpl"), "GNU Library or \"Lesser\" General Public License (LGPL)"),
    License(LicenseId("mit"), "MIT license"),
    License(LicenseId("mozilla"), "Mozilla Public License 2.0"),
    License(LicenseId("cddl"), "Common Development and Distribution License"),
    License(LicenseId("eclipse"), "Eclipse Public License")
  )
}

object LicenseId {
  implicit val msgShow: MsgShow[LicenseId] = v => UserMsg(_.license(v.key))
  implicit val stringShow: StringShow[LicenseId] = _.key
}
case class LicenseId(key: String) extends Key(msg"license")

case class License(id: LicenseId, name: String)

object RefSpec {
  implicit val msgShow: MsgShow[RefSpec] = v => UserMsg(_.version(v.id))
  implicit val stringShow: StringShow[RefSpec] = _.id
  
  val master = RefSpec("master")
}

case class RefSpec(id: String)

object Source {
  implicit val stringShow: StringShow[Source] = _.description
  
  implicit val msgShow: MsgShow[Source] = v =>
    UserMsg { theme => msg"${theme.repo(v.repoId.key)}${theme.gray(":")}${theme.path(v.path.value)}".string(theme) }

  def unapply(string: String): Option[Source] = string match {
    case r"$repo@([a-z][a-z0-9\.\-]*[a-z0-9]):$path@(.*)" => Some(Source(RepoId(repo), Path(path)))
    case _ => None
  }
}

case class Source(repoId: RepoId, path: Path) {
  
  def description: String = str"${repoId}:${path.value}"
  
  def remoteSource(schema: Schema)
             : Result[List[(Repo, Path)], ~ | FileWriteError | ItemNotFound | InvalidValue] =
    repoId match {
      case RepoId.Local => Answer(Nil)
      case _ => schema.repos.findBy(repoId).map { r => List((r.repo, path)) }
    }

  def dirPath(schema: Schema)(implicit layout: Layout)
             : Result[Path, ~ | FileWriteError | ItemNotFound | InvalidValue] =
    repoId match {
      case RepoId.Local => Answer(path in layout.pwd)
      case _ => for {
        repo <- schema.repos.findBy(repoId)
        repoDir <- repo.directory
      } yield path.in(repoDir)
    }
}

object RepoId {
  implicit val msgShow: MsgShow[RepoId] = r => UserMsg(_.repo(r.key))
  implicit val stringShow: StringShow[RepoId] = _.key
  
  final val Local: RepoId = RepoId("local")
}  

case class RepoId(key: String) extends Key(msg"repository")

object Parameter {
  implicit val stringShow: StringShow[Parameter] = _.name
  implicit val msgShow: MsgShow[Parameter] = v => UserMsg(_.param(v.name))
}

case class Parameter(name: String) { def parameter = str"-$name" }

abstract class Key(val kind: UserMsg) { def key: String }


class ProjectSpace(val projects: Set[Project]) {
  
}
