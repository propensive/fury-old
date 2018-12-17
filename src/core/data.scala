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

import mitigation._
import guillotine._
import gastronomy._, ByteEncoder.base64Url
import kaleidoscope._
import exoskeleton.{InvalidArgValue, MissingArg}

import scala.collection.mutable.HashMap
import scala.collection.immutable.{SortedSet, TreeSet}
import scala.concurrent._, ExecutionContext.Implicits.global

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

case class Compilation(graph: Map[ModuleRef, List[ModuleRef]],
                       checkouts: Set[Checkout],
                       artifacts: Map[ModuleRef, Artifact]) {

  def apply(ref: ModuleRef): Result[Artifact, ~ | ItemNotFound] =
    artifacts.get(ref).ascribe(ItemNotFound(ref.moduleId))

  def checkoutAll()(implicit layout: Layout, shell: Shell): Unit =
    checkouts.foreach(_.get.unit)

  def generateFiles(universe: Universe)
                   (implicit layout: Layout, env: Environment, shell: Shell)
                   : Result[Iterable[Path], ~ | ItemNotFound | InvalidValue | ProjectConflict | ShellFailure | UnknownCompiler | FileWriteError | FileNotFound] =
    Bloop.generateFiles(artifacts.values, universe)
}

/** A Universe represents a the fully-resolved set of projects available in the layer */
case class Universe(projects: Map[ProjectId, Project] = Map(),
                    schemas: Map[ProjectId, Schema] = Map()) {
  def ids: Set[ProjectId] = projects.keySet
  
  def project(id: ProjectId): Result[Project, ~ | ItemNotFound] =
    projects.get(id).ascribe(ItemNotFound(id))
  
  def schema(id: ProjectId): Result[Schema, ~ | ItemNotFound] =
    schemas.get(id).ascribe(ItemNotFound(id))
  
  def artifact(ref: ModuleRef)(implicit shell: Shell): Result[Artifact, ~ | ShellFailure | ItemNotFound] = for {
    project   <- project(ref.projectId)
    schema    <- schema(ref.projectId)
    module    <- project(ref.moduleId)
    compiler  <- if(module.compiler == ModuleRef.JavaRef) Answer(None) else artifact(module.compiler).map(Some(_))
    binaries  <- module.binaries.map(_.paths).sequence.map(_.flatten)
    checkouts <- checkout(ref)
  } yield Artifact(ref,
                   module.kind,
                   module.main,
                   schema.repos.map(_.repo).to[List],
                   checkouts.to[List],
                   binaries.to[List],
                   module.after.to[List],
                   compiler,
                   module.bloopSpec,
                   module.params.map(_.name).to[List],
                   ref.intransitive)

  def checkout(ref: ModuleRef): Result[Set[Checkout], ~ | ItemNotFound] = for {
    project   <- project(ref.projectId)
    schema    <- schema(ref.projectId)
    module    <- project(ref.moduleId)
    repos     <- module.sources.groupBy(_.repoId).map { case (k, v) => schema.repo(k).map(_ -> v) }.sequence
  } yield repos.map { case (repo, paths) =>
    Checkout(repo.repo, repo.refSpec, paths.map(_.path).to[List])
  }.to[Set]

  def ++(that: Universe): Universe = Universe(projects ++ that.projects, schemas ++ that.schemas)
  
  def classpath(ref: ModuleRef)
               (implicit layout: Layout, shell: Shell)
               : Result[Set[Path], ~ | ShellFailure | ItemNotFound] = for {
    art  <- artifact(ref)
    deps <- transitiveDependencies(ref)
    dirs <- ~deps.map(layout.classesDir(_))
    bins <- ~deps.flatMap(_.binaries)
  } yield (dirs ++ bins ++ art.binaries)

  def dependencies(ref: ModuleRef)
                  (implicit shell: Shell)
                  : Result[Set[Artifact], ~ | ShellFailure | ItemNotFound] = for {
    project <- project(ref.projectId)
    module  <- project(ref.moduleId)
    deps    <- module.after.map(artifact).sequence
  } yield deps

  def transitiveDependencies(ref: ModuleRef)
                            (implicit shell: Shell)
                            : Result[Set[Artifact], ~ | ShellFailure | ItemNotFound] = for {
    after      <- dependencies(ref)
    tDeps      <- after.map(_.ref).map(transitiveDependencies).sequence
    itDeps      = tDeps.flatten.filterNot(_.intransitive).to[Set]
  } yield after ++ itDeps
  
  def clean(ref: ModuleRef)(implicit layout: Layout, shell: Shell): Unit =
    layout.classesDir.delete().unit

  def allParams(ref: ModuleRef)
               (implicit layout: Layout, shell: Shell)
               : Result[List[String], ~ | ShellFailure | ItemNotFound] = for {
    tDeps        <- transitiveDependencies(ref)
    plugins      <- ~tDeps.filter(_.kind == Plugin)
    artifact     <- artifact(ref)
  } yield artifact.params ++ plugins.map { plugin => Parameter(str"Xplugin:${layout.classesDir(plugin)}") }.map(_.parameter)

  def compilation(ref: ModuleRef)
                 (implicit shell: Shell, layout: Layout)
                 : Result[Compilation, ~ | ItemNotFound | ShellFailure] = for {
    art       <- artifact(ref)
    graph     <- transitiveDependencies(ref).map(_.map { a => (a.ref, a.dependencies) }.toMap.updated(art.ref, art.dependencies))
    artifacts <- graph.keys.map { key => artifact(key).map(key -> _) }.sequence.map(_.toMap)
    checkouts <- graph.keys.map { key => checkout(key) }.sequence
  } yield Compilation(graph, checkouts.foldLeft(Set[Checkout]())(_ ++ _), artifacts)

  def saveJars(cli: Cli[_])(io: cli.Io, ref: ModuleRef, dest: Path)
              (implicit shell: Shell, layout: Layout)
              : Result[Unit, ~ | FileWriteError | ItemNotFound | ShellFailure] = for {
    dest         <- dest.directory
    current      <- artifact(ref)
    deps         <- transitiveDependencies(ref)
    dirs         <- ~deps.map(layout.classesDir(_))
    files        <- ~dirs.map { dir => (dir, dir.children) }.filter(_._2.nonEmpty)
    bins         <- ~deps.flatMap(_.binaries)
    _            <- ~io.println(msg"Writing manifest file ${layout.manifestFile(current)}")
    manifestFile <- Manifest.file(layout.manifestFile(current), bins.map(_.name), None)
    path         <- ~(dest / str"${ref.projectId.key}-${ref.moduleId.key}.jar")
    _            <- ~io.println(msg"Saving JAR file $path")
    _            <- shell.aggregatedJar(path, files, manifestFile)
    _            <- ~bins.foreach { b => b.copyTo(dest / b.name) }
  } yield ()

  def compile(artifact: Artifact,
              multiplexer: Multiplexer[ModuleRef, CompileEvent],
              futures: Map[ModuleRef, Future[CompileResult]] = Map())
             (implicit layout: Layout, shell: Shell)
             : Map[ModuleRef, Future[CompileResult]] = {

    // FIXME: don't .get
    val deps = dependencies(artifact.ref).opt.get
    
    val newFutures = deps.foldLeft(futures) { (futures, dep) =>
      if(futures.contains(dep.ref)) futures
      else compile(dep, multiplexer, futures)
    }

    val dependencyFutures = Future.sequence(deps.map(_.ref).map(newFutures))

    val future = dependencyFutures.flatMap { inputs =>
      if(inputs.exists(!_.success)) {
        multiplexer(artifact.ref) = SkipCompile(artifact.ref)
        multiplexer.close(artifact.ref)
        Future.successful(CompileResult(false, ""))
      } else Future {
        val out = new StringBuilder()
        multiplexer(artifact.ref) = StartCompile(artifact.ref)
        
        val result: Boolean = blocking {
          shell.bloop.compile(artifact.hash.encoded, artifact.kind == Application) { ln =>
            out.append(ln)
            out.append("\n")
          }.await() == 0
        }

        multiplexer(artifact.ref) = StopCompile(artifact.ref, out.toString, result)
        multiplexer.close(artifact.ref)
        CompileResult(result, out.toString)
      }
    }

    newFutures.updated(artifact.ref, future)
  }

}

case class SchemaTree(schema: Schema, inherited: Set[SchemaTree]) {

  lazy val universe: Result[Universe, ~ | ProjectConflict] = {
    val localProjectIds = schema.projects.map(_.id)
    val empty: Result[Universe, ~ | ProjectConflict] = Answer(Universe())
    inherited.foldLeft(empty) { (projects, schemaTree) =>
      projects.flatMap { projects =>
        schemaTree.universe.flatMap { nextProjects =>
          val conflictIds = (projects.ids -- localProjectIds).intersect(nextProjects.ids)
          if(conflictIds.isEmpty) Answer(projects ++ nextProjects)
          else Result.abort(ProjectConflict(conflictIds))
        }
      }
    }.map { old =>
      val newProjects = schema.projects.map { p => p.id -> p }.toMap
      val newSchemas = schema.projects.map(_.id -> schema).toMap
      old ++ Universe(newProjects, newSchemas)
    }
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
  
  def repo(repoId: RepoId): Result[SourceRepo, ~ | ItemNotFound] = repos.findBy(repoId)

  def moduleRefs: SortedSet[ModuleRef] = projects.flatMap(_.moduleRefs)
  
  def compilerRefs(implicit layout: Layout, shell: Shell): List[ModuleRef] =
    allProjects.opt.to[List].flatMap(_.flatMap(_.compilerRefs))

  def mainProject: Result[Option[Project], ~ | ItemNotFound] =
    main.map(projects.findBy(_)).to[List].sequence.map(_.headOption)

  def importCandidates(implicit layout: Layout, shell: Shell): List[String] =
    repos.to[List].flatMap(_.importCandidates(this).opt.to[List].flatten)

  def moduleRefStrings(implicit layout: Layout, shell: Shell): List[String] =
    knownImportedSchemas.opt.to[List].flatMap(_.flatMap(_.moduleRefStrings)) ++
        moduleRefs.to[List].map { ref => str"$ref" }

  def schemaTree()(implicit layout: Layout, shell: Shell)
                        : Result[SchemaTree, ~ | ItemNotFound | FileWriteError | ShellFailure |
                            FileNotFound | ConfigFormatError | InvalidValue] = for {
    imports   <- importedSchemas
    inherited <- imports.map(_.schemaTree).sequence
  } yield SchemaTree(this, inherited.to[Set])

  def importedSchemas(implicit layout: Layout, shell: Shell)
                     : Result[List[Schema], ~ | ItemNotFound | FileWriteError | ShellFailure |
                         FileNotFound | ConfigFormatError | InvalidValue] =
    imports.map { ref =>
      for {
        repo      <- repos.findBy(ref.repo)
        dir       <- repo.fullCheckout.get
        layer     <- Ogdl.read[Layer](Layout(layout.home, dir).furyConfig)
        resolved  <- layer.schemas.findBy(ref.schema)
      } yield resolved
    }.sequence

  def knownImportedSchemas(implicit layout: Layout, shell: Shell)
                          : Result[List[Schema], ~ | ItemNotFound | FileWriteError | ShellFailure |
                              FileNotFound | ConfigFormatError | InvalidValue] =
    imports.map { ref =>
      for {
        repo      <- repos.findBy(ref.repo)
        dir       <- ~repo.fullCheckout.path
        layer     <- Ogdl.read[Layer](Layout(layout.home, dir).furyConfig)
        resolved  <- layer.schemas.findBy(ref.schema)
      } yield resolved
    }.sequence

  def sourceRepoIds: SortedSet[RepoId] = repos.map(_.id) + RepoId.Local

  def allProjects(implicit layout: Layout, shell: Shell)
                 : Result[List[Project], ~ | ItemNotFound | FileWriteError | ShellFailure |
                     FileNotFound | ConfigFormatError | InvalidValue] =
    knownImportedSchemas.flatMap(_.map(_.allProjects).sequence.map(_.flatten)).map(_ ++ projects.to[List])

  def allRepos(implicit layout: Layout, shell: Shell): Result[Set[SourceRepo], ~ | ShellFailure] = {
    val remote = shell.git.getRemote(layout.pwd).opt.getOrElse("")
    
    for {
      commit <- shell.git.getCommit(layout.pwd)
    } yield repos + SourceRepo(RepoId.Local, Repo(remote), RefSpec(commit), Some(layout.pwd))
  }

  def unused(projectId: ProjectId) = projects.find(_.id == projectId) match {
    case None => Answer(projectId)
    case Some(m) => Result.abort(ProjectAlreadyExists(m.id))
  }
  
  def duplicate(id: String) = copy(id = SchemaId(id))
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

case class Layer(schemas: SortedSet[Schema],
                 main: SchemaId,
                 aliases: SortedSet[Alias] = TreeSet()) { layer =>
  
  def mainSchema: Result[Schema, ~ | ItemNotFound] = schemas.findBy(main)

  def showSchema: Boolean = schemas.size > 1

  def apply(schemaId: SchemaId): Result[Schema, ~ | ItemNotFound] =
    schemas.find(_.id == schemaId).ascribe(ItemNotFound(schemaId))

  def projects: Result[SortedSet[Project], ~ | ItemNotFound] = mainSchema.map(_.projects)
}

object Layer {
  def empty() = Layer(SortedSet(Schema(SchemaId.default)), SchemaId.default)

  def read(file: Path)
          (implicit layout: Layout)
          : Result[Layer, ~ | FileNotFound | MissingArg | InvalidArgValue | ConfigFormatError |
              FileWriteError | AlreadyInitialized] =
    Ogdl.read[Layer](file).abide(Layer.empty())
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

  override def toString: String = str"$projectId/$moduleId"
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

case class Checkout(repo: Repo, refSpec: RefSpec, sources: List[Path]) {

  def hash: Digest = this.digest[Md5]
  def path(implicit layout: Layout): Path = layout.srcsDir / hash.encoded
 
  def get(implicit shell: Shell, layout: Layout)
         : Result[Path, ~ | ItemNotFound | ShellFailure | FileWriteError | InvalidValue] = for {
    repoDir    <- repo.fetch
    workingDir <- checkout
  } yield workingDir
                                                 

  private def checkout(implicit shell: Shell, layout: Layout)
                      : Result[Path, ~ | ItemNotFound | ShellFailure | FileWriteError | InvalidValue] =
    if(!path.exists) {
      println(s"Checking out sources ${if(sources.isEmpty) "" else sources.map(_.value).mkString("", ", ", " ")}in refspec ${refSpec.id}.")
      path.mkdir()
      shell.git.sparseCheckout(repo.path, path, sources, refSpec.id).map { _ => path }
    } else {
      println(s"Using existing checkout of sources in ${path.value}")
      Answer(path)
    }
}

object SourceRepo {
  implicit val msgShow: MsgShow[SourceRepo] = r => UserMsg(_.repo(r.id.key))
  implicit val stringShow: StringShow[SourceRepo] = _.id.key
  implicit def diff: Diff[SourceRepo] = Diff.gen[SourceRepo]
}

case class SourceRepo(id: RepoId, repo: Repo, refSpec: RefSpec, local: Option[Path]) {

  def listFiles(implicit layout: Layout, shell: Shell) = for {
    dir   <- local.map(Answer(_)).getOrElse(repo.fetch)
    files <- shell.git.lsTree(dir, refSpec.id)
  } yield files

  def fullCheckout: Checkout = Checkout(repo, refSpec, List())

  def importCandidates(schema: Schema)
                      (implicit layout: Layout, shell: Shell)
                      : Result[List[String], ~ | ConfigFormatError | ItemNotFound | FileNotFound |
                          InvalidValue | FileWriteError | ShellFailure] = for {
    dir       <- ~fullCheckout.path
    layer     <- Ogdl.read[Layer](Layout(layout.home, dir).furyConfig)
    schemas   <- ~layer.schemas.to[List]
  } yield schemas.map { schema => str"${id.key}:${schema.id.key}" }

  def current(implicit shell: Shell, layout: Layout)
             : Result[RefSpec, ~ | FileWriteError | ShellFailure | InvalidValue] = for {
    dir    <- local.map(Answer(_)).getOrElse(repo.fetch)
    commit <- shell.git.getCommit(dir)
  } yield RefSpec(commit)

  def sourceCandidates(pred: String => Boolean)
                      (implicit layout: Layout, shell: Shell)
                      : Result[Set[Source], ~ | ShellFailure | FileWriteError | InvalidValue] =
    listFiles.map { files => files.filter { f => pred(f.filename) }.map { p => Source(id, p.parent) }.to[Set] }
}

case class BinRepoId(id: String)

object BinRepoId {
  implicit val msgShow: MsgShow[BinRepoId] = v => UserMsg(_.repo(v.id))
  implicit val stringShow: StringShow[BinRepoId] = _.id
  final val Central: BinRepoId = BinRepoId("central")
}

case class Repo(url: String) {
  def hash: Digest = url.digest[Md5]
  def path(implicit layout: Layout): Path = layout.reposDir / hash.encoded

  def update()
            (implicit shell: Shell, layout: Layout)
            : Result[UserMsg, ~ | FileWriteError | EarlyCompletions | ShellFailure | InvalidValue] = for {
    oldCommit <- shell.git.getCommit(path)
    _         <- shell.git.pull(path, None)
    newCommit <- shell.git.getCommit(path)
    msg       <- ~(if(oldCommit != newCommit) msg"Repository ${url} updated to new commit $newCommit"
                   else msg"Repository ${url} is unchanged")
  } yield msg

  def fetch(implicit layout: Layout, shell: Shell)
           : Result[Path, ~ | ShellFailure | InvalidValue | FileWriteError] = {
    if(!path.exists) {
      println(s"Fetching Git repository $url.")
      path.mkdir()
      shell.git.cloneBare(url, path).map { _ => path }
    } else Answer(path)
  }

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

sealed trait CompileEvent
case object Tick extends CompileEvent
case class StartCompile(ref: ModuleRef) extends CompileEvent
case class StopCompile(ref: ModuleRef, output: String, success: Boolean) extends CompileEvent
case class SkipCompile(ref: ModuleRef) extends CompileEvent

case class CompileResult(success: Boolean, output: String)

case class Artifact(ref: ModuleRef,
                    kind: Kind,
                    main: Option[String],
                    repos: List[Repo],
                    checkouts: List[Checkout],
                    binaries: List[Path],
                    dependencies: List[ModuleRef],
                    compiler: Option[Artifact],
                    bloopSpec: Option[BloopSpec],
                    params: List[String],
                    intransitive: Boolean) {

  def hash: Digest =
    (kind, main, checkouts, binaries, dependencies, compiler, params).digest[Sha256]
  
  def writePlugin()(implicit layout: Layout): Unit = if(kind == Plugin) {
    val file = layout.classesDir(this) / "scalac-plugin.xml"
    
    main.foreach { main =>
      file.writeSync(str"<plugin><name>${ref.moduleId.key}</name><classname>${main}</classname></plugin>")
    }
  }

  def sourcePaths(implicit layout: Layout): List[Path] =
    checkouts.flatMap { c => c.sources.map(_ in c.path) }

}

object Project {
  implicit val msgShow: MsgShow[Project] = v => UserMsg(_.project(v.id.key))
  implicit val stringShow: StringShow[Project] = _.id.key
  implicit def diff: Diff[Project] = Diff.gen[Project]
  
  def available(projectId: ProjectId, layer: Layer): Boolean =
    !layer.projects.opt.to[List].flatten.findBy(projectId).successful
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

  def apply(module: ModuleId): Result[Module, ~ | ItemNotFound] = modules.findBy(module)
}

object License {
  implicit val msgShow: MsgShow[License] = v => UserMsg(_.license(v.id.key))
  implicit val stringShow: StringShow[License] = _.id.key

  val unknown = LicenseId("unknown")
  val standardLicenses = List(
    License(LicenseId("afl-3.0"), "Academic Free License v3.0"),
    License(LicenseId("apache-2.0"), "Apache license 2.0"),
    License(LicenseId("artistic-2.0"), "Artistic license 2.0"),
    License(LicenseId("bsd-2-clause"), "BSD 2-clause \"Simplified\" license"),
    License(LicenseId("bsd-3-clause"), "BSD 3-clause \"New\" or \"Revised\" license"),
    License(LicenseId("bsl-1.0"), "Boost Software License 1.0"),
    License(LicenseId("bsd-3-clause-clear"), "BSD 3-clause Clear license"),
    License(LicenseId("cc"), "Creative Commons license family"),
    License(LicenseId("cc0-1.0"), "Creative Commons Zero v1.0 Universal"),
    License(LicenseId("cc-by-4.0"), "Creative Commons Attribution 4.0"),
    License(LicenseId("cc-by-sa-4.0"), "Creative Commons Attribution Share Alike 4.0"),
    License(LicenseId("wtfpl"), "Do What The F*ck You Want To Public License"),
    License(LicenseId("ecl-2.0"), "Educational Community License v2.0"),
    License(LicenseId("epl-1.0"), "Eclipse Public License 1.0"),
    License(LicenseId("epl-1.1"), "European Union Public License 1.1"),
    License(LicenseId("agpl-3.0"), "GNU Affero General Public License v3.0"),
    License(LicenseId("gpl"), "GNU General Public License family"),
    License(LicenseId("gpl-2.0"), "GNU General Public License v2.0"),
    License(LicenseId("gpl-3.0"), "GNU General Public License v3.0"),
    License(LicenseId("lgpl"), "GNU Lesser General Public License family"),
    License(LicenseId("lgpl-2.1"), "GNU Lesser General Public License v2.1"),
    License(LicenseId("lgpl-3.0"), "GNU Lesser General Public License v3.0"),
    License(LicenseId("isc"), "ISC"),
    License(LicenseId("lppl-1.3c"), "LaTeX Project Public License v1.3c"),
    License(LicenseId("ms-pl"), "Microsoft Public License"),
    License(LicenseId("mit"), "MIT"),
    License(LicenseId("mpl-2.0"), "Mozilla Public License 2.0"),
    License(LicenseId("osl-3.0"), "Open Software License 3.0"),
    License(LicenseId("postgresql"), "PostgreSQL License"),
    License(LicenseId("ofl-1.1"), "SIL Open Font License 1.1"),
    License(LicenseId("ncsa"), "University of Illinois/NCSA Open Source License"),
    License(LicenseId("unlicense"), "The Unlicense"),
    License(LicenseId("zlib"), "zLib License"),
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

  def hash(schema: Schema): Result[Digest, ~ | ItemNotFound] =
    schema.repo(repoId).map((path, _).digest[Sha256])
 
  def path(schema: Schema)(implicit layout: Layout): Result[Path, ~ | ItemNotFound] =
    hash(schema).map(layout.srcsDir / _.encoded)
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
