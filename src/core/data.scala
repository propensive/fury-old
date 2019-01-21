/*
  Fury, version 0.2.2. Copyright 2019 Jon Pretty, Propensive Ltd.

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

import exoskeleton.{InvalidArgValue, MissingArg}
import fury.io._
import fury.ogdl._
import fury.error._
import gastronomy._
import guillotine._
import mercator._
import kaleidoscope._

import scala.collection.immutable.{SortedSet, TreeSet}
import scala.collection.mutable.HashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.util._

object ManifestEntry {
  implicit val stringShow: StringShow[ManifestEntry] = _.key
  implicit val msgShow: MsgShow[ManifestEntry] = v =>
    UserMsg { t =>
      v.key
    }

  implicit val diff: Diff[ManifestEntry] =
    (l, r) => Diff.stringDiff.diff(l.pairString, r.pairString)
}

case class ManifestEntry(key: String, value: String) {
  def pairString: String = str"$key=$value"
}

object Kind {
  implicit val msgShow: MsgShow[Kind] = v =>
    UserMsg { t =>
      v.name
    }
  implicit val stringShow: StringShow[Kind] = _.name

  val all: List[Kind] = List(Library, Compiler, Plugin, Application)

  def unapply(str: String): Option[Kind] = all.find(_.name == str)
}

case class Kind(val name: String)
object Library     extends Kind("library")
object Compiler    extends Kind("compiler")
object Plugin      extends Kind("plugin")
object Application extends Kind("application")

object Module {
  implicit val msgShow: MsgShow[Module]       = v => UserMsg(_.module(v.id.key))
  implicit val stringShow: StringShow[Module] = _.id.key
  implicit val diff: Diff[Module]             = Diff.gen[Module]

  def available(id: ModuleId, project: Project): Outcome[ModuleId] =
    project.modules
      .find(_.id == id)
      .map { module =>
        Failure(ModuleAlreadyExists(module.id))
      }
      .getOrElse(Success(id))
}

object Binary {
  implicit val msgShow: MsgShow[Binary]       = v => UserMsg(_.binary(v.spec))
  implicit val stringShow: StringShow[Binary] = _.spec
  implicit def diff: Diff[Binary]             = Diff.gen[Binary]

  def unapply(service: BinRepoId, string: String): Outcome[Binary] =
    string match {
      case r"$g@([\.\-_a-zA-Z0-9]*)\:$a@([\.\-_a-zA-Z0-9]*)\:$v@([\.\-_a-zA-Z0-9]*)" =>
        Success(Binary(service, g, a, v))
      case _ =>
        Failure(InvalidArgValue("binary", string))
    }

  private val compilerVersionCache: HashMap[Binary, Outcome[String]] =
    HashMap()

  private val coursierCache: HashMap[Binary, Outcome[List[Path]]] = HashMap()
}

case class Binary(binRepo: BinRepoId, group: String, artifact: String, version: String) {
  def spec = str"$group:$artifact:$version"

  def paths(implicit shell: Shell): Outcome[List[Path]] =
    Binary.coursierCache.getOrElseUpdate(this, shell.coursier.fetch(spec))

  def detectCompilerVersion(implicit shell: Shell): Outcome[String] =
    Binary.compilerVersionCache.getOrElseUpdate(
        this,
        (for {
          path       <- paths
          entries    <- path.map(_.zipfileEntries).sequence
          properties <- ~entries.flatten.filter(_.name == "compiler.properties")
          line <- ~properties.flatMap { p =>
                   scala.io.Source.fromInputStream(p.inputStream()).getLines
                 }
        } yield line).flatMap(_.collect {
          case r"version.number=$version@(.*)$$" =>
            version
        }.headOption.ascribe(UnknownCompiler()))
    )
}

case class Module(
    id: ModuleId,
    kind: Kind = Library,
    main: Option[String] = None,
    plugin: Option[String] = None,
    manifest: List[ManifestEntry] = List(),
    compiler: ModuleRef = ModuleRef.JavaRef,
    after: SortedSet[ModuleRef] = TreeSet(),
    params: SortedSet[Parameter] = TreeSet(),
    sources: SortedSet[Source] = TreeSet(),
    binaries: SortedSet[Binary] = TreeSet(),
    resources: SortedSet[Path] = TreeSet(),
    bloopSpec: Option[BloopSpec] = None) {
  def ref(project: Project): ModuleRef = ModuleRef(project.id, id)

  def externalSources: SortedSet[ExternalSource] = sources.collect {
    case src: ExternalSource => src
  }

  def sharedSources: SortedSet[SharedSource] = sources.collect {
    case src: SharedSource => src
  }

  def localSources: SortedSet[Path] = sources.collect { case src: LocalSource => src.path }
}

object BloopSpec {
  implicit val msgShow: MsgShow[BloopSpec]       = v => msg"${v.org}:${v.name}"
  implicit val stringShow: StringShow[BloopSpec] = bs => s"${bs.org}:${bs.name}"
  implicit def diff: Diff[BloopSpec]             = Diff.gen[BloopSpec]

  def parse(str: String): Outcome[BloopSpec] = str match {
    case r"$org@([a-z][a-z0-9_\-\.]*):$id@([a-z][a-z0-9_\-\.]*):$version@([0-9a-z][A-Za-z0-9_\-\.]*)" =>
      Success(BloopSpec(org, id, version))
    case _ =>
      Failure(InvalidValue(str))
  }
}

case class BloopSpec(org: String, name: String, version: String)

case class Compilation(
    graph: Map[ModuleRef, List[ModuleRef]],
    checkouts: Set[Checkout],
    artifacts: Map[ModuleRef, Artifact]) {

  def apply(ref: ModuleRef): Outcome[Artifact] =
    artifacts.get(ref).ascribe(ItemNotFound(ref.moduleId))

  def checkoutAll()(implicit layout: Layout, shell: Shell): Unit =
    checkouts.foreach(_.get.unit)

  def generateFiles(
      universe: Universe
    )(implicit layout: Layout,
      env: Environment,
      shell: Shell
    ): Outcome[Iterable[Path]] =
    Bloop.generateFiles(artifacts.values, universe)
}

/** A Universe represents a the fully-resolved set of projects available in the layer */
case class Universe(
    projects: Map[ProjectId, Project] = Map(),
    schemas: Map[ProjectId, Schema] = Map(),
    dirs: Map[ProjectId, Path] = Map()) {
  def ids: Set[ProjectId] = projects.keySet

  def project(id: ProjectId): Outcome[Project] =
    projects.get(id).ascribe(ItemNotFound(id))

  def contains(project: Project): Boolean = projects.get(project.id) == Some(project)

  def dir(id: ProjectId): Outcome[Path] =
    dirs.get(id).ascribe(ItemNotFound(id))

  def schema(id: ProjectId): Outcome[Schema] =
    schemas.get(id).ascribe(ItemNotFound(id))

  def artifact(ref: ModuleRef)(implicit layout: Layout, shell: Shell): Outcome[Artifact] =
    for {
      project <- project(ref.projectId)
      schema  <- schema(ref.projectId)
      module  <- project(ref.moduleId)
      dir     <- dir(ref.projectId)
      compiler <- if (module.compiler == ModuleRef.JavaRef) Success(None)
                 else artifact(module.compiler).map(Some(_))
      binaries  <- module.binaries.map(_.paths).sequence.map(_.flatten)
      checkouts <- checkout(ref)
    } yield
      Artifact(
          ref,
          module.kind,
          module.main,
          module.plugin,
          schema.repos.map(_.repo).to[List],
          checkouts.to[List],
          binaries.to[List],
          module.after.to[List],
          compiler,
          module.bloopSpec,
          module.params.to[List],
          ref.intransitive,
          module.localSources.map(_ in dir).to[List],
          module.sharedSources.map(_.path in layout.sharedDir).to[List]
      )

  def checkout(ref: ModuleRef)(implicit layout: Layout, shell: Shell): Outcome[Set[Checkout]] =
    for {
      project <- project(ref.projectId)
      schema  <- schema(ref.projectId)
      module  <- project(ref.moduleId)
      repos <- module.externalSources
                .groupBy(_.repoId)
                .map { case (k, v) => schema.repo(k).map(_ -> v) }
                .sequence
    } yield
      repos.map {
        case (repo, paths) =>
          Checkout(
              repo.repo,
              repo.local.isDefined,
              repo.commit,
              repo.track,
              paths.map(_.path).to[List])
      }.to[Set]

  def ++(that: Universe): Universe =
    Universe(projects ++ that.projects, schemas ++ that.schemas, dirs ++ that.dirs)

  def classpath(ref: ModuleRef)(implicit layout: Layout, shell: Shell): Outcome[Set[Path]] =
    for {
      art  <- artifact(ref)
      deps <- transitiveDependencies(ref)
      dirs <- ~deps.map(layout.classesDir(_))
      bins <- ~deps.flatMap(_.binaries)
    } yield (dirs ++ bins ++ art.binaries)

  def runtimeClasspath(ref: ModuleRef)(implicit layout: Layout, shell: Shell): Outcome[Set[Path]] =
    for {
      cp       <- classpath(ref)
      art      <- artifact(ref)
      compiler <- ~art.compiler
      compilerCp <- compiler.map { c =>
                     classpath(c.ref)
                   }.getOrElse(Success(Set()))
    } yield compilerCp ++ cp + layout.classesDir(art)

  def dependencies(ref: ModuleRef)(implicit layout: Layout, shell: Shell): Outcome[Set[Artifact]] =
    for {
      project <- project(ref.projectId)
      module  <- project(ref.moduleId)
      deps    <- module.after.map(artifact).sequence
    } yield deps

  def transitiveDependencies(
      ref: ModuleRef
    )(implicit shell: Shell,
      layout: Layout
    ): Outcome[Set[Artifact]] =
    for {
      after  <- dependencies(ref)
      tDeps  <- after.map(_.ref).map(transitiveDependencies).sequence
      itDeps = tDeps.flatten.filterNot(_.intransitive).to[Set]
    } yield after ++ itDeps

  def clean(ref: ModuleRef)(implicit layout: Layout, shell: Shell): Unit =
    layout.classesDir.delete().unit

  def allParams(ref: ModuleRef)(implicit layout: Layout, shell: Shell): Outcome[List[String]] =
    for {
      tDeps    <- transitiveDependencies(ref)
      plugins  <- ~tDeps.filter(_.kind == Plugin)
      artifact <- artifact(ref)
    } yield
      (artifact.params ++ plugins.map { plugin =>
        Parameter(str"Xplugin:${layout.classesDir(plugin)}")
      }).map(_.parameter)

  def compilation(ref: ModuleRef)(implicit shell: Shell, layout: Layout): Outcome[Compilation] =
    for {
      art <- artifact(ref)
      graph <- transitiveDependencies(ref).map(_.map { a =>
                (a.ref, a.dependencies)
              }.toMap.updated(art.ref, art.dependencies))
      artifacts <- graph.keys.map { key =>
                    artifact(key).map(key -> _)
                  }.sequence.map(_.toMap)
      checkouts <- graph.keys.map { key =>
                    checkout(key)
                  }.sequence
    } yield Compilation(graph, checkouts.foldLeft(Set[Checkout]())(_ ++ _), artifacts)

  def saveJars(
      cli: Cli[_]
    )(io: cli.Io,
      ref: ModuleRef,
      dest: Path
    )(implicit shell: Shell,
      layout: Layout
    ): Outcome[Unit] =
    for {
      dest    <- dest.directory
      current <- artifact(ref)
      deps    <- transitiveDependencies(ref)
      dirs    <- ~deps.map(layout.classesDir(_))
      files <- ~dirs.map { dir =>
                (dir, dir.children)
              }.filter(_._2.nonEmpty)
      bins         <- ~deps.flatMap(_.binaries)
      _            <- ~io.println(msg"Writing manifest file ${layout.manifestFile(current)}")
      manifestFile <- Manifest.file(layout.manifestFile(current), bins.map(_.name), None)
      path         <- ~(dest / str"${ref.projectId.key}-${ref.moduleId.key}.jar")
      _            <- ~io.println(msg"Saving JAR file $path")
      _            <- shell.aggregatedJar(path, files, manifestFile)
      _ <- ~bins.foreach { b =>
            b.copyTo(dest / b.name)
          }
    } yield ()

  def compile(
      artifact: Artifact,
      multiplexer: Multiplexer[ModuleRef, CompileEvent],
      futures: Map[ModuleRef, Future[CompileResult]] = Map()
    )(implicit layout: Layout,
      shell: Shell
    ): Map[ModuleRef, Future[CompileResult]] = {

    // FIXME: don't .get
    val deps = dependencies(artifact.ref).toOption.get

    val newFutures = deps.foldLeft(futures) { (futures, dep) =>
      if (futures.contains(dep.ref)) futures
      else compile(dep, multiplexer, futures)
    }

    val dependencyFutures = Future.sequence(deps.map(_.ref).map(newFutures))

    val future = dependencyFutures.flatMap { inputs =>
      if (inputs.exists(!_.success)) {
        multiplexer(artifact.ref) = SkipCompile(artifact.ref)
        multiplexer.close(artifact.ref)
        Future.successful(CompileResult(false, ""))
      } else
        Future {
          val out = new StringBuilder()
          multiplexer(artifact.ref) = StartCompile(artifact.ref)

          val compileResult: Boolean = blocking {
            shell.bloop
              .compile(artifact.hash.encoded) { ln =>
                out.append(ln)
                out.append("\n")
              }
              .await() == 0
          }

          val finalResult = if (compileResult && artifact.kind == Application) {
            shell
              .runJava(
                  runtimeClasspath(artifact.ref).toOption.get.to[List].map(_.value),
                  artifact.main.getOrElse("")) { ln =>
                out.append(ln)
                out.append("\n")
              }
              .await() == 0
          } else compileResult

          multiplexer(artifact.ref) = StopCompile(artifact.ref, out.toString, finalResult)

          multiplexer.close(artifact.ref)

          CompileResult(finalResult, out.toString)
        }
    }

    newFutures.updated(artifact.ref, future)
  }

}

case class SchemaTree(schema: Schema, dir: Path, inherited: Set[SchemaTree]) {

  lazy val universe: Outcome[Universe] = {
    val localProjectIds          = schema.projects.map(_.id)
    val empty: Outcome[Universe] = Success(Universe())
    inherited
      .foldLeft(empty) { (projects, schemaTree) =>
        projects.flatMap { projects =>
          schemaTree.universe.flatMap { nextProjects =>
            val potentialConflictIds = (projects.ids -- localProjectIds).intersect(nextProjects.ids)
            val conflictIds = potentialConflictIds.filter { id =>
              projects.project(id) != nextProjects.project(id)
            }
            if (conflictIds.isEmpty) Success(projects ++ nextProjects)
            else Failure(ProjectConflict(conflictIds))
          }
        }
      }
      .map { old =>
        val newProjects = schema.projects.map { p =>
          p.id -> p
        }.toMap
        val newSchemas = schema.projects.map(_.id -> schema).toMap
        val newDirs    = schema.projects.map(_.id -> dir).toMap
        old ++ Universe(newProjects, newSchemas, newDirs)
      }
  }
}

object Schema {
  implicit val msgShow: MsgShow[Schema]       = v => UserMsg(_.schema(v.id.key))
  implicit val stringShow: StringShow[Schema] = _.id.key
  implicit def diff: Diff[Schema]             = Diff.gen[Schema]
}

case class Schema(
    id: SchemaId,
    projects: SortedSet[Project] = TreeSet(),
    repos: SortedSet[SourceRepo] = TreeSet(),
    imports: List[SchemaRef] = List(),
    main: Option[ProjectId] = None) {

  def repo(repoId: RepoId)(implicit layout: Layout, shell: Shell): Outcome[SourceRepo] =
    repos.findBy(repoId)

  def moduleRefs: SortedSet[ModuleRef] = projects.flatMap(_.moduleRefs)

  def compilerRefs(implicit layout: Layout, shell: Shell): List[ModuleRef] =
    allProjects.toOption.to[List].flatMap(_.flatMap(_.compilerRefs))

  def mainProject: Outcome[Option[Project]] =
    main.map(projects.findBy(_)).to[List].sequence.map(_.headOption)

  def importCandidates(implicit layout: Layout, shell: Shell): List[String] =
    repos.to[List].flatMap(_.importCandidates(this).toOption.to[List].flatten)

  def moduleRefStrings(implicit layout: Layout, shell: Shell): List[String] =
    importedSchemas.toOption.to[List].flatMap(_.flatMap(_.moduleRefStrings)) ++
      moduleRefs.to[List].map { ref =>
        str"$ref"
      }

  def schemaTree(dir: Path)(implicit layout: Layout, shell: Shell): Outcome[SchemaTree] =
    for {
      imps <- imports.map { ref =>
               for {
                 repo         <- repos.findBy(ref.repo)
                 repoDir      <- repo.fullCheckout.get
                 nestedLayout <- ~Layout(layout.home, repoDir)
                 layer        <- Layer.read(nestedLayout.furyConfig)(nestedLayout, shell)
                 resolved     <- layer.schemas.findBy(ref.schema)
                 tree         <- resolved.schemaTree(repoDir)
               } yield tree
             }.sequence
    } yield SchemaTree(this, dir, imps.to[Set])

  def importedSchemas(implicit layout: Layout, shell: Shell): Outcome[List[Schema]] =
    imports.map(_.resolve(this)).sequence

  def sourceRepoIds: SortedSet[RepoId] = repos.map(_.id)

  def allProjects(implicit layout: Layout, shell: Shell): Outcome[List[Project]] =
    importedSchemas
      .flatMap(_.map(_.allProjects).sequence.map(_.flatten))
      .map(_ ++ projects.to[List])

  def unused(projectId: ProjectId) = projects.find(_.id == projectId) match {
    case None    => Success(projectId)
    case Some(m) => Failure(ProjectAlreadyExists(m.id))
  }

  def duplicate(id: String) = copy(id = SchemaId(id))
}

object AliasCmd {
  implicit val msgShow: MsgShow[AliasCmd]       = v => UserMsg(_.module(v.key))
  implicit val stringShow: StringShow[AliasCmd] = _.key
}

case class AliasCmd(key: String)

object Alias {
  implicit val msgShow: MsgShow[Alias]       = v => UserMsg(_.module(v.cmd.key))
  implicit val stringShow: StringShow[Alias] = _.cmd.key
}

case class Alias(cmd: AliasCmd, description: String, schema: Option[SchemaId], module: ModuleRef)

case class Layer(
    version: Int = Layer.CurrentVersion,
    schemas: SortedSet[Schema] = TreeSet(Schema(SchemaId.default)),
    main: SchemaId = SchemaId.default,
    aliases: SortedSet[Alias] = TreeSet()) { layer =>

  def mainSchema: Outcome[Schema] = schemas.findBy(main)

  def showSchema: Boolean = schemas.size > 1

  def apply(schemaId: SchemaId): Outcome[Schema] =
    schemas.find(_.id == schemaId).ascribe(ItemNotFound(schemaId))

  def projects: Outcome[SortedSet[Project]] = mainSchema.map(_.projects)
}

object Layer {
  val CurrentVersion = 2

  def read(string: String)(implicit layout: Layout, shell: Shell): Outcome[Layer] =
    Success(Ogdl.read[Layer](string, upgrade(_)))

  def read(file: Path)(implicit layout: Layout, shell: Shell): Outcome[Layer] =
    Success(Ogdl.read[Layer](file, upgrade(_)).toOption.getOrElse(Layer()))

  private def upgrade(ogdl: Ogdl)(implicit shell: Shell): Ogdl =
    (try ogdl.version().toInt
    catch { case e: Exception => 1 }) match {
      case 1 =>
        //io.println("Migrating layer.fury from file format v1")
        ogdl.set(
            schemas = ogdl.schemas.map { schema =>
              schema.set(
                  repos = schema.repos.map { repo =>
                    //io.println(s"Checking commit hash for ${repo.repo()}")
                    val commit = shell.git.lsRemoteRefSpec(repo.repo(), repo.refSpec()).toOption.get
                    repo.set(commit = Ogdl(CheckoutId(commit)), track = repo.refSpec)
                  }
              )
            },
            version = Ogdl(2)
        )
      case CurrentVersion => ogdl
    }

}

object ModuleRef {

  implicit val stringShow: StringShow[ModuleRef] = ref => str"${ref.projectId}/${ref.moduleId}"
  implicit val entityName: EntityName[ModuleRef] = EntityName(msg"dependency")

  implicit val msgShow: MsgShow[ModuleRef] =
    ref =>
      UserMsg { theme =>
        msg"${theme.project(ref.projectId.key)}${theme.gray("/")}${theme.module(ref.moduleId.key)}"
          .string(theme)
      }

  val JavaRef = ModuleRef(ProjectId("java"), ModuleId("compiler"), false)

  def parse(project: Project, string: String, intransitive: Boolean): Outcome[ModuleRef] =
    string match {
      case r"$projectId@([a-z][a-z0-9\-]*[a-z0-9])\/$moduleId@([a-z][a-z0-9\-]*[a-z0-9])" =>
        Success(ModuleRef(ProjectId(projectId), ModuleId(moduleId), intransitive))
      case r"[a-z][a-z0-9\-]*[a-z0-9]" =>
        Success(ModuleRef(project.id, ModuleId(string), intransitive))
      case _ =>
        Failure(ItemNotFound(ModuleId(string)))
    }
}

case class ModuleRef(projectId: ProjectId, moduleId: ModuleId, intransitive: Boolean = false) {
  override def equals(that: Any): Boolean = that match {
    case ModuleRef(p, m, _) => projectId == p && moduleId == m
    case _                  => false
  }

  override def hashCode: Int = projectId.hashCode + moduleId.hashCode

  override def toString: String = str"$projectId/$moduleId"
}

object SchemaId {
  implicit val msgShow: MsgShow[SchemaId]       = v => UserMsg(_.schema(v.key))
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
  implicit val msgShow: MsgShow[ProjectId]       = p => UserMsg(_.project(p.key))
  implicit val stringShow: StringShow[ProjectId] = _.key
  implicit def diff: Diff[ProjectId]             = (l, r) => Diff.stringDiff.diff(l.key, r.key)
}

case class ProjectId(key: String) extends Key(msg"project")

object ModuleId {
  implicit val msgShow: MsgShow[ModuleId]       = m => UserMsg(_.module(m.key))
  implicit val stringShow: StringShow[ModuleId] = _.key
  implicit def diff: Diff[ModuleId]             = (l, r) => Diff.stringDiff.diff(l.key, r.key)
  final val Core: ModuleId                      = ModuleId("core")
}

case class ModuleId(key: String) extends Key(msg"module")

object Repo {
  implicit val msgShow: MsgShow[Repo]       = r => UserMsg(_.url(r.simplified))
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

case class Checkout(
    repo: Repo,
    local: Boolean,
    commit: CheckoutId,
    refSpec: RefSpec,
    sources: List[Path]) {

  def hash: Digest                        = this.digest[Md5]
  def path(implicit layout: Layout): Path = layout.srcsDir / hash.encoded

  def get(implicit shell: Shell, layout: Layout): Outcome[Path] =
    for {
      repoDir    <- repo.fetch
      workingDir <- checkout
    } yield workingDir

  private def checkout(implicit shell: Shell, layout: Layout): Outcome[Path] =
    if (!(path / ".done").exists) {

      if (path.exists()) {
        println(s"Found incomplete checkout of ${if (sources.isEmpty) "all sources"
        else sources.map(_.value).mkString("[", ", ", "]")}.")
        path.delete()
      }

      println(s"Checking out ${if (sources.isEmpty) "all sources"
      else sources.map(_.value).mkString("[", ", ", "]")}.")
      path.mkdir()
      shell.git
        .sparseCheckout(repo.path, path, sources, refSpec = refSpec.id, commit = commit.id)
        .map { _ =>
          path
        }
    } else Success(path)
}

object SourceRepo {
  implicit val msgShow: MsgShow[SourceRepo]       = r => UserMsg(_.repo(r.id.key))
  implicit val stringShow: StringShow[SourceRepo] = _.id.key
  implicit def diff: Diff[SourceRepo]             = Diff.gen[SourceRepo]
}

case class SourceRepo(
    id: RepoId,
    repo: Repo,
    track: RefSpec,
    commit: CheckoutId,
    local: Option[Path]) { // TODO: change Option[String] to RefSpec

  def listFiles(implicit layout: Layout, shell: Shell): Outcome[List[Path]] =
    for {
      dir <- local.map(Success(_)).getOrElse(repo.fetch)
      commit <- ~shell.git
                 .getTag(dir, track.id)
                 .toOption
                 .orElse(shell.git.getBranchHead(dir, track.id).toOption)
                 .getOrElse(track.id)
      files <- local.map { _ =>
                Success(dir.children.map(Path(_)).to[List])
              }.getOrElse(shell.git.lsTree(dir, commit))
    } yield files

  def fullCheckout: Checkout = Checkout(repo, local.isDefined, commit, track, List())

  def importCandidates(
      schema: Schema
    )(implicit layout: Layout,
      shell: Shell
    ): Outcome[List[String]] =
    for {
      repoDir     <- repo.fetch
      layerString <- shell.git.showFile(repoDir, "layer.fury")
      layer       <- Layer.read(layerString)
      schemas     <- ~layer.schemas.to[List]
    } yield
      schemas.map { schema =>
        str"${id.key}:${schema.id.key}"
      }

  def current(implicit shell: Shell, layout: Layout): Outcome[RefSpec] =
    for {
      dir    <- local.map(Success(_)).getOrElse(repo.fetch)
      commit <- shell.git.getCommit(dir)
    } yield RefSpec(commit)

  def sourceCandidates(
      pred: String => Boolean
    )(implicit layout: Layout,
      shell: Shell
    ): Outcome[Set[Source]] =
    listFiles.map { files =>
      files.filter { f =>
        pred(f.filename)
      }.map { p =>
        ExternalSource(id, p.parent): Source
      }.to[Set]
    }
}

case class BinRepoId(id: String)

object BinRepoId {
  implicit val msgShow: MsgShow[BinRepoId]       = v => UserMsg(_.repo(v.id))
  implicit val stringShow: StringShow[BinRepoId] = _.id
  final val Central: BinRepoId                   = BinRepoId("central")
}

case class Repo(url: String) {
  def hash: Digest                        = url.digest[Md5]
  def path(implicit layout: Layout): Path = layout.reposDir / hash.encoded

  def update()(implicit shell: Shell, layout: Layout): Outcome[UserMsg] =
    for {
      oldCommit <- shell.git.getCommit(path)
      _         <- shell.git.fetch(path, None)
      newCommit <- shell.git.getCommit(path)
      msg <- ~(if (oldCommit != newCommit) msg"Repository ${url} updated to new commit $newCommit"
               else msg"Repository ${url} is unchanged")
    } yield msg

  def getCommitFromTag(implicit layout: Layout, shell: Shell, tag: RefSpec): Outcome[String] =
    for {
      commit <- shell.git.getCommitFromTag(path, tag.id)
    } yield commit

  def fetch(implicit layout: Layout, shell: Shell): Outcome[Path] =
    if (!(path / ".done").exists) {
      if (path.exists()) {
        println(s"Found incomplete clone of $url.")
        path.delete()
      }

      println(s"Cloning Git repository $url.")
      path.mkdir()
      shell.git.cloneBare(url, path).map { _ =>
        path
      }
    } else Success(path)

  def simplified: String = url match {
    case r"git@github.com:$group@(.*)/$project@(.*)\.git"    => s"gh:$group/$project"
    case r"git@bitbucket.com:$group@(.*)/$project@(.*)\.git" => s"bb:$group/$project"
    case r"git@gitlab.com:$group@(.*)/$project@(.*)\.git"    => s"gl:$group/$project"
    case other                                               => other
  }

  def projectName: Outcome[RepoId] = url match {
    case r".*/$project@([^\/]*).git" => Success(RepoId(project))
    case value                       => Failure(InvalidValue(value))
  }
}

object SchemaRef {

  implicit val msgShow: MsgShow[SchemaRef] =
    v =>
      UserMsg { theme =>
        msg"${v.repo}${theme.gray(":")}${v.schema}".string(theme)
      }

  implicit val stringShow: StringShow[SchemaRef] = sr => str"${sr.repo}:${sr.schema}"
  implicit def diff: Diff[SchemaRef]             = Diff.gen[SchemaRef]

  def unapply(value: String): Option[SchemaRef] = value match {
    case r"$repo@([a-z0-9\.\-]*[a-z0-9]):$schema@([a-zA-Z0-9\-\.]*[a-zA-Z0-9])$$" =>
      Some(SchemaRef(RepoId(repo), SchemaId(schema)))
    case _ =>
      None
  }
}

case class SchemaRef(repo: RepoId, schema: SchemaId) {

  def resolve(base: Schema)(implicit layout: Layout, shell: Shell): Outcome[Schema] =
    for {
      repo     <- base.repos.findBy(repo)
      dir      <- repo.fullCheckout.get
      layer    <- Layer.read(Layout(layout.home, dir).furyConfig)
      resolved <- layer.schemas.findBy(schema)
    } yield resolved
}

sealed trait CompileEvent
case object Tick                                                         extends CompileEvent
case class StartCompile(ref: ModuleRef)                                  extends CompileEvent
case class StopCompile(ref: ModuleRef, output: String, success: Boolean) extends CompileEvent
case class SkipCompile(ref: ModuleRef)                                   extends CompileEvent

case class CompileResult(success: Boolean, output: String)

case class Artifact(
    ref: ModuleRef,
    kind: Kind,
    main: Option[String],
    plugin: Option[String],
    repos: List[Repo],
    checkouts: List[Checkout],
    binaries: List[Path],
    dependencies: List[ModuleRef],
    compiler: Option[Artifact],
    bloopSpec: Option[BloopSpec],
    params: List[Parameter],
    intransitive: Boolean,
    localSources: List[Path],
    sharedSources: List[Path]) {

  def hash: Digest =
    (kind, main, checkouts, binaries, dependencies, compiler, params).digest[Md5]

  def writePlugin()(implicit layout: Layout): Unit = if (kind == Plugin) {
    val file = layout.classesDir(this) / "scalac-plugin.xml"

    main.foreach { main =>
      file.writeSync(
          str"<plugin><name>${plugin.getOrElse("plugin"): String}</name><classname>${main}</classname></plugin>")
    }
  }

  def sourcePaths(implicit layout: Layout): List[Path] =
    localSources ++ sharedSources ++ checkouts.flatMap { c =>
      if (c.local) c.sources.map(_ in layout.pwd) else c.sources.map(_ in c.path)
    }

}

object Project {
  implicit val msgShow: MsgShow[Project]       = v => UserMsg(_.project(v.id.key))
  implicit val stringShow: StringShow[Project] = _.id.key
  implicit def diff: Diff[Project]             = Diff.gen[Project]

  def available(projectId: ProjectId, layer: Layer): Boolean =
    !layer.projects.toOption.to[List].flatten.findBy(projectId).isSuccess
}

case class Project(
    id: ProjectId,
    modules: SortedSet[Module] = TreeSet(),
    main: Option[ModuleId] = None,
    license: LicenseId = License.unknown,
    description: String = "") {
  def moduleRefs: List[ModuleRef] = modules.to[List].map(_.ref(this))

  def mainModule: Outcome[Option[Module]] =
    main.map(modules.findBy(_)).to[List].sequence.map(_.headOption)

  def compilerRefs: List[ModuleRef] =
    modules.to[List].collect {
      case m @ Module(_, Compiler, _, _, _, _, _, _, _, _, _, _) => m.ref(this)
    }

  def unused(moduleId: ModuleId) =
    modules.findBy(moduleId) match {
      case Success(_) =>
        Failure(ModuleAlreadyExists(moduleId))
      case _ =>
        Success(moduleId)
    }

  def apply(module: ModuleId): Outcome[Module] = modules.findBy(module)
}

object License {
  implicit val msgShow: MsgShow[License]       = v => UserMsg(_.license(v.id.key))
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
      License(LicenseId("zlib"), "zLib License")
  )
}

object LicenseId {
  implicit val msgShow: MsgShow[LicenseId]       = v => UserMsg(_.license(v.key))
  implicit val stringShow: StringShow[LicenseId] = _.key
}
case class LicenseId(key: String) extends Key(msg"license")

case class License(id: LicenseId, name: String)

object RefSpec {
  implicit val msgShow: MsgShow[RefSpec]       = v => UserMsg(_.version(v.id))
  implicit val stringShow: StringShow[RefSpec] = _.id

  val master = RefSpec("master")
}
case class RefSpec(id: String)

object CheckoutId {
  implicit val stringShow: StringShow[CheckoutId] = _.id
  implicit val msgShow: MsgShow[CheckoutId]       = r => UserMsg(_.version(r.id.take(7)))
}
case class CheckoutId(id: String)

object Source {
  implicit val stringShow: StringShow[Source] = _.description

  implicit val msgShow: MsgShow[Source] = v =>
    UserMsg { theme =>
      v match {
        case ExternalSource(repoId, path) =>
          msg"${theme.repo(repoId.key)}${theme.gray(":")}${theme.path(path.value)}".string(theme)
        case SharedSource(path) =>
          msg"${theme.repo("shared")}${theme.gray(":")}${theme.path(path.value)}".string(theme)
        case LocalSource(path) => msg"${theme.path(path.value)}".string(theme)
      }
    }

  def unapply(string: String): Option[Source] = string match {
    case r"shared:$path@(.*)" =>
      Some(SharedSource(Path(path)))
    case r"$repo@([a-z][a-z0-9\.\-]*[a-z0-9]):$path@(.*)" =>
      Some(ExternalSource(RepoId(repo), Path(path)))
    case r"$path@(.*)" => Some(LocalSource(Path(path)))
    case _             => None
  }

  implicit val ogdlReader: OgdlReader[Source] = src => unapply(src()).get // FIXME
  implicit val ogdlWriter: OgdlWriter[Source] = src => Ogdl(src.description)

  def repoId(src: Source): Option[RepoId] = src match {
    case ExternalSource(repoId, _) => Some(repoId)
    case _                         => None
  }
}

trait Source {
  def description: String

  def hash(schema: Schema)(implicit shell: Shell, layout: Layout): Outcome[Digest]
  def path: Path
  def repoIdentifier: RepoId
}

case class ExternalSource(repoId: RepoId, path: Path) extends Source {
  def description: String = str"${repoId}:${path.value}"

  def hash(schema: Schema)(implicit shell: Shell, layout: Layout): Outcome[Digest] =
    schema.repo(repoId).map((path, _).digest[Md5])

  def repoIdentifier: RepoId = repoId
}

case class SharedSource(path: Path) extends Source {
  def description: String = str"shared:${path.value}"

  def hash(schema: Schema)(implicit shell: Shell, layout: Layout): Outcome[Digest] =
    Success((-2, path).digest[Md5])

  def repoIdentifier: RepoId = RepoId("-")
}

case class LocalSource(path: Path) extends Source {
  def description: String = str"${path.value}"

  def hash(schema: Schema)(implicit shell: Shell, layout: Layout): Outcome[Digest] =
    Success((-1, path).digest[Md5])

  def repoIdentifier: RepoId = RepoId("-")
}

object RepoId {
  implicit val msgShow: MsgShow[RepoId]       = r => UserMsg(_.repo(r.key))
  implicit val stringShow: StringShow[RepoId] = _.key
}

case class RepoId(key: String) extends Key(msg"repository")

object Parameter {
  implicit val stringShow: StringShow[Parameter] = _.name
  implicit val msgShow: MsgShow[Parameter]       = v => UserMsg(_.param(v.name))
}

case class Parameter(name: String) { def parameter = str"-$name" }

abstract class Key(val kind: UserMsg) { def key: String }
