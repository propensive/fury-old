/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.6.7. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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

import fury._, io._, strings._, ogdl._, model._, utils._
import Graph.DiagnosticMessage
import exoskeleton._
import gastronomy._
import kaleidoscope._
import mercator._

import ch.epfl.scala.bsp4j.{CompileResult => _, _}

import scala.collection.immutable.{SortedSet, TreeSet}
import scala.collection.mutable.HashMap
import scala.util._

import language.higherKinds
import scala.annotation.tailrec

object Binary {
  implicit val msgShow: MsgShow[Binary] = v => UserMsg(_.binary(v.spec))
  implicit val stringShow: StringShow[Binary] = _.spec
  implicit def diff: Diff[Binary] = Diff.gen[Binary]

  def unapply(service: BinRepoId, string: String): Try[Binary] =
    string match {
      case r"$g@([\.\-_a-zA-Z0-9]*)\:$a@([\.\-_a-zA-Z0-9]*)\:$v@([\.\-\+_a-zA-Z0-9]*)" =>
        Success(Binary(service, g, a, v))
      case _ =>
        Failure(InvalidArgValue("binary", string))
    }

  private val compilerVersionCache: HashMap[Binary, Try[String]] = HashMap()
  
  val Jmh = Binary(BinRepoId.Central, "org.openjdk.jmh", "jmh-core", "1.21")
}

case class Binary(binRepo: BinRepoId, group: String, artifact: String, version: String) {
  def spec = str"$group:$artifact:$version"
  def paths(io: Io): Try[List[Path]] = Coursier.fetch(io, this)
}

object Policy {
  def read(io: Io, layout: GlobalLayout): Try[Policy] =
    Success(Ogdl.read[Policy](layout.policyFile,
        upgrade(io, layout, _)).toOption.getOrElse(Policy(SortedSet.empty[Grant])))

  def save(io: Io, layout: GlobalLayout, policy: Policy): Try[Unit] =
    layout.policyFile.writeSync(Ogdl.serialize(Ogdl(policy)))
  
  private def upgrade(io: Io, layout: GlobalLayout, ogdl: Ogdl): Ogdl = ogdl
}

case class Policy(policy: SortedSet[Grant] = TreeSet()) {
  def forContext(layout: Layout, projectId: ProjectId/*, layer: Layer*/): Policy =
    Policy(policy.filter {
      case Grant(DirectoryScope(dir), _) => dir == layout.base.value
      case Grant(ProjectScope(id), _)    => projectId == id
      //case Grant(LayerScope(hash), _)    => hash == layer.hash
    })

  def grant(scope: Scope, permissions: List[Permission]): Policy =
    copy(policy = policy ++ permissions.map(Grant(scope, _)))

  def obviate(scope: Scope, permissions: List[Permission]): Policy =
    copy(policy = policy.filterNot(permissions.contains))
  
  def checkAll(permissions: Iterable[Permission]): Try[Unit] = {
    val missing = permissions.to[Set] -- policy.map(_.permission)
    if(missing.isEmpty) Success(()) else Failure(NoPermissions(missing))
  }

  def save(file: Path): Try[Unit] = file.writeSync {
    val sb = new StringBuilder()
    sb.append("grant {\n")
    policy.foreach { grant =>
      val p = grant.permission
      val actionAddendum = p.action.fold("") { a => s""", "$a"""" }
      sb.append(str""" permission ${p.className} "${p.target}"${actionAddendum};""")
      sb.append('\n')
    }
    sb.append("};\n")
    sb.toString
  }
}

object Module {
  implicit val msgShow: MsgShow[Module]       = v => UserMsg(_.module(v.id.key))
  implicit val stringShow: StringShow[Module] = _.id.key
  implicit val diff: Diff[Module] = Diff.gen[Module]

  def available(id: ModuleId, project: Project): Try[ModuleId] =
    project.modules.find(_.id == id).fold(Try(id)) { module => Failure(ModuleAlreadyExists(module.id)) }
}

case class Module(id: ModuleId,
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
                  bloopSpec: Option[BloopSpec] = None,
                  environment: SortedSet[EnvVar] = TreeSet(),
                  properties: SortedSet[JavaProperty] = TreeSet(),
                  policy: SortedSet[Permission] = TreeSet()) {

  def allBinaries: SortedSet[Binary] = if(kind == Benchmarks) binaries + Binary.Jmh else binaries
  def compilerDependencies: Set[ModuleRef] = Set(compiler).filter(_ != ModuleRef.JavaRef).map(_.hide)
  def ref(project: Project): ModuleRef = ModuleRef(project.id, id)
  def externalSources: SortedSet[ExternalSource] = sources.collect { case src: ExternalSource => src }
  def sharedSources: SortedSet[SharedSource] = sources.collect { case src: SharedSource => src }
  def localSources: SortedSet[Path] = sources.collect { case src: LocalSource => src.path }

  def permission(hashPrefix: PermissionHash): Option[Permission] = {
    val allMatches = policy.filter(_.hash.startsWith(hashPrefix.key))
    if (allMatches.size == 1) Some(allMatches.head) else None
  }
  
  def policyEntries: Set[PermissionEntry] = {
    val prefixLength = Compare.uniquePrefixLength(policy.map(_.hash)).max(3)
    policy.map { p => PermissionEntry(p, PermissionHash(p.hash.take(prefixLength))) }
  }
}

case class ProjectSpec(project: Project, repos: Map[RepoId, SourceRepo])

case class Entity(project: Project, schema: Schema, path: Path) {
  def spec: ProjectSpec = {
    val repoIds = project.allRepoIds
    ProjectSpec(project, schema.repos.to[List].filter(repoIds contains _.id).map { r => (r.id, r) }.toMap)
  }
}

/** A Universe represents a the fully-resolved set of projects available in the layer */
case class Universe(entities: Map[ProjectId, Entity] = Map()) {
  //projects: Map[ProjectId, Project] = Map(),
  //schemas: Map[ProjectId, Schema] = Map(),
  //dirs: Map[ProjectId, Path] = Map()) {
  
  def ids: Set[ProjectId] = entities.keySet
  def entity(id: ProjectId): Try[Entity] = entities.get(id).ascribe(ItemNotFound(id))

  def makeTarget(io: Io, ref: ModuleRef, layout: Layout): Try[Target] =
    for {
      resolvedProject <- entity(ref.projectId)
      module          <- resolvedProject.project(ref.moduleId)
      compiler        <- if(module.compiler == ModuleRef.JavaRef) Success(None)
                         else makeTarget(io, module.compiler, layout).map(Some(_))
      binaries        <- module.allBinaries.map(_.paths(io)).sequence.map(_.flatten)
      dependencies    <- module.after.traverse { dep => for{
                           origin <- entity(dep.projectId)
                         } yield TargetId(origin.schema.id, dep)}
      checkouts       <- checkout(ref, layout)
    } yield {
      val sourcePaths = module.localSources.map(_ in resolvedProject.path).to[List] ++
        module.sharedSources.map(_.path in layout.sharedDir).to[List] ++
        checkouts.flatMap { c =>
          c.local match {
            case Some(p) => c.sources.map(_ in p)
            case None    => c.sources.map(_ in c.path(layout))
          }
        }
      Target(
        ref,
        resolvedProject.schema.id,
        module.kind,
        module.main,
        module.plugin,
        resolvedProject.schema.repos.map(_.repo).to[List],
        checkouts.to[List],
        binaries.to[List],
        dependencies.to[List],
        compiler,
        module.bloopSpec,
        module.params.to[List],
        module.policy.to[List],
        ref.intransitive,
        sourcePaths,
        module.environment.map { e => (e.key, e.value) }.toMap,
        module.properties.map { p => (p.key, p.value) }.toMap
      )
    }

  def checkout(ref: ModuleRef, layout: Layout): Try[Set[Checkout]] =
    for {
      entity <- entity(ref.projectId)
      module <- entity.project(ref.moduleId)
      repos <- module.externalSources
                .groupBy(_.repoId)
                .map { case (k, v) => entity.schema.repo(k, layout).map(_ -> v) }
                .sequence
    } yield
      repos.map {
        case (repo, paths) =>
          Checkout(
              repo.id,
              repo.repo,
              repo.local,
              repo.commit,
              repo.track,
              paths.map(_.path).to[List])
      }.to[Set]

  def ++(that: Universe): Universe =
    Universe(entities ++ that.entities)

  private[fury] def dependencies(io: Io, ref: ModuleRef, layout: Layout): Try[Set[ModuleRef]] =
    for {
      entity <- entity(ref.projectId)
      module <- entity.project(ref.moduleId)
      deps    = module.after ++ module.compilerDependencies
      tDeps  <- deps.map(dependencies(io, _, layout)).sequence
    } yield deps ++ tDeps.flatten

  def clean(ref: ModuleRef, layout: Layout): Unit = layout.classesDir.delete().unit

  def getMod(ref: ModuleRef): Try[Module] = for {
    entity <- entity(ref.projectId)
    module <- entity.project(ref.moduleId)
  } yield module

  def compilation(io: Io, ref: ModuleRef, policy: Policy, layout: Layout): Try[Compilation] = for {
    target    <- makeTarget(io, ref, layout)
    entity    <- entity(ref.projectId)
    graph     <- dependencies(io, ref, layout).map(_.map(makeTarget(io, _, layout)).map { a =>
                   a.map { dependencyTarget =>
                     (dependencyTarget.id, dependencyTarget.dependencies ++ dependencyTarget.compiler.map(_.id))
                   }
                 }.sequence.map(_.toMap.updated(target.id, target.dependencies ++
                     target.compiler.map(_.id)))).flatten
    targets   <- graph.keys.map { targetId =>
                  makeTarget(io, targetId.ref, layout).map(targetId.ref -> _)
                }.sequence.map(_.toMap)
    permissions = targets.flatMap(_._2.permissions)
    _         <- policy.checkAll(permissions)
    appModules = targets.filter(_._2.executed)
    subgraphs  = DirectedGraph(graph.mapValues(_.to[Set])).subgraph(appModules.map(_._2.id).to[Set] +
                     TargetId(entity.schema.id, ref)).connections.mapValues(_.to[List])
    checkouts <- graph.keys.map { targetId => checkout(targetId.ref, layout) }.sequence
  } yield
    Compilation(graph, subgraphs, checkouts.foldLeft(Set[Checkout]())(_ ++ _),
        targets ++ (target.compiler.map { compilerTarget => compilerTarget.ref -> compilerTarget }), this)
}

case class Hierarchy(schema: Schema, dir: Path, inherited: Set[Hierarchy]) {
  lazy val universe: Try[Universe] = {
    val localProjectIds = schema.projects.map(_.id)
    
    def merge(universe: Try[Universe], hierarchy: Hierarchy) = for {
      projects             <- universe
      nextProjects         <- hierarchy.universe
      potentialConflictIds  = (projects.ids -- localProjectIds).intersect(nextProjects.ids)
      
      conflictIds           = potentialConflictIds.filter { id =>
                                projects.entity(id).map(_.spec) != nextProjects.entity(id).map(_.spec)
                              }
      
      allProjects          <- conflictIds match {
                                case x if x.isEmpty => Success(projects ++ nextProjects)
                                case _ => Failure(ProjectConflict(conflictIds/*, h1 = this, h2 = hierarchy*/))
                              }
    } yield allProjects

    val empty: Try[Universe] = Success(Universe())
    
    for(allInherited <- inherited.foldLeft(empty)(merge)) yield {
      val schemaEntities = schema.projects.map { project => project.id -> Entity(project, schema, dir) }
      allInherited ++ Universe(schemaEntities.toMap)
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
                  imports: SortedSet[SchemaRef] = TreeSet(),
                  main: Option[ProjectId] = None) {

  def apply(id: ProjectId) = projects.findBy(id)
  def repo(repoId: RepoId, layout: Layout): Try[SourceRepo] = repos.findBy(repoId)
  def moduleRefs: SortedSet[ModuleRef] = projects.flatMap(_.moduleRefs)
  def mainProject: Try[Option[Project]] = main.map(projects.findBy(_)).to[List].sequence.map(_.headOption)
  def sourceRepoIds: SortedSet[RepoId] = repos.map(_.id)
  def duplicate(id: String) = copy(id = SchemaId(id))

  def compilerRefs(io: Io, layout: Layout, https: Boolean): List[ModuleRef] =
    allProjects(io, layout, https).toOption.to[List].flatMap(_.flatMap(_.compilerRefs))

  def importCandidates(io: Io, layout: Layout, https: Boolean): List[String] =
    repos.to[List].flatMap(_.importCandidates(io, this, layout, https).toOption.to[List].flatten)

  def hierarchy(io: Io, dir: Path, layout: Layout, https: Boolean): Try[Hierarchy] = for {
    imps <- imports.map { ref => for {
      repo         <- repos.findBy(ref.repo)
      repoDir      <- repo.fullCheckout.get(io, layout, https)
      nestedLayout <- ~Layout(layout.home, repoDir, layout.env, repoDir)
      layer        <- Layer.read(io, nestedLayout.furyConfig, nestedLayout)
      resolved     <- layer.schemas.findBy(ref.schema)
      tree         <- resolved.hierarchy(io, repoDir, layout, https)
    } yield tree }.sequence
  } yield Hierarchy(this, dir, imps)

  def importedSchemas(io: Io, layout: Layout, https: Boolean): Try[List[Schema]] =
    imports.to[List].map(resolve(_, io, layout, https)).sequence

  def allProjects(io: Io, layout: Layout, https: Boolean): Try[List[Project]] = {
    @tailrec
    def flatten[T](treeNodes: List[T])(aggregated: List[T], getChildren: T => Try[List[T]]): Try[List[T]] = {
      treeNodes match{
        case Nil => ~aggregated
        case head :: tail =>
          getChildren(head) match {
            case Success(ch) => flatten(ch ::: tail)(head :: aggregated, getChildren)
            case fail => fail
          }
      }
    }

    for {
      allSchemas <- flatten(List(this))(Nil, _.importedSchemas(io, layout, https))
    } yield allSchemas.flatMap(_.projects)
  }

  def unused(projectId: ProjectId): Try[ProjectId] = projects.find(_.id == projectId) match {
    case None    => Success(projectId)
    case Some(m) => Failure(ProjectAlreadyExists(m.id))
  }
  
  def resolve(ref: SchemaRef, io: Io, layout: Layout, https: Boolean): Try[Schema] = for {
    repo     <- repos.findBy(ref.repo)
    dir      <- repo.fullCheckout.get(io, layout, https)
    layer    <- Layer.read(io, Layout(layout.home, dir, layout.env, dir).furyConfig, layout)
    resolved <- layer.schemas.findBy(ref.schema)
  } yield resolved
}

case class Layer(version: Int = Layer.CurrentVersion,
                 schemas: SortedSet[Schema] = TreeSet(Schema(SchemaId.default)),
                 main: SchemaId = SchemaId.default,
                 aliases: SortedSet[Alias] = TreeSet()) { layer =>

  def mainSchema: Try[Schema] = schemas.findBy(main)
  def showSchema: Boolean = schemas.size > 1
  def apply(schemaId: SchemaId): Try[Schema] = schemas.find(_.id == schemaId).ascribe(ItemNotFound(schemaId))
  def projects: Try[SortedSet[Project]] = mainSchema.map(_.projects)
}

object Layer {
  val CurrentVersion = 3

  def read(io: Io, string: String, layout: Layout): Try[Layer] =
    Success(Ogdl.read[Layer](string, upgrade(io, layout, _)))

  def read(io: Io, file: Path, layout: Layout): Try[Layer] =
    Success(Ogdl.read[Layer](file, upgrade(io, layout, _)).toOption.getOrElse(Layer()))

  def save(io: Io, layer: Layer, layout: Layout): Try[Unit] = for {
    layerRepo <- LayerRepository(layout).update(io, layer, layout)
    _         <- Bsp.createConfig(layout)
  } yield ()

  private def upgrade(io: Io, layout: Layout, ogdl: Ogdl): Ogdl =
    Try(ogdl.version().toInt).getOrElse(1) match {
      case 1 =>
        io.println("Migrating layer.fury from file format v1")
        upgrade(
            io,
            layout,
            ogdl.set(
                schemas = ogdl.schemas.map { schema =>
                  schema.set(
                      repos = schema.repos.map { repo =>
                        io.println(msg"Checking commit hash for ${repo.repo()}")
                        val commit =
                          Shell(layout.env).git.lsRemoteRefSpec(repo.repo(), repo.refSpec()).toOption.get
                        repo.set(commit = Ogdl(Commit(commit)), track = repo.refSpec)
                      }
                  )
                },
                version = Ogdl(2)
            )
        )
      case 2 =>
        io.println("Migrating layer.fury from file format v2")
        upgrade(
            io,
            layout,
            ogdl.set(
                schemas = ogdl.schemas.map { schema =>
                  schema.set(
                      projects = schema.projects.map { project =>
                        project.set(
                            modules = project.modules.map { module =>
                              module.set(kind = Ogdl(module.kind().capitalize))
                            }
                        )
                      }
                  )
                },
                version = Ogdl(3)
            )
        )
      case CurrentVersion => ogdl
    }
}

object Repo {
  implicit val msgShow: MsgShow[Repo]       = r => UserMsg(_.url(r.simplified))
  implicit val stringShow: StringShow[Repo] = _.simplified

  case class ExistingLocalFileAsAbspath(absPath: String)

  object ExistingLocalFileAsAbspath {
    def unapply(path: String): Option[String] = Path(path).absolutePath().toOption match {
      case Some(absPath) => absPath.ifExists().map(_.value)
      case None          => None
    }
  }

  def fromString(str: String, https: Boolean): String = str match {
    case "." => ""
    case r"gh:$group@([A-Za-z0-9_\-\.]+)/$project@([A-Za-z0-9\._\-]+)" =>
      if(https) str"https://github.com/$group/$project.git"
      else str"git@github.com:$group/$project.git"
    case r"gl:$group@([A-Za-z0-9_\-\.]+)/$project@([A-Za-z0-9\._\-]+)" =>
      if(https) str"https://gitlab.com/$group/$project.git"
      else str"git@gitlab.com:$group/$project.git"
    case r"bb:$group@([A-Za-z0-9_\-\.]+)/$project@([A-Za-z0-9\._\-]+)" =>
      if(https) str"https://bitbucket.com/$group/$project.git"
      str"git@bitbucket.com:$group/$project.git"
    case ExistingLocalFileAsAbspath(abspath) =>
      abspath
    case other =>
      other
  }
}

case class Checkout(repoId: RepoId,
                    repo: Repo,
                    local: Option[Path],
                    commit: Commit,
                    refSpec: RefSpec,
                    sources: List[Path]) {

  def hash: Digest = this.digest[Md5]
  def path(layout: Layout): Path = layout.srcsDir / hash.encoded

  def get(io: Io, layout: Layout, https: Boolean): Try[Path] = for {
    repoDir    <- repo.fetch(io, layout, https)
    workingDir <- checkout(io, layout)
  } yield workingDir

  private def checkout(io: Io, layout: Layout): Try[Path] =
    local.map(Success(_)).getOrElse {
      if(!(path(layout) / ".done").exists) {
        if(path(layout).exists()) {
          val sourceText = if(sources.isEmpty) "all sources" else sources.map(_.value).mkString("[", ", ", "]")
          io.println(msg"Found incomplete checkout of $sourceText")
          path(layout).delete()
        }

        val sourceDesc: UserMsg = sources match {
          case List() =>
            UserMsg { theme => theme.path("*") }
          case head :: Nil =>
            msg"$head"
          case head :: tail =>
            val init = tail.foldLeft(msg"${'{'}$head") { case (str, next) => msg"$str${','} $next" }
            msg"$init${'}'}"
        }

        io.println(msg"Checking out $sourceDesc from repository $repoId")
        path(layout).mkdir()
        Shell(layout.env).git
          .sparseCheckout(repo.path(layout), path(layout), sources, refSpec = refSpec.id, commit = commit.id)
          .map(path(layout).waive)
      } else Success(path(layout))
    }
}

object SourceRepo {
  implicit val msgShow: MsgShow[SourceRepo] = r => UserMsg(_.repo(r.id.key))
  implicit val stringShow: StringShow[SourceRepo] = _.id.key
  implicit def diff: Diff[SourceRepo] = Diff.gen[SourceRepo]
}

case class SourceRepo(id: RepoId, repo: Repo, track: RefSpec, commit: Commit, local: Option[Path]) {
  def listFiles(io: Io, layout: Layout, https: Boolean): Try[List[Path]] = for {
    dir    <- local.map(Success(_)).getOrElse(repo.fetch(io, layout, https))
    commit <- ~Shell(layout.env).git.getTag(dir, track.id).toOption.orElse(Shell(layout.env).git.getBranchHead(dir,
                  track.id).toOption).getOrElse(track.id)
    files  <- local.map(Success(dir.children.map(Path(_))).waive).getOrElse(Shell(layout.env).git.lsTree(dir,
                  commit))
  } yield files

  def fullCheckout: Checkout = Checkout(id, repo, local, commit, track, List())

  def importCandidates(io: Io, schema: Schema, layout: Layout, https: Boolean): Try[List[String]] = for {
    repoDir     <- repo.fetch(io, layout, https)
    layerString <- Shell(layout.env).git.showFile(repoDir, "layer.fury")
    layer       <- Layer.read(io, layerString, layout)
    schemas     <- ~layer.schemas.to[List]
  } yield schemas.map { schema => str"${id.key}:${schema.id.key}" }

  def current(io: Io, layout: Layout, https: Boolean): Try[RefSpec] = for {
    dir    <- local.map(Success(_)).getOrElse(repo.fetch(io, layout, https))
    commit <- Shell(layout.env).git.getCommit(dir)
  } yield RefSpec(commit)

  def sourceCandidates(io: Io, layout: Layout, https: Boolean)(pred: String => Boolean): Try[Set[Source]] =
    listFiles(io, layout, https).map(_.filter { f => pred(f.filename) }.map { p =>
        ExternalSource(id, p.parent): Source }.to[Set])
}

case class Repo(ref: String) {
  def hash: Digest               = ref.digest[Md5]
  def path(layout: Layout): Path = layout.reposDir / hash.encoded

  def update(layout: Layout): Try[UserMsg] = for {
    oldCommit <- Shell(layout.env).git.getCommit(path(layout)).map(Commit(_))
    _         <- Shell(layout.env).git.fetch(path(layout), None)
    newCommit <- Shell(layout.env).git.getCommit(path(layout)).map(Commit(_))
    msg <- ~(if(oldCommit != newCommit) msg"Repository $this updated to new commit $newCommit"
              else msg"Repository $this is unchanged")
  } yield msg

  def getCommitFromTag(layout: Layout, tag: RefSpec): Try[Commit] =
    for(commit <- Shell(layout.env).git.getCommitFromTag(path(layout), tag.id)) yield Commit(commit)

  def fetch(io: Io, layout: Layout, https: Boolean): Try[Path] =
    if(!(path(layout) / ".done").exists) {
      if(path(layout).exists()) {
        io.println(msg"Found incomplete clone of $this at ${path(layout)}")
        path(layout).delete()
      }

      io.println(msg"Cloning repository at $this")
      path(layout).mkdir()
      Shell(layout.env).git.cloneBare(Repo.fromString(ref, https), path(layout)).map(path(layout).waive)
    } else Success(path(layout))

  def simplified: String = ref match {
    case r"git@github.com:$group@(.*)/$project@(.*)\.git"    => str"gh:$group/$project"
    case r"git@bitbucket.com:$group@(.*)/$project@(.*)\.git" => str"bb:$group/$project"
    case r"git@gitlab.com:$group@(.*)/$project@(.*)\.git"    => str"gl:$group/$project"
    case other                                               => other
  }

  def projectName: Try[RepoId] = ref match {
    case r".*/$project@([^\/]*).git" => Success(RepoId(project))
    case value                       => Failure(InvalidValue(value))
  }
}

sealed trait CompileEvent
case object Tick                                                 extends CompileEvent
case class StartCompile(ref: ModuleRef)                          extends CompileEvent
case class CompilationProgress(ref: ModuleRef, progress: Double) extends CompileEvent
case class StopCompile(ref: ModuleRef, success: Boolean)         extends CompileEvent
case class NoCompile(ref: ModuleRef)                             extends CompileEvent
case class SkipCompile(ref: ModuleRef)                           extends CompileEvent
case class Print(ref: ModuleRef, line: String)                   extends CompileEvent
case class StartRun(ref: ModuleRef)                              extends CompileEvent
case class StopRun(ref: ModuleRef)                               extends CompileEvent
case class DiagnosticMsg(ref: ModuleRef, msg: DiagnosticMessage) extends CompileEvent

sealed trait CompileResult {
  def isSuccessful: Boolean
  def asTry: Try[CompileSuccess]
}

case class CompileSuccess(outputDirectories: Set[Path]) extends CompileResult {
  override def isSuccessful: Boolean = true
  override def asTry: Try[CompileSuccess] = Success(this)
}

case object CompileFailure extends CompileResult {
  override def isSuccessful: Boolean = false
  override def asTry: Try[CompileSuccess] = Failure(CompilationFailure())
}

case class Target(ref: ModuleRef,
                  schemaId: SchemaId,
                  kind: Kind,
                  main: Option[String],
                  plugin: Option[String],
                  repos: List[Repo],
                  checkouts: List[Checkout],
                  binaries: List[Path],
                  dependencies: List[TargetId],
                  compiler: Option[Target],
                  bloopSpec: Option[BloopSpec],
                  params: List[Parameter],
                  permissions: List[Permission],
                  intransitive: Boolean,
                  sourcePaths: List[Path],
                  environment: Map[String, String],
                  properties: Map[String, String]) {

  def id: TargetId = TargetId(schemaId, ref.projectId, ref.moduleId)
  def executed = kind == Application || kind == Benchmarks
}

object Project {
  implicit val msgShow: MsgShow[Project]       = v => UserMsg(_.project(v.id.key))
  implicit val stringShow: StringShow[Project] = _.id.key
  implicit def diff: Diff[Project]             = Diff.gen[Project]

  def available(projectId: ProjectId, layer: Layer): Boolean =
    !layer.projects.toOption.to[List].flatten.findBy(projectId).isSuccess
}

case class Project(id: ProjectId,
                   modules: SortedSet[Module] = TreeSet(),
                   main: Option[ModuleId] = None,
                   license: LicenseId = License.unknown,
                   description: String = "",
                   compiler: Option[ModuleRef] = None) {
  
  def apply(module: ModuleId): Try[Module] = modules.findBy(module)
  def moduleRefs: List[ModuleRef] = modules.to[List].map(_.ref(this))
  def mainModule: Try[Option[Module]] = main.map(modules.findBy(_)).to[List].sequence.map(_.headOption)

  def compilerRefs: List[ModuleRef] =
    modules.to[List].collect { case m if m.kind == Compiler => m.ref(this) }

  def unused(moduleId: ModuleId) = modules.findBy(moduleId) match {
    case Success(_) => Failure(ModuleAlreadyExists(moduleId))
    case _          => Success(moduleId)
  }

  def allRepoIds: Set[RepoId] = modules.flatMap(_.sources).collect { case ExternalSource(repoId, _) => repoId }
}

object Source {
  implicit val stringShow: StringShow[Source] = _.description
  implicit val ogdlReader: OgdlReader[Source] = src => unapply(src()).get // FIXME
  implicit val ogdlWriter: OgdlWriter[Source] = src => Ogdl(src.description)

  implicit val sourceDiff: Diff[Source] =
    (l, r) => if(l == r) Nil else List(Difference(msg"source", msg"", msg"$l", msg"$r"))

  implicit val msgShow: MsgShow[Source] = v => UserMsg { theme =>
    v match {
      case ExternalSource(repoId, path) =>
        msg"${theme.repo(repoId.key)}${theme.gray(":")}${theme.path(path.value)}".string(theme)
      case SharedSource(path) =>
        msg"${theme.repo("shared")}${theme.gray(":")}${theme.path(path.value)}".string(theme)
      case LocalSource(path) =>
        msg"${theme.path(path.value)}".string(theme)
    }
  }

  def unapply(string: String): Option[Source] = string match {
    case r"shared:$path@(.*)" =>
      Some(SharedSource(Path(path)))
    case r"$repo@([a-z][a-z0-9\.\-]*[a-z0-9]):$path@(.*)" =>
      Some(ExternalSource(RepoId(repo), Path(path)))
    case r"$path@(.*)" =>
      Some(LocalSource(Path(path)))
    case _ =>
      None
  }

  def repoId(src: Source): Option[RepoId] = src match {
    case ExternalSource(repoId, _) => Some(repoId)
    case _                         => None
  }
}

trait Source {
  def description: String
  def hash(schema: Schema, layout: Layout): Try[Digest]
  def path: Path
  def repoIdentifier: RepoId
}

case class ExternalSource(repoId: RepoId, path: Path) extends Source {
  def description: String = str"${repoId}:${path.value}"
  def hash(schema: Schema, layout: Layout): Try[Digest] = schema.repo(repoId, layout).map((path, _).digest[Md5])
  def repoIdentifier: RepoId = repoId
}

case class SharedSource(path: Path) extends Source {
  def description: String = str"shared:${path.value}"
  def hash(schema: Schema, layout: Layout): Try[Digest] = Success((-2, path).digest[Md5])
  def repoIdentifier: RepoId = RepoId("-")
}

case class LocalSource(path: Path) extends Source {
  def description: String = str"${path.value}"
  def hash(schema: Schema, layout: Layout): Try[Digest] = Success((-1, path).digest[Md5])
  def repoIdentifier: RepoId = RepoId("-")
}
