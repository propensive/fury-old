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

import java.net.URI

import ch.epfl.scala.bsp4j.{CompileResult => BspCompileResult, _}
import exoskeleton._
import fury._
import fury.core.Graph.DiagnosticMessage
import fury.io._
import fury.model._
import fury.ogdl._
import fury.strings._
import fury.utils._
import gastronomy._
import guillotine._
import kaleidoscope._
import mercator._

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.immutable.{SortedSet, TreeSet}
import scala.collection.mutable.HashMap
import scala.language.higherKinds
import scala.util._

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

  /**
    * Filters a set of Binaries by id, allowing full and partial matches:
    *  - group:artifact:version
    *  - artifact:version
    *  - group:artifact
    *  - artifact
    */
  def filterByPartialId(binaries: SortedSet[Binary], id: String): List[Binary] =
    binaries.filter(bin => id match {
      case r"([\.\-_a-zA-Z0-9]*)\:([\.\-_a-zA-Z0-9]*)\:([\.\-\+_a-zA-Z0-9]*)" =>
        id == bin.spec
      case r"([\.\-_a-zA-Z0-9]*)\:([\.\-\+_a-zA-Z0-9]*)" =>
        id == str"${bin.group}:${bin.artifact}" || id == str"${bin.artifact}:${bin.version}"
      case _ =>
        id == bin.artifact
    }).toList
}

case class Binary(binRepo: BinRepoId, group: String, artifact: String, version: String) {
  def spec = str"$group:$artifact:$version"
  def paths(implicit log: Log): Try[List[Path]] = Coursier.fetch(this)
}

object Policy {
  def read(implicit log: Log): Policy =
    Ogdl.read[Policy](Installation.policyFile,
        upgrade(_)).toOption.getOrElse(Policy(SortedSet.empty[Grant]))

  def save(policy: Policy)(implicit log: Log): Try[Unit] =
    Installation.policyFile.extantParents().writeSync(Ogdl.serialize(Ogdl(policy)))

  private def upgrade(ogdl: Ogdl)(implicit log: Log): Ogdl = ogdl
}

case class Policy(policy: SortedSet[Grant] = TreeSet()) {
  def forContext(layout: Layout, projectId: ProjectId/*, layer: Layer*/): Policy =
    Policy(policy.filter {
      case Grant(DirectoryScope(dir), _) => dir == layout.baseDir.value
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
                  policy: SortedSet[Permission] = TreeSet(),
                  hidden: Boolean = false) {

  def allBinaries: SortedSet[Binary] = if(kind == Benchmarks) binaries + Binary.Jmh else binaries
  def compilerDependencies: Set[ModuleRef] = Set(compiler).filter(_ != ModuleRef.JavaRef).map(_.hide)
  def ref(project: Project): ModuleRef = ModuleRef(project.id, id, hidden = hidden)
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

case class Entity(project: Project, schema: Schema) {
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

  def makeTarget(ref: ModuleRef, layout: Layout)(implicit log: Log): Try[Target] =
    for {
      resolvedProject <- entity(ref.projectId)
      module          <- resolvedProject.project(ref.moduleId)
      compiler        <- if(module.compiler == ModuleRef.JavaRef) Success(None)
                         else makeTarget(module.compiler, layout).map(Some(_))
      binaries        <- module.allBinaries.map(_.paths).sequence.map(_.flatten)
      dependencies    <- module.after.traverse { dep => for{
                           origin <- entity(dep.projectId)
                         } yield TargetId(origin.schema.id, dep)}
      checkouts       <- checkout(ref, layout)
    } yield {
      // FIXME: This assumes published layers will not have local sources
      // Previously this was (_ in resolvedProject.path)
      val sourcePaths = module.localSources.map(_ in layout.baseDir).to[List] ++
        module.sharedSources.map(_.path in layout.sharedDir).to[List] ++
        checkouts.flatMap { c =>
          c.local match {
            case Some(p) => c.sources.map(_ in p)
            case None    => c.sources.map(_ in c.path)
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

  private[fury] def dependencies(ref: ModuleRef, layout: Layout): Try[Set[ModuleRef]] =
    resolveTransitiveDependencies(forbidden = Set.empty, ref, layout)

  private[this] def resolveTransitiveDependencies(forbidden: Set[ModuleRef], ref: ModuleRef, layout: Layout): Try[Set[ModuleRef]] =
    for {
      entity   <- entity(ref.projectId)
      module   <- entity.project(ref.moduleId)
      deps     =  module.after ++ module.compilerDependencies
      repeated =  deps.intersect(forbidden)
      _        <- if (repeated.isEmpty) ~() else Failure(CyclesInDependencies(repeated))
      tDeps    <- deps.map(resolveTransitiveDependencies(forbidden + ref, _, layout).filter(!_.contains(ref))).sequence
    } yield deps ++ tDeps.flatten

  def clean(ref: ModuleRef, layout: Layout): Unit = layout.classesDir.delete().unit

  def getMod(ref: ModuleRef): Try[Module] = for {
    entity <- entity(ref.projectId)
    module <- entity.project(ref.moduleId)
  } yield module
}

case class Hierarchy(schema: Schema, inherited: Set[Hierarchy]) {
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
      val schemaEntities = schema.projects.map { project => project.id -> Entity(project, schema) }
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

  def compilerRefs(layout: Layout, https: Boolean)(implicit log: Log): List[ModuleRef] =
    allProjects(layout, https).toOption.to[List].flatMap(_.flatMap(_.compilerRefs))

  def importCandidates(layout: Layout, https: Boolean)(implicit log: Log): List[String] =
    repos.to[List].flatMap(_.importCandidates(this, layout, https).toOption.to[List].flatten)

  def hierarchy(layout: Layout, https: Boolean)(implicit log: Log): Try[Hierarchy] = for {
    imps <- imports.map { ref => for {
      layer        <- Layer.read(ref.layerRef, layout)
      resolved     <- layer.schemas.findBy(ref.schema)
      tree         <- resolved.hierarchy(layout, https)
    } yield tree }.sequence
  } yield Hierarchy(this, imps)

  def resolvedImports(layout: Layout, https: Boolean)(implicit log: Log): Try[Map[ImportId, Schema]] =
    imports.to[List].map { sr => resolve(sr, layout, https).map(sr.id -> _) }.sequence.map(_.toMap)

  def importedSchemas(layout: Layout, https: Boolean)(implicit log: Log): Try[List[Schema]] =
    resolvedImports(layout, https).map(_.values.to[List])
  
  def importTree(layout: Layout, https: Boolean)(implicit log: Log): Try[List[ImportPath]] = for {
    imports    <- resolvedImports(layout, https)
    importList <- imports.to[List].map { case (id, schema) =>
                    schema.importTree(layout, https).map { is => is.map(_.prefix(id)) }
                  }.sequence.map(_.flatten)
  } yield (ImportPath.Root :: importList)

  def allProjects(layout: Layout, https: Boolean)(implicit log: Log): Try[List[Project]] = {
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
      allSchemas <- flatten(List(this))(Nil, _.importedSchemas(layout, https))
    } yield allSchemas.flatMap(_.projects)
  }

  def unused(projectId: ProjectId): Try[ProjectId] = projects.find(_.id == projectId) match {
    case None    => Success(projectId)
    case Some(m) => Failure(ProjectAlreadyExists(m.id))
  }
  
  def resolve(ref: SchemaRef, layout: Layout, https: Boolean)(implicit log: Log): Try[Schema] = for {
    layer    <- Layer.read(ref.layerRef, layout)
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
  val CurrentVersion = 4

  def loadFromIpfs(layerRef: IpfsRef, env: Environment)(implicit log: Log): Try[LayerRef] =
    Installation.tmpFile { tmpFile => for {
      file     <- Shell(env).ipfs.get(layerRef, tmpFile)
      layer    <- Layer.read(file, env)
      layerRef <- saveLayer(layer)
    } yield layerRef }

  def loadFile(file: Path, layout: Layout, env: Environment)(implicit log: Log): Try[LayerRef] =
    Installation.tmpDir { tmpDir => for {
      _      <- TarGz.extract(file, tmpDir)
      _      <- (tmpDir / "layers").childPaths.map { f => f.moveTo(Installation.layersPath / f.name) }.sequence
      bases  <- ~(tmpDir / "bases").childPaths
      _      <- bases.map { b => b.moveTo(layout.basesDir / b.name)}.sequence
      focus  <- Ogdl.read[Focus](tmpDir / ".focus.fury", identity(_))
    } yield focus.layerRef }

    def share(layer: Layer, env: Environment)(implicit log: Log): Try[IpfsRef] = for {
      layerRef <- ~digestLayer(layer)
      file     <- ~(Installation.layersPath / layerRef.key)
      _        <- file.writeSync(Ogdl.serialize(Ogdl(layer)))
      ref      <- Shell(env).ipfs.add(file)
    } yield ref


  def loadCatalog(catalogRef: IpfsRef, env: Environment)(implicit log: Log): Try[Catalog] =
    Installation.tmpFile { tmpFile => for {
      file     <- Shell(env).ipfs.get(catalogRef, tmpFile)
      catalog  <- Ogdl.read[Catalog](tmpFile, identity(_))
    } yield catalog
  }

  def lookup(domain: String, env: Environment)(implicit log: Log): Try[List[Artifact]] = for {
    records   <- Dns.lookup(domain)
    records   <- ~records.filter(_.startsWith("fury:")).map { rec => IpfsRef(rec.drop(5)) }
    catalogs  <- records.map { loadCatalog(_, env) }.sequence
    artifacts <- ~catalogs.flatMap(_.artifacts)
  } yield artifacts
 
  def follow(importLayer: ImportLayer): Option[Followable] = importLayer match {
    case IpfsImport(hash) => None
    case RefImport(followable) => Some(followable)
    case DefaultImport(path) => Some(Followable(ManagedConfig().service, path))
  }

  def resolve(followable: Followable, env: Environment)(implicit log: Log): Try[LayerRef] = for {
    artifacts <- lookup(followable.domain, env)
    ref       <- Try(artifacts.find(_.path == followable.path).get)
    layerRef  <- loadFromIpfs(ref.ref, env)
  } yield layerRef

  def pathCompletions(domain: String, env: Environment)(implicit log: Log): Try[List[String]] =
    lookup(domain, env).map(_.map(_.path))

  def read(string: String, env: Environment)(implicit log: Log): Try[Layer] =
    Success(Ogdl.read[Layer](string, upgrade(env, _)))

  def read(path: Path, env: Environment)(implicit log: Log): Try[Layer] =
    Ogdl.read[Layer](path, upgrade(env, _))

  def readFocus(layout: Layout)(implicit log: Log): Try[Focus] =
    Ogdl.read[Focus](layout.focusFile, identity(_))

  private def collectLayerRefs(ref: SchemaRef, layout: Layout)(implicit log: Log): Try[Set[LayerRef]] = for {
    layer   <- read(ref.layerRef, layout)
    schema  <- layer.schemas.findBy(ref.schema)
    imports <- schema.imports.map(collectLayerRefs(_, layout)).sequence.map(_.flatten)
  } yield imports + ref.layerRef

  def export(layer: Layer, layout: Layout, path: Path)(implicit log: Log): Try[Path] = for {
    layerRef  <- ~digestLayer(layer)
    schemaRef <- ~SchemaRef(ImportId(""), layerRef, layer.main)
    layerRefs <- collectLayerRefs(schemaRef, layout)
    filesMap  <- ~layerRefs.map { ref => (Path(str"layers/${ref}"), Installation.layersPath / ref.key) }.toMap
    // include bases
    _         <- TarGz.store(filesMap.updated(Path(".focus.fury"), layout.focusFile), path)
  } yield path

  def base(layout: Layout)(implicit log: Log): Try[Layer] = for {
    focus    <- readFocus(layout)
    layer    <- read(focus.layerRef, layout)
  } yield layer

  def read(layout: Layout)(implicit log: Log): Try[Layer] = for {
    focus    <- readFocus(layout)
    layer    <- read(focus.layerRef, layout)
    newLayer <- resolveSchema(layout, layer, focus.path)
  } yield newLayer

  def read(ref: LayerRef, layout: Layout)(implicit log: Log): Try[Layer] =
    Ogdl.read[Layer](Installation.layersPath / ref.key, upgrade(layout.env, _))

  def resolveSchema(layout: Layout, layer: Layer, path: ImportPath)(implicit log: Log): Try[Layer] =
    path.parts.foldLeft(Try(layer)) { case (current, importId) => for {
      layer     <- current
      schema    <- layer.mainSchema
      schemaRef <- schema.imports.findBy(importId)
      layer     <- read(schemaRef.layerRef, layout)
    } yield layer.copy(main = schemaRef.schema) }

  def digestLayer(layer: Layer): LayerRef =
    LayerRef(Ogdl.serialize(Ogdl(layer)).digest[Sha256].encoded[Hex])

  def create(newLayer: Layer, layout: Layout)(implicit log: Log): Try[LayerRef] = for {
    layerRef     <- saveLayer(newLayer)
    _            <- saveFocus(Focus(layerRef), layout)
  } yield layerRef

  def save(newLayer: Layer, layout: Layout)(implicit log: Log): Try[LayerRef] = for {
    focus        <- readFocus(layout)
    currentLayer <- read(focus.layerRef, layout)
    layerRef     <- saveSchema(layout, newLayer, focus.path, currentLayer)
    _            <- saveFocus(focus.copy(layerRef = layerRef), layout)
  } yield layerRef

  private def saveSchema(layout: Layout, newLayer: Layer, path: ImportPath, currentLayer: Layer)(implicit log: Log): Try[LayerRef] =
    if(path.isEmpty) saveLayer(newLayer)
    else for {
      schema    <- currentLayer.mainSchema
      schemaRef <- schema.imports.findBy(path.head)
      nextLayer <- read(schemaRef.layerRef, layout)
      layerRef  <- saveSchema(layout, newLayer, path.tail, nextLayer)
      newSchema <- ~schema.copy(imports = schema.imports.filter(_.id != path.head) + schemaRef.copy(layerRef = layerRef))
      newLayer  <- ~currentLayer.copy(schemas = currentLayer.schemas.filter(_.id != currentLayer.main) + newSchema)
      newLayerRef <- saveLayer(newLayer)
    } yield newLayerRef

  private def saveLayer(layer: Layer): Try[LayerRef] = for {
    layerRef <- ~digestLayer(layer)
    _        <- (Installation.layersPath / layerRef.key).writeSync(Ogdl.serialize(Ogdl(layer)))
  } yield layerRef

  def saveFocus(focus: Focus, layout: Layout)(implicit log: Log): Try[Unit] =
    saveFocus(focus, layout.focusFile)

  def saveFocus(focus: Focus, path: Path)(implicit log: Log): Try[Unit] = for {
    focusStr <- ~Ogdl.serialize(Ogdl(focus))
    _        <- path.writeSync(focusStr)
  } yield ()

  private def upgrade(env: Environment, ogdl: Ogdl)(implicit log: Log): Ogdl =
    Try(ogdl.version().toInt).getOrElse(1) match {
      case 1 =>
        log.info("Migrating layer file from version 1")
        upgrade(
            env,
            ogdl.set(
                schemas = ogdl.schemas.map { schema =>
                  schema.set(
                      repos = schema.repos.map { repo =>
                        log.info(msg"Checking commit hash for ${repo.repo()}")
                        val commit =
                          Shell(env).git.lsRemoteRefSpec(repo.repo(), repo.refSpec()).toOption.get
                        repo.set(commit = Ogdl(Commit(commit)), track = repo.refSpec)
                      }
                  )
                },
                version = Ogdl(2)
            )
        )
      case 2 =>
        log.info("Migrating layer file from version 2")
        upgrade(
            env,
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
      case 3 =>
        log.info("Migrating layer file from version 2")
        upgrade(
            env,
            ogdl.set(
                schemas = ogdl.schemas.map { schema =>
                  schema.set(
                      imports = schema.imports.map { imp =>
                        imp.set(id = Ogdl(s"unknown-${Counter.next()}"))
                      }
                  )
                },
                version = Ogdl(4)
            )
        )

      case CurrentVersion => ogdl
    }
}

object Counter {
  private var count: Int = 0
  def next(): Int = {
    count += 1
    count
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
  def path: Path = Installation.srcsDir / hash.encoded

  def get(layout: Layout, https: Boolean)(implicit log: Log): Try[Path] = for {
    repoDir    <- repo.fetch(layout, https)
    workingDir <- checkout(layout)
  } yield workingDir

  private def checkout(layout: Layout)(implicit log: Log): Try[Path] =
    local.map(Success(_)).getOrElse {
      if(!(path / ".done").exists) {
        if(path.exists()) {
          val sourceText = if(sources.isEmpty) "all sources" else sources.map(_.value).mkString("[", ", ", "]")
          log.info(msg"Found incomplete checkout of $sourceText")
          path.delete()
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

        log.info(msg"Checking out $sourceDesc from repository $repoId")
        path.mkdir()
        Shell(layout.env).git
          .sparseCheckout(repo.path(layout), path, sources, refSpec = refSpec.id, commit = commit.id)
          .map(path.waive)
      } else Success(path)
    }
}

object SourceRepo {
  implicit val msgShow: MsgShow[SourceRepo] = r => UserMsg(_.repo(r.id.key))
  implicit val stringShow: StringShow[SourceRepo] = _.id.key
  implicit def diff: Diff[SourceRepo] = Diff.gen[SourceRepo]
}

case class SourceRepo(id: RepoId, repo: Repo, track: RefSpec, commit: Commit, local: Option[Path]) {
  def listFiles(layout: Layout, https: Boolean)(implicit log: Log): Try[List[Path]] = for {
    dir    <- local.map(Success(_)).getOrElse(repo.fetch(layout, https))
    commit <- ~Shell(layout.env).git.getTag(dir, track.id).toOption.orElse(Shell(layout.env).git.getBranchHead(dir,
                  track.id).toOption).getOrElse(track.id)
    files  <- local.map(Success(dir.children.map(Path(_))).waive).getOrElse(Shell(layout.env).git.lsTree(dir,
                  commit))
  } yield files

  def fullCheckout: Checkout = Checkout(id, repo, local, commit, track, List())

  def importCandidates(schema: Schema, layout: Layout, https: Boolean)(implicit log: Log): Try[List[String]] = for {
    repoDir     <- repo.fetch(layout, https)
    focusString <- Shell(layout.env).git.showFile(repoDir, ".focus.fury")
    focus       <- ~Ogdl.read[Focus](focusString, identity(_))
    layer       <- Layer.read(focus.layerRef, layout)
    schemas     <- ~layer.schemas.to[List]
  } yield schemas.map { schema => str"${id.key}:${schema.id.key}" }

  def current(layout: Layout, https: Boolean)(implicit log: Log): Try[RefSpec] = for {
    dir    <- local.map(Success(_)).getOrElse(repo.fetch(layout, https))
    commit <- Shell(layout.env).git.getCommit(dir)
  } yield RefSpec(commit)

  def sourceCandidates(layout: Layout, https: Boolean)(pred: String => Boolean)(implicit log: Log): Try[Set[Source]] =
    listFiles(layout, https).map(_.filter { f => pred(f.filename) }.map { p =>
        ExternalSource(id, p.parent): Source }.to[Set])
}

case class Repo(ref: String) {
  def hash: Digest               = ref.digest[Md5]
  def path(layout: Layout): Path = Installation.reposDir / hash.encoded

  def update(layout: Layout): Try[UserMsg] = for {
    oldCommit <- Shell(layout.env).git.getCommit(path(layout)).map(Commit(_))
    _         <- Shell(layout.env).git.fetch(path(layout), None)
    newCommit <- Shell(layout.env).git.getCommit(path(layout)).map(Commit(_))
    msg <- ~(if(oldCommit != newCommit) msg"Repository $this updated to new commit $newCommit"
              else msg"Repository $this is unchanged")
  } yield msg

  def getCommitFromTag(layout: Layout, tag: RefSpec): Try[Commit] =
    for(commit <- Shell(layout.env).git.getCommitFromTag(path(layout), tag.id)) yield Commit(commit)

  def fetch(layout: Layout, https: Boolean)(implicit log: Log): Try[Path] =
    if(!(path(layout) / ".done").exists) {
      if(path(layout).exists()) {
        log.info(msg"Found incomplete clone of $this at ${path(layout)}")
        path(layout).delete()
      }

      log.info(msg"Cloning repository at $this")
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

case class CompileResult(bspCompileResult: BspCompileResult, scalacOptions: ScalacOptionsResult) {
  def isSuccessful: Boolean = bspCompileResult.getStatusCode == StatusCode.OK
  def classDirectories: Set[Path] = scalacOptions.getItems.asScala.toSet.map { x: ScalacOptionsItem =>
    Path(new URI(x.getClassDirectory))
  }
  def asTry: Try[CompileResult] = if(isSuccessful) Success(this) else Failure(CompilationFailure())
  def failed: CompileResult = {
    val updatedResult = new BspCompileResult(StatusCode.ERROR)
    updatedResult.setOriginId(bspCompileResult.getOriginId)
    updatedResult.setDataKind(bspCompileResult.getDataKind)
    updatedResult.setData(bspCompileResult.getData)
    copy(bspCompileResult = updatedResult)
  }
}

object CompileResult {
  def merge(results: Iterable[CompileResult]): CompileResult = {
    CompileResult(merge(results.map(_.bspCompileResult)), merge(results.map(_.scalacOptions)))
  }

  private def merge(results: Iterable[BspCompileResult]): BspCompileResult = {
    val distinctStatuses = results.map(_.getStatusCode).toSet
    val aggregatedStatus = List(StatusCode.CANCELLED, StatusCode.ERROR, StatusCode.OK).find(distinctStatuses.contains)
    val mergedResult = new BspCompileResult(aggregatedStatus.getOrElse(StatusCode.OK))
    results.headOption.foreach { res =>
      //TODO think of a better way to merge those fields
      mergedResult.setOriginId(res.getOriginId)
      mergedResult.setDataKind(res.getDataKind)
      mergedResult.setData(res.getData)
    }
    mergedResult
  }

  private def merge(results: Iterable[ScalacOptionsResult]): ScalacOptionsResult = {
    new ScalacOptionsResult(results.flatMap(_.getItems.asScala).toList.asJava)
  }
}

object Target {
  type Graph = Map[TargetId, Set[TargetId]]
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

object ManagedConfig {
  private var config: Config =
    Ogdl.read[Config](Installation.userConfig, identity(_)).toOption.getOrElse(Config())

  def write(newConfig: Config) = synchronized {
    config = newConfig
    Ogdl.write(config, Installation.userConfig)
  }

  def apply(): Config = config
}

