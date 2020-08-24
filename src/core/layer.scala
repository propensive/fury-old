/*

    Fury, version 0.18.3. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

import fury.model._, fury.io._, fury.text._, fury.ogdl._

import mercator._
import gastronomy._
import kaleidoscope._
import optometry._

import scala.util._
import scala.collection.immutable._
import guillotine._

import scala.collection.mutable.HashMap
import scala.annotation._

import java.io.ByteArrayInputStream

case class Layer(version: Int,
                 aliases: SortedSet[Alias] = TreeSet(),
                 projects: SortedSet[Project] = TreeSet(),
                 repos: SortedSet[Repo] = TreeSet(),
                 imports: SortedSet[Import] = TreeSet(),
                 main: Option[ProjectId] = None,
                 mainRepo: Option[RepoId] = None,
                 previous: Option[LayerRef] = None) { layer =>

  def apply(id: ProjectId) = projects.findBy(id)
  def moduleRefs: SortedSet[ModuleRef] = projects.flatMap(_.moduleRefs)
  def mainProject: Try[Option[Project]] = main.map(projects.findBy(_)).to[List].sequence.map(_.headOption)

  def checkoutSources(repoId: RepoId): Layer = copy(projects = projects.map { project =>
    project.copy(modules = project.modules.map { module =>
      module.copy(sources = module.sources.map {
        case RepoSource(`repoId`, dir, glob) => LocalSource(dir, glob)
        case other => other
      })
    })
  })

  def localUniverse(hierarchy: Hierarchy, path: Pointer): Universe = Universe(
    hierarchy = hierarchy,
    projects = projects.map { project => project.id -> Uniqueness.Unique(project.projectRef, Set(path)) }.toMap,
    repoSets = repos.groupBy(_.commit.repoSetId).mapValues(_.map(_.ref(path))),
    imports = imports.map { i => i.layerRef.short -> LayerProvenance(i.layerRef.short, Map(path -> i)) }.toMap
  )

  def checkinSources(repoId: RepoId): Layer = copy(projects = projects.map { project =>
    project.copy(modules = project.modules.map { module =>
      module.copy(sources = module.sources.map {
        case LocalSource(dir, glob) => RepoSource(repoId, dir, glob)
        case other => other
      })
    })
  })

  def localSources: List[ModuleRef] = for {
    project           <- projects.to[List]
    module            <- project.modules
    LocalSource(_, _) <- module.sources
  } yield module.ref(project)

  def deepModuleRefs(universe: Universe): Try[Set[ModuleRef]] =
    for(projects <- universe.allProjects) yield projects.flatMap(_.moduleRefs).to[Set]

  def unresolvedModules(universe: Universe): Map[ModuleRef, Set[Dependency]] = { for {
    project    <- projects.to[List]
    module     <- project.modules
    dependency <- module.dependencies
    missing    <- if(universe(dependency.ref).isSuccess) Nil else List((module.ref(project), dependency))
  } yield missing }.groupBy(_._1).mapValues(_.map(_._2).to[Set])

  def verifyConf(local: Boolean, conf: FuryConf, pointer: Pointer, quiet: Boolean, force: Boolean)
                (implicit log: Log)
                : Try[Unit] = for {
    _ <- if(force || conf.path == Pointer.Root) Success(()) else Failure(RootLayerNotSelected(conf.path))
    _ <- verify(force, local, pointer, quiet)
  } yield ()

  def verify(ignore: Boolean, local: Boolean, ref: Pointer, quiet: Boolean = false)
            (implicit log: Log)
            : Try[Unit] = if(ignore) Success(()) else for {
    _         <- ~log.infoWhen(!quiet)(msg"Checking that no modules reference local sources")
    localSrcs <- ~localSources
    _         <- if(localSrcs.isEmpty || local) Success(()) else Failure(LayerContainsLocalSources(localSrcs))
    _         <- ~log.infoWhen(!quiet)(msg"Checking that no project names conflict")
    universe  <- hierarchy(ref).flatMap(_.universe)
    _         <- ~log.infoWhen(!quiet)(msg"Checking that all module references resolve")
    missing   <- ~unresolvedModules(universe)
    _         <- if(missing.isEmpty) Success(()) else Failure(UnresolvedModules(missing))
  } yield ()

  def compilerRefs(layout: Layout)(implicit log: Log): List[ModuleRef] =
    allProjects(layout).toOption.to[List].flatMap(_.flatMap(_.compilerRefs))

  def hierarchy(pointer: Pointer = Pointer.Empty)(implicit log: Log): Try[Hierarchy] = for {
    imps <- imports.to[Set].traverse { ref =>
      Layer.get(ref.layerRef, ref.remote) >>= (_.hierarchy(pointer / ref.id).map(ref.id -> _))
    }
  } yield Hierarchy(this, pointer, imps.toMap)

  def resolvedImports(implicit log: Log): Try[Map[ImportId, Layer]] =
    imports.to[List].traverse { sr => Layer.get(sr.layerRef, sr.remote).map(sr.id -> _) }.map(_.toMap)

  def importedLayers(implicit log: Log): Try[List[Layer]] = resolvedImports.map(_.values.to[List])
  
  def importTree(implicit log: Log): Try[List[Pointer]] = for {
    imports <- resolvedImports
    imports <- imports.traverse { case (id, layer) => layer.importTree.map(_.map(_.prefix(id))) }.map(_.flatten)
  } yield Pointer.Root :: imports.to[List]

  def allProjects(layout: Layout)(implicit log: Log): Try[List[Project]] = {
    @tailrec
    def flatten[T](treeNodes: List[T])(aggregated: List[T], getChildren: T => Try[List[T]]): Try[List[T]] = {
      treeNodes match {
        case Nil => ~aggregated
        case head :: tail =>
          getChildren(head) match {
            case Success(ch) => flatten(ch ::: tail)(head :: aggregated, getChildren)
            case fail => fail
          }
      }
    }

    for {
      allLayers <- flatten(List(this))(Nil, _.importedLayers)
    } yield allLayers.flatMap(_.projects)
  }

  def localRepo(layout: Layout): Try[Repo] = for {
    repo   <- Remote.local(layout)
    gitDir <- ~GitDir(layout)
    commit <- gitDir.commit
    branch <- gitDir.branch
  } yield Repo(RepoId("~"), repo, branch, commit, Some(layout.baseDir))
}

object Layer extends Lens.Partial[Layer] {
  private val cache: HashMap[IpfsRef, Layer] = HashMap()
  private val dbCache: HashMap[Path, Long] = HashMap()
  private def lookup(ref: IpfsRef): Option[Layer] = cache.synchronized(cache.get(ref))
  implicit val stringShow: StringShow[Layer] = store(_)(Log()).toOption.fold("???")(_.key)

  val CurrentVersion: Int = 11

  def set[T](newValue: T)(layer: Layer, lens: Lens[Layer, T, T]): Layer = lens(layer) = newValue

  def retrieve(conf: FuryConf)(implicit log: Log): Try[Layer] = for {
    base  <- get(conf.layerRef, conf.published)
    layer <- dereference(base, conf.path)
  } yield layer

  def readDb(layout: Layout)(implicit log: Log): Try[Unit] =
    if(layout.layerDb.exists && Some(layout.layerDb.lastModified) != dbCache.get(layout.layerDb)) for {
      inputs <- TarGz.untargz(layout.layerDb.inputStream())
      _      =  log.note(msg"The layer storage at ${layout.layerDb} contains ${inputs.size} entries")
      _      <- inputs.traverse { bytes => for {
                  layer <- Ogdl.read[Layer](bytes, migrate(_))
                  _     <- store(layer)
                } yield () }
      _      <- Try(synchronized { dbCache(layout.layerDb) = layout.layerDb.lastModified })
    } yield ()
    else Success(())

  def dereference(layer: Layer, path: Pointer)(implicit log: Log): Try[Layer] =
    if(path.isEmpty) Success(layer)
    else for {
      layerImport <- layer.imports.findBy(path.head)
      layer       <- get(layerImport.layerRef, layerImport.remote)
      layer       <- dereference(layer, path.tail)
    } yield layer

  def get(layerRef: LayerRef, id: Option[PublishedLayer])(implicit log: Log): Try[Layer] =
    lookup(layerRef.ipfsRef).map(Success(_)).getOrElse { for {
      pub   <- ~id.fold(msg"Fetching layer $layerRef") { pl =>
                 msg"Fetching layer ${Pointer(pl.url.path)}${'@'}${pl.version} ${'('}$layerRef${')'}"
               }
      _ = log.info(pub)
      ipfs  <- Ipfs.daemon(false)
      data  <- ipfs.get(layerRef.ipfsRef)
      layer <- Ogdl.read[Layer](data, migrate(_))
      _     <- ~cache.synchronized { cache(layerRef.ipfsRef) = layer }
    } yield layer }

  def store(layer: Layer)(implicit log: Log): Try[LayerRef] = for {
    ipfs    <- Ipfs.daemon(false)
    ipfsRef <- ipfs.add(Ogdl.serialize(Ogdl(layer)))
  } yield {
    cache.synchronized { cache(ipfsRef) = layer }
    log.note(msg"Layer added to IPFS at $ipfsRef")
    LayerRef(ipfsRef.key)
  }

  def commit(layer: Layer, conf: FuryConf, layout: Layout, force: Boolean = false)
            (implicit log: Log)
            : Try[LayerRef] = for {
    baseRef  <- commitNested(conf, layer)
    base     <- get(baseRef, conf.published)
    layer    <- ~base.copy(previous = Some(conf.layerRef))
    previous <- retrieve(conf)
    
    ref      <- if(!Layer.diff(previous, layer).isEmpty || force) store(layer).flatMap { baseRef =>
                  saveFuryConf(conf.copy(layerRef = baseRef), layout).map(_.layerRef)
                } else Success(baseRef)
    } yield ref

  private def commitNested(conf: FuryConf, layer: Layer)(implicit log: Log): Try[LayerRef] =
    if(conf.path.isEmpty) store(layer) else for {
      ref     <- store(layer)
      parent  <- retrieve(conf.parent)
      pLayer   = Layer(_.imports(conf.path.last).layerRef)(parent) = ref
      baseRef <- commitNested(conf.parent, pLayer)
    } yield baseRef

  def hashes(layer: Layer)(implicit log: Log): Try[Set[LayerRef]] = for {
    layerRef  <- store(layer)
    layers    <- layer.imports.to[List].traverse { ref => Layer.get(ref.layerRef, ref.remote) }
    hashes    <- layers.traverse(hashes(_))
  } yield hashes.foldLeft(Set[LayerRef]())(_ ++ _) + layerRef

  def writeDb(layer: Layer, layout: Layout)(implicit log: Log): Try[Unit] = for {
    hashes  <- hashes(layer)
    entries <- hashes.to[List].traverse { ref => for {
                 layer <- lookup(ref.ipfsRef).ascribe(LayerNotFound(Path(ref.ipfsRef.key)))
                 bytes <- ~Ogdl.serialize(Ogdl(layer)).getBytes("UTF-8")
                 in    <- ~(new ByteArrayInputStream(bytes))
               } yield (ref.ipfsRef.key, bytes.length.toLong, in) }
    _       <- TarGz.store(entries, layout.layerDb)
  } yield ()

  def share(service: DomainName, layer: Layer, token: OauthToken)(implicit log: Log): Try[LayerRef] = for {
    ref    <- store(layer)
    hashes <- Layer.hashes(layer)
    _      <- Service.share(service, ref.ipfsRef, token, (hashes - ref).map(_.ipfsRef))
  } yield ref

  def published(layerName: LayerName, version: Option[LayerVersion] = None)(implicit log: Log): Try[Option[PublishedLayer]] = layerName match {
    case furyUri@FuryUri(domain, path) =>
      val artifact = version match {
        case Some(v) => Service.fetch(domain, path, v)
        case None => Service.latest(domain, path, None)
      }
      artifact.map( a => Some(PublishedLayer(furyUri, a.version, LayerRef(a.ref))))
    case _ => Success(None)
  }

  def resolve(layerInput: LayerName, version: Option[LayerVersion] = None)(implicit log: Log): Try[LayerRef] = layerInput match {
    case FileInput(path)       => ???
    case FuryUri(domain, path) =>
      val artifact = version match {
        case Some(v) => Service.fetch(domain, path, v)
        case None => Service.latest(domain, path, None)
      }
      artifact.map { a => LayerRef(a.ref) }
    case IpfsRef(key)          => Success(LayerRef(key))
  }

  def pathCompletions()(implicit log: Log): Try[List[String]] = Service.catalog(ManagedConfig().service)

  def readFuryConf(layout: Layout)(implicit log: Log): Try[FuryConf] =
    layout.confFile.lines().flatMap { lines =>
      if(lines.contains("=======")) Failure(MergeConflicts())
      else for {
        _    <- readDb(layout)
        conf <- ~Ogdl.read[FuryConf](layout.confFile, identity(_))
      } yield conf
    }.flatten
  
  def showMergeConflicts(layout: Layout)(implicit log: Log) = for {
    gitDir        <- ~GitDir(layout)
    (left, right) <- gitDir.mergeConflicts
    common        <- gitDir.mergeBase(left, right)
    _             <- ~log.warn(msg"The layer has merge conflicts")
    leftSrc       <- gitDir.cat(left, path".fury/config")
    commonSrc     <- gitDir.cat(common, path".fury/config")
    rightSrc      <- gitDir.cat(right, path".fury/config")
    leftConf      <- Ogdl.read[FuryConf](leftSrc, identity(_))
    commonConf    <- Ogdl.read[FuryConf](commonSrc, identity(_))
    rightConf     <- Ogdl.read[FuryConf](rightSrc, identity(_))
    leftLayer     <- Layer.get(leftConf.layerRef, leftConf.published)
    commonLayer   <- Layer.get(commonConf.layerRef, commonConf.published)
    rightLayer    <- Layer.get(rightConf.layerRef, rightConf.published)
    leftDiff      <- ~Layer.diff(commonLayer, leftLayer)
    rightDiff     <- ~Layer.diff(commonLayer, rightLayer)
    leftMsg       <- gitDir.logMessage(left)
    rightMsg       <- gitDir.logMessage(right)
    _             <- ~log.info(msg"Changes since Git commit $left ($leftMsg):")
    leftTable     <- ~Tables().differences("Base", str"${leftConf.layerRef}")
    leftTable     <- ~Tables().show[Difference, Difference](leftTable, 100, leftDiff, false)
    _             <- ~log.rawln(leftTable)
    _             <- ~log.info(msg"Changes since Git commit $right ($rightMsg):")
    rightTable    <- ~Tables().differences("Base", str"${rightConf.layerRef}")
    rightTable    <- ~Tables().show[Difference, Difference](rightTable, 100, rightDiff, false)
    _             <- ~log.rawln(rightTable)
  } yield ()

  def diff(left: Layer, right: Layer): List[Difference] =
    Diff.gen[Layer].diff(left.copy(previous = None), right.copy(previous = None)).to[List]
  
  def init(layout: Layout)(implicit log: Log): Try[Unit] =
    if(layout.confFile.exists) { for {
      conf     <- readFuryConf(layout)
      layer    <- Layer.get(conf.layerRef, conf.published)
      _        <- ~log.info(msg"Reinitialized layer ${conf.layerRef}")
    } yield () } else { for {
      _        <- layout.confFile.mkParents()
      ref      <- store(Layer(CurrentVersion))
      conf     <- saveFuryConf(FuryConf(ref), layout)
      _        <- ~log.info(msg"Initialized an empty layer")
    } yield () }

  private final val confComments: String =
    str"""# This is a Fury configuration file. It contains significant
         |# whitespace and is not intended to be human-editable.
         |#
         |# To start using Fury with this project, install Fury and run,
         |#
         |#   fury layer init
         |#
         |# For more information, please visit https://propensive.com/fury/
         |#
         |""".stripMargin

  private final val vimModeline: String =
    str"""# vim: set noai ts=12 sw=12:
         |""".stripMargin

  def saveFuryConf(conf: FuryConf, layout: Layout)(implicit log: Log): Try[FuryConf] = for {
    confStr  <- ~Ogdl.serialize(Ogdl(conf))
    _        <- layout.confFile.writeSync(confComments+confStr+vimModeline)
  } yield conf

  private def migrateModules(root: Ogdl)(fn: Ogdl => Ogdl): Ogdl =
    migrateProjects(root) { project =>
      if(project.has("modules")) project.set(modules = project.modules.map(fn(_))) else project
    }

  private def migrateRepos(root: Ogdl)(fn: Ogdl => Ogdl): Ogdl =
    if(root.has("repos")) root.set(repos = root.repos.map(fn(_))) else root
  
  private def migrateProjects(root: Ogdl)(fn: Ogdl => Ogdl): Ogdl =
    if(root.has("projects")) root.set(projects = root.projects.map(fn(_))) else root

  private def migrate(ogdl: Ogdl)(implicit log: Log): Ogdl = {
    val version = Try(ogdl.version().toInt).getOrElse(1)
    if(version < CurrentVersion) {
      log.note(msg"Migrating layer file from version $version to ${version + 1}")
      migrate((version match {
        case 10 =>
          val step1 = migrateModules(ogdl) { module =>
            log.note(msg"Old module = $module")
            val newValue = Ogdl[CompilerRef] {
              if(!module.has("compiler")) Javac(8) else BspCompiler(ModuleRef(module.compiler.id()))
            }
            log.note(msg"Updated compiler reference for module ${module} to $newValue")
            module.set(compiler = newValue)
          }

          val step2 = migrateProjects(step1) { project =>
            val newProject = project.set(compiler = Ogdl[Option[CompilerRef]] {
              if(project.has("compiler")) Some(BspCompiler(ModuleRef(project.compiler.Some.value.id())))
              else None
            })

            log.note(msg"Updated project compiler reference for project ${project} to ${newProject}")
            newProject
          }

          migrateModules(step2) { module =>
            if(module.has("dependencies")) module.set(dependencies = Ogdl(module.dependencies.*.map { d =>
              Dependency(ModuleRef(d))
            }.to[SortedSet])) else module
          }

        case 9 =>
          migrateModules(ogdl) { module =>
            if(!module.has("kind")) module else module.kind() match {
              case "Compiler" =>
                val c = module.kind.Compiler
                log.note(msg"Updated compiler type for module ${module}")
                module.set(kind = Ogdl[Kind](Compiler(BloopSpec(c.spec.org(), c.spec.name(), c.spec.version()))))

              case "App" =>
                log.note(msg"Updated app type for module ${module}")
                module.set(kind = Ogdl[Kind](App(ClassRef(module.kind.App()), 0)))
              
              case other =>
                module
            } 
          }

        case 8 => // Expires 19 November 2020
          migrateModules(ogdl) { module =>
            lazy val main = Try(ClassRef(module.main.Some()))
            lazy val plugin = Try(PluginId(module.plugin.Some()))
            
            lazy val spec = Try(BloopSpec(module.bloopSpec.Some.org(), module.bloopSpec.Some.name(),
                module.bloopSpec.Some.version()))
            
            log.note(msg"Updated module type for module ${module}")
            if(!module.has("kind")) module else module.set(kind = (module.kind() match {
              case "Application" => Ogdl[Kind](App(main.get, 0))
              case "Plugin"      => Ogdl[Kind](Plugin(plugin.get, main.get))
              case "Benchmarks"  => Ogdl[Kind](Bench(main.get))
              case "Compiler"    => Ogdl[Kind](Compiler(spec.get))
              case _             => Ogdl[Kind](Lib())
            }))
          }
          
        case 7 =>
          migrateRepos(ogdl) { repo =>
            log.note(msg"Renamed repo to remote in repo ${repo}")
            repo.set(remote = repo.repo)
          }
          
        case 6 =>
          migrateRepos(ogdl) { repo =>
            log.note(msg"Renamed branch to track for repo ${repo}")
            repo.set(branch = repo.track, remote = repo.repo)
          }
          
        case 0 | 1 | 2 | 3 | 4 | 5 =>
          log.fail(msg"Cannot migrate from layers earlier than version 6")
          // FIXME: Handle this better
          throw new Exception()
          
        case _ => Ogdl(Vector())
      }).set(version = Ogdl(version + 1)))
    } else ogdl
  }
}
