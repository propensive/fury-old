/*

    Fury, version 0.12.3. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

import fury.model._, fury.io._, fury.strings._, fury.ogdl._

import mercator._
import gastronomy._
import kaleidoscope._
import optometry._

import scala.util._
import scala.collection.immutable._
import guillotine._

import scala.collection.mutable.HashMap
import scala.annotation._

case class Layer(version: Int,
                 aliases: SortedSet[Alias] = TreeSet(),
                 projects: SortedSet[Project] = TreeSet(),
                 repos: SortedSet[SourceRepo] = TreeSet(),
                 imports: SortedSet[Import] = TreeSet(),
                 main: Option[ProjectId] = None,
                 mainRepo: Option[RepoId] = None,
                 previous: Option[LayerRef] = None) { layer =>

  def apply(id: ProjectId) = projects.findBy(id)
  def repo(repoId: RepoId, layout: Layout): Try[SourceRepo] = repos.findBy(repoId)
  def moduleRefs: SortedSet[ModuleRef] = projects.flatMap(_.moduleRefs)
  def mainProject: Try[Option[Project]] = main.map(projects.findBy(_)).to[List].sequence.map(_.headOption)
  def sourceRepoIds: SortedSet[RepoId] = repos.map(_.id)

  def localSources: List[ModuleRef] = for {
    project           <- projects.to[List]
    module            <- project.modules
    LocalSource(_, _) <- module.sources
  } yield module.ref(project)

  def deepModuleRefs(universe: Universe): Set[ModuleRef] =
    universe.entities.values.flatMap(_.project.moduleRefs).to[Set]

  def unresolvedModules(universe: Universe): Map[ModuleRef, Set[ModuleRef]] = { for {
    project    <- projects.to[List]
    module     <- project.modules
    dependency <- module.dependencies
    missing    <- if(universe.getMod(dependency).isSuccess) Nil else List((module.ref(project), dependency))
  } yield missing }.groupBy(_._1).mapValues(_.map(_._2).to[Set])

  def verify(conf: FuryConf)(implicit log: Log): Try[Unit] = for {
    _         <- ~log.info(msg"Checking that the root layer is selected")
    _         <- if(conf.path == ImportPath.Root) Success(()) else Failure(RootLayerNotSelected())
    _         <- ~log.info(msg"Checking that no modules reference local sources")
    localSrcs <- ~localSources
    _         <- if(localSrcs.isEmpty) Success(()) else Failure(LayerContainsLocalSources(localSrcs))
    _         <- ~log.info(msg"Checking that no project names conflict")
    universe  <- hierarchy().flatMap(_.universe)
    _         <- ~log.info(msg"Checking that all module references resolve")
    missing   <- ~unresolvedModules(universe)
    _         <- if(missing.isEmpty) Success(()) else Failure(UnresolvedModules(missing))
  } yield ()

  def compilerRefs(layout: Layout, https: Boolean)(implicit log: Log): List[ModuleRef] =
    allProjects(layout, https).toOption.to[List].flatMap(_.flatMap(_.compilerRefs))

  def hierarchy()(implicit log: Log): Try[Hierarchy] = for {
    imps <- imports.map { ref => for {
      layer        <- Layer.get(ref.layerRef)
      tree         <- layer.hierarchy()
    } yield tree }.sequence
  } yield Hierarchy(this, imps)

  def resolvedImports(implicit log: Log): Try[Map[ImportId, Layer]] =
    imports.to[List].map { sr => Layer.get(sr.layerRef).map(sr.id -> _) }.sequence.map(_.toMap)

  def importedLayers(implicit log: Log): Try[List[Layer]] = resolvedImports.map(_.values.to[List])
  
  def importTree(implicit log: Log): Try[List[ImportPath]] = for {
    imports    <- resolvedImports
    importList <- imports.to[List].map { case (id, layer) =>
                    layer.importTree.map { is => is.map(_.prefix(id)) }
                  }.sequence.map(_.flatten)
  } yield (ImportPath.Root :: importList)

  def allProjects(layout: Layout, https: Boolean)(implicit log: Log): Try[List[Project]] = {
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

  def localRepo(layout: Layout): Try[SourceRepo] = for {
    repo   <- Repo.local(layout)
    commit <- Shell(layout.env).git.getCommit(layout.baseDir)
    branch <- Shell(layout.env).git.getBranch(layout.baseDir).map(RefSpec(_))
  } yield SourceRepo(RepoId("~"), repo, branch, commit, Some(layout.baseDir))

  def allRepos(layout: Layout): SortedSet[SourceRepo] =
    (localRepo(layout).toOption.to[SortedSet].filterNot { r =>
      repos.map(_.repo.simplified).contains(r.repo.simplified)
    }) ++ repos

}

object Layer extends Lens.Partial[Layer] {
  private val cache: HashMap[IpfsRef, Layer] = HashMap()
  private def lookup(ref: IpfsRef): Option[Layer] = cache.synchronized(cache.get(ref))
  implicit val stringShow: StringShow[Layer] = store(_)(Log.log(Pid(0))).toOption.fold("???")(_.key)

  val CurrentVersion: Int = 6

  def set[T](newValue: T)(layer: Layer, lens: Lens[Layer, T, T]): Layer = lens(layer) = newValue

  def retrieve(conf: FuryConf)(implicit log: Log): Try[Layer] = for {
    base  <- get(conf.layerRef)
    layer <- dereference(base, conf.path)
  } yield layer

  private def dereference(layer: Layer, path: ImportPath)(implicit log: Log): Try[Layer] =
    if(path.isEmpty) Success(layer)
    else for {
      layerImport <- layer.imports.findBy(path.head)
      layer       <- get(layerImport.layerRef)
      layer       <- dereference(layer, path.tail)
    } yield layer

  def get(layerRef: LayerRef)(implicit log: Log): Try[Layer] =
    lookup(layerRef.ipfsRef).map(Success(_)).getOrElse { for {
      ipfs  <- Ipfs.daemon(false)
      data  <- ipfs.get(layerRef.ipfsRef)
      layer <- Try(Ogdl.read[Layer](data, migrate(_)))
    } yield layer }

  def store(layer: Layer)(implicit log: Log): Try[LayerRef] = for {
    ipfs    <- Ipfs.daemon(false)
    ipfsRef <- ipfs.add(Ogdl.serialize(Ogdl(layer)))
    _       <- Try(cache.synchronized { cache(ipfsRef) = layer })
  } yield LayerRef(ipfsRef.key)

  def commit(layer: Layer, conf: FuryConf, layout: Layout)(implicit log: Log): Try[Unit] = for {
    baseRef <- commitNested(conf, layer)
    base    <- get(baseRef)
    layer   <- ~base.copy(previous = Some(conf.layerRef))
    baseRef <- store(layer)
    _       <- saveFuryConf(conf.copy(layerRef = baseRef), layout)
    } yield ()

  private def commitNested(conf: FuryConf, layer: Layer)(implicit log: Log): Try[LayerRef] =
    if(conf.path.isEmpty) store(layer) else for {
      ref     <- store(layer)
      parent  <- retrieve(conf.parent)
      pLayer   = Layer(_.imports(conf.path.last).layerRef)(parent) = ref
      baseRef <- commitNested(conf.parent, pLayer)
    } yield baseRef

  def hashes(layer: Layer)(implicit log: Log): Try[Set[IpfsRef]] = for {
    layerRef  <- store(layer)
    layerRefs <- ~layer.imports.map(_.layerRef)
    layers    <- layerRefs.to[List].traverse(Layer.get(_))
    hashes    <- layers.traverse(hashes(_))
  } yield hashes.foldLeft(Set[IpfsRef]())(_ ++ _) + layerRef.ipfsRef

  def share(service: DomainName, layer: Layer, token: OauthToken)(implicit log: Log): Try[LayerRef] = for {
    ref    <- store(layer)
    hashes <- Layer.hashes(layer)
    _      <- Service.share(service, ref.ipfsRef, token, hashes - ref.ipfsRef)
  } yield ref

  def published(layerName: LayerName)(implicit log: Log): Try[Option[PublishedLayer]] = layerName match {
    case furyUri@FuryUri(service, path) =>
      Service.latest(service, path, None).map { artifact =>
        Some(PublishedLayer(furyUri, artifact.version, LayerRef(artifact.ref)))
       }
    case _ =>
      Success(None)
  }

  def resolve(layerInput: LayerName)(implicit log: Log): Try[LayerRef] = layerInput match {
    case FileInput(path)       => ???
    case FuryUri(domain, path) => Service.latest(domain, path, None).map { a => LayerRef(a.ref) }
    case IpfsRef(key)          => Success(LayerRef(key))
  }

  def pathCompletions()(implicit log: Log): Try[List[String]] =
    Service.catalog(ManagedConfig().service)

  def readFuryConf(layout: Layout)(implicit log: Log): Try[FuryConf] =
    Ogdl.read[FuryConf](layout.confFile, identity(_))
  
  def init(layout: Layout)(implicit log: Log): Try[Unit] = {
    if(layout.confFile.exists) { for {
      conf     <- readFuryConf(layout)
      layer    <- Layer.get(conf.layerRef)
      _        <- ~log.info(msg"Reinitialized layer ${conf.layerRef}")
    } yield () } else { for {
      _        <- layout.confFile.mkParents()
      ref      <- store(Layer(CurrentVersion))
      conf     <- saveFuryConf(FuryConf(ref), layout)
      _        <- ~log.info(msg"Initialized an empty layer")
    } yield () }
  }

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

  def pushToHistory(layerRef: LayerRef, layout: Layout)(implicit log: Log): Try[Unit] =
    layout.undoStack.writeSync(layerRef.key + "\n", append = true)

  def popFromHistory(layout: Layout)(implicit log: Log): Try[LayerRef] = {
    val history = layout.undoStack
    if(history.exists()) {
      val reader = scala.io.Source.fromFile(history.javaFile)
      val content = Try(reader.getLines().toList)
      reader.close()
      for {
        lines      <- content
        entries    <- lines.traverse(LayerRef.unapply(_).ascribe(HistoryCorrupt()))
        last       <- entries.lastOption.ascribe(HistoryMissing())
        newEntries =  entries.dropRight(1).map(_.key)
        _          <- layout.undoStack.writeSync(newEntries.mkString("\n"))
      } yield last
    } else Failure(HistoryMissing())
  }

  private def migrate(ogdl: Ogdl)(implicit log: Log): Ogdl = {
    val version = Try(ogdl.version().toInt).getOrElse(1)
    if(version < CurrentVersion) {
      log.note(msg"Migrating layer file from version $version to ${version + 1}")
      migrate((version match {
        case 0 | 1 | 2 | 3 | 4 | 5 =>
          log.fail(msg"Cannot migrate from layers earlier than version 6")
          // FIXME: Handle this better
          throw new Exception()
        case _ => null: Ogdl
      }).set(version = Ogdl(version + 1)))
    } else ogdl
  }
}