/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.8.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.                                         ║
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

import fury.model._, fury.io._, fury.strings._, fury.ogdl._

import mercator._
import gastronomy._
import kaleidoscope._

import scala.util._
import scala.collection.immutable._
import guillotine._

import scala.collection.mutable.HashMap

case class Layer(version: Int,
                 schemas: SortedSet[Schema] = TreeSet(Schema(SchemaId.default)),
                 main: SchemaId = SchemaId.default,
                 aliases: SortedSet[Alias] = TreeSet()) { layer =>

  def mainSchema: Try[Schema] = schemas.findBy(main)
  def showSchema: Boolean = schemas.size > 1
  def apply(schemaId: SchemaId): Try[Schema] = schemas.find(_.id == schemaId).ascribe(ItemNotFound(schemaId))
  def projects: Try[SortedSet[Project]] = mainSchema.map(_.projects)
}

object Layer {

  private val cache: HashMap[IpfsRef, Layer] = HashMap()

  private def lookup(ref: IpfsRef): Option[Layer] = cache.synchronized(cache.get(ref))

  val CurrentVersion = 5

  def retrieve(conf: FuryConf, quiet: Boolean = false)(implicit log: Log): Try[Layer] = for {
    base  <- get(conf.layerRef, quiet)
    layer <- dereference(base, conf.path, quiet)
  } yield layer

  private def dereference(layer: Layer, path: ImportPath, quiet: Boolean)(implicit log: Log): Try[Layer] =
    if(path.isEmpty) Success(layer)
    else { for {
      schema      <- layer.mainSchema
      layerImport <- schema.imports.findBy(path.head)
      layer       <- get(layerImport.layerRef, quiet)
      layer       <- dereference(layer, path.tail, quiet)
    } yield layer }

  def get(layerRef: LayerRef, quiet: Boolean = false)(implicit log: Log): Try[Layer] =
    lookup(layerRef.ipfsRef).map(Success(_)).getOrElse { for {
      ipfs  <- Ipfs.daemon(quiet)
      data  <- ipfs.get(layerRef.ipfsRef)
      layer <- Try(Ogdl.read[Layer](data, migrate(_)))
    } yield layer }

  def store(layer: Layer, quiet: Boolean = false)(implicit log: Log): Try[LayerRef] = for {
    ipfs    <- Ipfs.daemon(quiet)
    ipfsRef <- ipfs.add(Ogdl.serialize(Ogdl(layer)))
    _       <- Try(cache.synchronized { cache(ipfsRef) = layer })
  } yield LayerRef(ipfsRef.key)

  def commit(layer: Layer, conf: FuryConf, layout: Layout, quiet: Boolean = false)
            (implicit log: Log)
            : Try[Unit] = for {
    ref  <- store(layer, quiet)
    conf <- ~conf.copy(layerRef = ref)
    _    <- saveFuryConf(conf, layout)
  } yield ()

  def hashes(layer: Layer)(implicit log: Log): Try[Set[IpfsRef]] = for {
    layerRef  <- store(layer)
    schema    <- layer.mainSchema
    layerRefs <- ~schema.imports.map(_.layerRef)
    layers    <- layerRefs.to[List].traverse(Layer.get(_))
    hashes    <- layers.traverse(hashes(_))
  } yield hashes.foldLeft(Set[IpfsRef]())(_ ++ _) + layerRef.ipfsRef

  def share(service: String, layer: Layer, token: OauthToken)(implicit log: Log): Try[LayerRef] = for {
    ref     <- store(layer)
    hashes  <- Layer.hashes(layer)
    _       <- Service.publish(service, ref.ipfsRef, None, None, false, true, true, 0, 0, token, hashes)
  } yield ref

  def resolve(layerInput: LayerName)(implicit log: Log): Try[LayerRef] = layerInput match {
    case FileInput(path) => 
      ???
    case FuryUri(domain, path) =>
      for {
        artifact <- Service.latest(domain, path, None)
      } yield LayerRef(artifact.ref)
    case IpfsRef(key) =>
      Success(LayerRef(key))
  }

  def pathCompletions()(implicit log: Log): Try[List[String]] =
    Service.catalog(ManagedConfig().service).map(_.map(_.path))

  def readFuryConf(layout: Layout)(implicit log: Log): Try[FuryConf] =
    Ogdl.read[FuryConf](layout.confFile, identity(_))
  
  def init(layout: Layout)(implicit log: Log): Try[Unit] = {
    if(layout.confFile.exists) { for {
      conf     <- readFuryConf(layout)
      layer    <- Layer.get(conf.layerRef, false)
      _        <- ~log.info(msg"Reinitialized layer ${conf.layerRef}")
    } yield () } else { for {
      _        <- layout.confFile.mkParents()
      ref      <- store(Layer(CurrentVersion), false)
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

  def pushToHistory(layerRef: LayerRef, layout: Layout)(implicit log: Log): Try[Unit] = {
    layout.undoStack.writeSync(layerRef.key + "\n", append = true)
  }

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
        case 0 | 1 | 2 =>
          log.fail(msg"Cannot migrate from layers earlier than version 3")
          // FIXME: Handle this better
          throw new Exception()
        case 3 =>
          ogdl.set(schemas = ogdl.schemas.map { schema =>
            schema.set(imports = schema.imports.map { imp =>
              imp.set(id = Ogdl(s"unknown-${Counter.next()}"))
            })
          })

        case 4 =>
          ogdl.set(schemas = ogdl.schemas.map { schema =>
            schema.set(projects = schema.projects.map { project =>
              project.set(modules = project.modules.map { module =>
                module.set(
                  opts = module.params.map { param => Ogdl(Opt(OptId(param()), false, false)) },
                  dependencies = module.after,
                  binaries = module.binaries.map { bin => bin.set(id = bin.artifact) },
                  policy = module.policy.map { permission => permission.set(classRef =
                      Ogdl(permission.className())) }
                )
              })
            })
          })
      }).set(version = Ogdl(version + 1)))
    } else ogdl
  }
}
  
