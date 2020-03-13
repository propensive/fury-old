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

case class Layer(version: Int,
                 schemas: SortedSet[Schema] = TreeSet(Schema(SchemaId.default)),
                 main: SchemaId = SchemaId.default,
                 aliases: SortedSet[Alias] = TreeSet()) { layer =>

  def mainSchema: Try[Schema] = schemas.findBy(main)
  def showSchema: Boolean = schemas.size > 1
  def apply(schemaId: SchemaId): Try[Schema] = schemas.find(_.id == schemaId).ascribe(ItemNotFound(schemaId))
  def projects: Try[SortedSet[Project]] = mainSchema.map(_.projects)

  lazy val hash: String = Layer.digestLayer(this).key.take(6).toLowerCase
}

object Layer {
    val CurrentVersion = 5
    implicit val msgShow: MsgShow[Layer] = r => UserMsg(_.layer(r.hash))
    implicit val stringShow: StringShow[Layer] = _.hash
  
    def loadFromIpfs(env: Environment, layerRef: IpfsRef, layout: Layout, quiet: Boolean)
                    (implicit log: Log)
                    : Try[LayerRef] =
      Installation.tmpDir { dir => for {
        ipfs <- Ipfs.daemon(env, quiet)
        file <- ipfs.get(layerRef, dir)
        ref  <- loadDir(file, layout)
      } yield ref }
  
    def moveFuryConfIfNecessary(dir: Path): Try[Unit] = {
      val oldFile = dir / ".focus.fury"
      val newFile = dir / ".fury.conf"
      Try(if(oldFile.exists && !newFile.exists) oldFile.moveTo(newFile))
    }
  
    def loadDir(dir: Path, layout: Layout)(implicit log: Log): Try[LayerRef] =
      for {
        _      <- (dir / "layers").childPaths.map { f => f.moveTo(Installation.layersPath / f.name) }.sequence
        bases  <- ~(dir / "bases").childPaths
        _      <- bases.map { b => b.moveTo(layout.basesDir / b.name)}.sequence
        _      <- moveFuryConfIfNecessary(dir)
        conf   <- Ogdl.read[FuryConf](dir / ".fury.conf", identity(_))
      } yield conf.layerRef
  
    def loadFile(file: Path, layout: Layout)(implicit log: Log): Try[LayerRef] =
      Installation.tmpDir { tmpDir => for {
        _      <- TarGz.extract(file, tmpDir)
        _      <- (tmpDir / "layers").childPaths.map { f => f.moveTo(Installation.layersPath / f.name) }.sequence
        bases  <- ~(tmpDir / "bases").childPaths
        _      <- bases.map { b => b.moveTo(layout.basesDir / b.name)}.sequence
        _      <- moveFuryConfIfNecessary(tmpDir)
        conf   <- Ogdl.read[FuryConf](tmpDir / ".fury.conf", identity(_))
      } yield conf.layerRef }
  
    def share(env: Environment, layer: Layer, layout: Layout, quiet: Boolean)(implicit log: Log): Try[IpfsRef] =
      Installation.tmpDir { tmp => for {
        layerRef  <- saveLayer(layer)
        schemaRef =  Import(ImportId(""), layerRef, layer.main, None)
        layerRefs <- collectLayerRefs(schemaRef, layout)
        _         =  (tmp / "layers").mkdir()
        filesMap  =  layerRefs.map { ref =>
                       (tmp / "layers" / ref.key, Installation.layersPath / ref.key)
                     }.toMap.updated(tmp / ".fury.conf", layout.confFile)
        _         <- filesMap.toSeq.traverse { case (dest, src) => src.copyTo(dest) }
        ipfs      <- Ipfs.daemon(env, quiet)
        ref       <- ipfs.add(tmp)
      } yield ref }
  
    def parse(path: String, layout: Layout): Try[LayerInput] = {
      val service = ManagedConfig().service
      path match {
        case r"fury:\/\/$ref@([A-Za-z0-9]{46})\/?" =>
          Success(IpfsRef(ref))
        case r"fury:\/\/$dom@(([a-z]+\.)+[a-z]{2,})\/$loc@(([a-z][a-z0-9]*\/)+[a-z][0-9a-z]*([\-.][0-9a-z]+)*)" =>
          Success(FuryUri(dom, loc))
        case r".*\.fury" =>
          val file = Path(path).in(layout.pwd)
          if(file.exists) Success(FileInput(file)) else Failure(LayerNotFound(file))
        case r"([a-z][a-z0-9]*\/)+[a-z][0-9a-z]*([\-.][0-9a-z]+)*" =>
          Success(FuryUri(service, path))
        case name =>
          Failure(InvalidLayer(name))
      }
    }
  
    def load(env: Environment, input: LayerInput, layout: Layout)(implicit log: Log): Try[LayerRef] =
      input match {
        case FuryUri(domain, path) => for {
                                        entries <- Service.catalog(domain)
                                        ipfsRef <- Try(entries.find(_.path == path).get)
                                        ref     <- loadFromIpfs(env, IpfsRef(ipfsRef.ref), layout, false)
                                      } yield ref
        case ref@IpfsRef(_)        => loadFromIpfs(env, ref, layout, false)
        case FileInput(file)       => loadFile(file, layout)
      }
  
    def pathCompletions()(implicit log: Log): Try[List[String]] =
      Service.catalog(ManagedConfig().service).map(_.map(_.path))
  
    def readFuryConf(layout: Layout)(implicit log: Log): Try[FuryConf] =
      Ogdl.read[FuryConf](layout.confFile, identity(_))
  
    private def collectLayerRefs(ref: Import, layout: Layout)(implicit log: Log): Try[Set[LayerRef]] = for {
      layer   <- read(ref.layerRef, layout)
      schema  <- layer.schemas.findBy(ref.schema)
      imports <- schema.imports.map(collectLayerRefs(_, layout)).sequence.map(_.flatten)
    } yield imports + ref.layerRef
  
    def export(layer: Layer, layout: Layout, path: Path)(implicit log: Log): Try[Path] = for {
      layerRef  <- saveLayer(layer)
      schemaRef <- ~Import(ImportId(""), layerRef, layer.main, None)
      layerRefs <- collectLayerRefs(schemaRef, layout)
      filesMap  <- ~layerRefs.map { ref => (Path(str"layers/${ref}"), Installation.layersPath / ref.key) }.toMap
      _         <- TarGz.store(filesMap.updated(Path(".fury.conf"), layout.confFile), path)
    } yield path
  
    def base(layout: Layout)(implicit log: Log): Try[Layer] = for {
      conf     <- readFuryConf(layout)
      layer    <- read(conf.layerRef, layout)
    } yield layer
  
    def read(layout: Layout, conf: FuryConf)(implicit log: Log): Try[Layer] = for {
      _        <- Ipfs.daemon(environments.enclosing, quiet = false)
      layer    <- read(conf.layerRef, layout)
      newLayer <- resolveSchema(layout, layer, conf.path)
    } yield newLayer
  
    def read(ref: LayerRef, layout: Layout)(implicit log: Log): Try[Layer] =
      Ogdl.read[Layer](Installation.layersPath / ref.key, migrate(_))
  
    def resolveSchema(layout: Layout, layer: Layer, path: ImportPath)(implicit log: Log): Try[Layer] =
      path.parts.foldLeft(Try(layer)) { case (current, importId) => for {
        layer     <- current
        schema    <- layer.mainSchema
        schemaRef <- schema.imports.findBy(importId)
        layer     <- read(schemaRef.layerRef, layout)
      } yield layer.copy(main = schemaRef.schema) }
  
    def digestLayer(layer: Layer): LayerRef = {
      val dummyLog = new Log(LogStyle(???, false), Pid(-1))
      val hash = Installation.tmpFile { file =>
        for {
          ipfs <- Ipfs.daemon(environments.enclosing, quiet = true)(dummyLog)
          _ <- file.writeSync(Ogdl.serialize(Ogdl(layer)))
          ref <- ipfs.hash(file)
        } yield ref.key
      }
      LayerRef(hash.get)
    }

  def init(env: Environment, layout: Layout)(implicit log: Log): Try[Unit] = {
      if(layout.confFile.exists) { for {
        conf     <- readFuryConf(layout)
        url      <- Try(conf.published.get)
        ref      <- parse(str"$url", layout)
        layer    <- Layer.load(env, ref, layout)
        conf     <- saveFuryConf(FuryConf(layer, conf.path, conf.published), layout)
        _        <- ~log.info(msg"Initialized layer ${layer}")
      } yield () } else { for {
        _        <- layout.confFile.mkParents()
        layerRef <- saveLayer(Layer(CurrentVersion))
        conf     <- saveFuryConf(FuryConf(layerRef), layout)
        _        <- ~log.info(msg"Initialized an empty layer")
      } yield () }
    }
  
    def save(newLayer: Layer, layout: Layout)(implicit log: Log): Try[LayerRef] = for {
      conf         <- readFuryConf(layout)
      currentLayer <- read(conf.layerRef, layout)
      layerRef     <- saveSchema(layout, newLayer, conf.path, currentLayer)
      _            <- pushToHistory(conf.layerRef, layout)
      _            <- saveFuryConf(conf.copy(layerRef = layerRef), layout)
    } yield layerRef

    def undo(layout: Layout)(implicit log: Log): Try[Unit] = for {
      conf         <- readFuryConf(layout)
      lastLayer    <- popFromHistory(layout)
      _            <- saveFuryConf(conf.copy(layerRef = lastLayer), layout)
      _            <- ~log.info(msg"Reverted to layer $lastLayer")
    } yield ()

    private def saveSchema(layout: Layout, newLayer: Layer, path: ImportPath, currentLayer: Layer)
                          (implicit log: Log)
                          : Try[LayerRef] =
      if(path.isEmpty) saveLayer(newLayer)
      else for {
        schema    <- currentLayer.mainSchema
        schemaRef <- schema.imports.findBy(path.head)
        nextLayer <- read(schemaRef.layerRef, layout)
        layerRef  <- saveSchema(layout, newLayer, path.tail, nextLayer)
        newSchema <- ~schema.copy(imports = schema.imports.filter(_.id != path.head) + schemaRef.copy(layerRef =
                         layerRef))
        newLayer  <- ~currentLayer.copy(schemas = currentLayer.schemas.filter(_.id != currentLayer.main) +
                         newSchema)
        newLayerRef <- saveLayer(newLayer)
      } yield newLayerRef

    private def saveLayer(layer: Layer)(implicit log: Log): Try[LayerRef] = for {
      _ <- Ipfs.daemon(environments.enclosing, quiet = false)
      layerRef <- ~digestLayer(layer)
      _        <- (Installation.layersPath / layerRef.key).writeSync(Ogdl.serialize(Ogdl(layer)))
    } yield layerRef

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
  
