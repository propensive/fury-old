/*
  Fury, version 0.4.0. Copyright 2018-19 Jon Pretty, Propensive Ltd.

  The primary distribution site is: https://propensive.com/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required  by applicable  law or  agreed to  in writing,  software  distributed  under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
  express  or  implied.  See  the  License for  the specific  language  governing  permissions and
  limitations under the License.
 */
package fury.core

import fury.ogdl._, fury.io._, fury.strings._

import scala.collection.mutable._
import scala.util._

import mercator._
import kaleidoscope._

sealed trait Pointer
case class Target(project: Option[ProjectId], module: Option[ModuleId]) extends Pointer
case class Deref(layerId: LayerId, pointer: Pointer)                    extends Pointer

object Context {

  def read(layout: Layout): Try[Context] =
    Ogdl.read[Context](layout.contextFile, identity(_))

  def write(context: Context, layout: Layout): Try[Path] =
    Ogdl.write[Context](context, layout.contextFile)
}

case class Context(schemaRef: SchemaRef, pointer: Pointer) {

  def dereference(path: LayerPath): Context = {
    def recurse(xs: List[LayerId]): Pointer = xs match {
      case Nil             => Target(None, None)
      case layerId :: tail => Deref(layerId, recurse(tail))
    }
    Context(schemaRef, recurse(path.layerIds))
  }

  def targetModule(newModuleId: Option[ModuleId]) = {
    def updatePointer(pointer: Pointer): Pointer = pointer match {
      case Target(projectId, moduleId) => Target(projectId, newModuleId)
      case Deref(layerId, pointer)     => Deref(layerId, updatePointer(pointer))
    }

    Context(schemaRef, updatePointer(pointer))
  }

  def targetProject(newProjectId: Option[ProjectId]) = {
    def updatePointer(pointer: Pointer): Pointer = pointer match {
      case Target(projectId, moduleId) => Target(newProjectId, moduleId)
      case Deref(layerId, pointer)     => Deref(layerId, updatePointer(pointer))
    }

    Context(schemaRef, updatePointer(pointer))
  }
}

case class Focus(
    layer: Layer,
    schemaId: SchemaId,
    projectId: Option[ProjectId],
    moduleId: Option[ModuleId]) {
  def schema: Try[Schema] = layer(schemaId)
}

object LayerPath {
  implicit val msgShow: MsgShow[LayerPath] =
    _.layerIds.map { l =>
      msg"$l"
    }.foldLeft(msg"${LayerId(".")}") { (l, r) =>
      msg"$l${'/'}$r"
    }

  implicit val stringShow: StringShow[LayerPath] = msgShow.show(_).string(Theme.NoColor)

  def parse(str: String): Option[LayerPath] = str match {
    case r"\.(/[a-z]-?[a-z0-9]+)*" => Some(LayerPath(str.split("/").to[List].tail.map(LayerId(_))))
    case _                         => None
  }
}
case class LayerPath(layerIds: List[LayerId]) {
  def ::(layerId: LayerId) = LayerPath(layerId :: layerIds)
}

object Layers {

  private[this] val cache: HashMap[IpfsRef, Layer] = HashMap()

  private[this] def loadFromIpfs(io: Io, layerRef: IpfsRef, layout: Layout): Try[Layer] =
    for {
      file  <- layout.shell.ipfs.get(layerRef, layout.tmpDir.tmpFile)
      layer <- Layer.read(io, file, layout)
      _     = cache.synchronized { cache(layerRef) = layer }
    } yield layer

  def children(schemaRef: SchemaRef, io: Io, layout: Layout) = {
    def recurse(schemaRef: SchemaRef): Try[List[LayerPath]] =
      for {
        layer   <- load(schemaRef.layerRef, io, layout)
        schema  <- layer(schemaRef.schemaId)
        imports = schema.imports.to[List]
        layerPaths <- imports.map { imp =>
                       recurse(imp.schemaRef).map(_.map(imp.id :: _))
                     }.sequence.map(_.flatten)
      } yield LayerPath(Nil) :: layerPaths

    recurse(schemaRef)
  }

  def load(layerRef: IpfsRef, io: Io, layout: Layout): Try[Layer] =
    cache.get(layerRef).fold(loadFromIpfs(io, layerRef, layout))(Success(_))

  def save(layout: Layout, io: Io, layer: Layer): Try[IpfsRef] =
    for {
      tmpFile  <- Ogdl.write(layer, layout.tmpDir.tmpFile)
      layerRef <- layout.shell.ipfs.add(tmpFile)
    } yield layerRef

  def apply(ctx: Context, io: Io, layout: Layout): Try[Focus] = {
    def resolve(currentLayer: Layer, schemaId: SchemaId, pointer: Pointer): Try[Focus] =
      pointer match {
        case Target(projectId, moduleId) =>
          Success(Focus(currentLayer, schemaId, projectId, moduleId))
        case Deref(layerId, pointer) =>
          for {
            schema   <- currentLayer(schemaId)
            imported <- schema.imports.findBy(layerId)
            layer    <- load(imported.schemaRef.layerRef, io, layout)
            resolved <- resolve(layer, imported.schemaRef.schemaId, pointer)
          } yield resolved
      }

    load(ctx.schemaRef.layerRef, io, layout)
      .flatMap(resolve(_, ctx.schemaRef.schemaId, ctx.pointer))
  }

  def update(ctx: Context, io: Io, layout: Layout, newLayer: Layer): Try[Context] = {
    def doUpdate(currentLayer: Layer, schemaId: SchemaId, pointer: Pointer): Try[IpfsRef] =
      pointer match {
        case Target(projectId, moduleId) =>
          save(layout, io, newLayer)
        case Deref(layerId, pointer) =>
          for {
            schema   <- currentLayer(schemaId)
            imported <- schema.imports.findBy(layerId)
            layer    <- load(imported.schemaRef.layerRef, io, layout)
            updated  <- doUpdate(layer, schemaId, pointer)
            layer <- Success(
                        Lenses.layer.importLayer(schemaId, layerId).modify(currentLayer) { sr =>
                          sr.copy(layerRef = updated)
                        })
            layerRef <- save(layout, io, layer)
          } yield layerRef
      }

    load(ctx.schemaRef.layerRef, io, layout)
      .flatMap(doUpdate(_, ctx.schemaRef.schemaId, ctx.pointer))
      .map { ref =>
        val newContext = ctx.copy(schemaRef = ctx.schemaRef.copy(layerRef = ref))
        Context.write(newContext, layout)
        newContext
      }
  }
}
