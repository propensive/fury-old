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

import fury.ogdl._, fury.io._

import scala.collection.mutable._
import scala.util._

sealed trait Pointer
case class ModulePointer(project: Option[ProjectId], module: Option[ModuleId]) extends Pointer
case class LayerPointer(layerId: LayerId, pointer: Pointer)                     extends Pointer

object Context {
  def read(layout: Layout): Try[Context] =
    Ogdl.read[Context](layout.contextFile, identity(_))

  def write(context: Context, layout: Layout): Try[Path] =
    Ogdl.write[Context](context, layout.contextFile)
}

case class Context(schemaRef: SchemaRef, pointer: Pointer)

case class Focus(
    layer: Layer,
    schemaId: SchemaId,
    projectId: Option[ProjectId],
    moduleId: Option[ModuleId]) {
  def schema: Try[Schema] = layer(schemaId)
}

object Layers {

  private[this] val cache: HashMap[IpfsRef, Layer] = HashMap()

  private[this] def loadFromIpfs(config: Config, layerRef: IpfsRef, layout: Layout): Try[Layer] =
    for {
      file  <- layout.shell.ipfs.get(layerRef, layout.tmpDir.tmpFile)
      layer <- Layer.read(Io.silent(config), file, layout)
      _     = cache.synchronized { cache(layerRef) = layer }
    } yield layer

  def load(layerRef: IpfsRef, config: Config, layout: Layout): Try[Layer] =
    cache.get(layerRef).fold(loadFromIpfs(config, layerRef, layout))(Success(_))

  def save(layout: Layout, config: Config, layer: Layer): Try[IpfsRef] =
    for {
      tmpFile  <- Ogdl.write(layer, layout.tmpDir.tmpFile)
      layerRef <- layout.shell.ipfs.add(tmpFile)
    } yield layerRef

  def apply(ctx: Context, config: Config, layout: Layout): Try[Focus] = {
    def resolve(currentLayer: Layer, schemaId: SchemaId, pointer: Pointer): Try[Focus] =
      pointer match {
        case ModulePointer(projectId, moduleId) =>
          Success(Focus(currentLayer, schemaId, projectId, moduleId))
        case LayerPointer(layerId, pointer) =>
          for {
            schema   <- currentLayer(schemaId)
            imported <- schema.imports.findBy(layerId)
            layer    <- load(imported.schemaRef.layerRef, config, layout)
            resolved <- resolve(layer, imported.schemaRef.schemaId, pointer)
          } yield resolved
      }

    load(ctx.schemaRef.layerRef, config, layout).flatMap(resolve(_, ctx.schemaRef.schemaId, ctx.pointer))
  }

  def update(ctx: Context, config: Config, layout: Layout, newLayer: Layer): Try[IpfsRef] = {
    def doUpdate(currentLayer: Layer, schemaId: SchemaId, pointer: Pointer): Try[IpfsRef] =
      pointer match {
        case ModulePointer(projectId, moduleId) =>
          save(layout, config, newLayer)
        case LayerPointer(layerId, pointer) =>
          for {
            schema   <- currentLayer(schemaId)
            imported <- schema.imports.findBy(layerId)
            layer    <- load(imported.schemaRef.layerRef, config, layout)
            updated  <- doUpdate(layer, schemaId, pointer)
            layer <- Success(Lenses.layer.importLayer(schemaId, layerId).modify(layer) { sr =>
                      sr.copy(layerRef = updated)
                    })
            layerRef <- save(layout, config, layer)
          } yield layerRef
      }

    load(ctx.schemaRef.layerRef, config, layout).flatMap(doUpdate(_, ctx.schemaRef.schemaId, ctx.pointer))
  }
}
