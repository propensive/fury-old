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

import fury.ogdl._

import scala.collection.mutable._
import scala.util._

sealed trait Focus
case class ModuleFocus(project: Option[ProjectId], module: Option[ModuleId]) extends Focus
case class LayerFocus(layerId: LayerId, focus: Focus)                        extends Focus

case class Context(schemaRef: SchemaRef, focus: Focus)

case class ResolvedFocus(
    layer: Layer,
    schemaId: SchemaId,
    projectId: Option[ProjectId],
    moduleId: Option[ModuleId]) {
  def schema: Try[Schema] = layer(schemaId)
}

object Layers {

  private[this] val cache: HashMap[IpfsRef, Layer] = HashMap()

  private[this] def loadFromIpfs(io: Io, layerRef: IpfsRef, layout: Layout): Try[Layer] =
    for {
      file  <- layout.shell.ipfs.get(layerRef, layout.tmpDir.tmpFile)
      layer <- Layer.read(io, file, layout)
      _     = cache.synchronized { cache(layerRef) = layer }
    } yield layer

  def load(io: Io, layerRef: IpfsRef, layout: Layout): Try[Layer] =
    cache.get(layerRef).fold(loadFromIpfs(io, layerRef, layout))(Success(_))

  def save(io: Io, layout: Layout, layer: Layer): Try[IpfsRef] =
    for {
      tmpFile  <- Ogdl.write(layer, layout.tmpDir.tmpFile)
      layerRef <- layout.shell.ipfs.add(tmpFile)
    } yield layerRef

  def apply(io: Io, ctx: Context, layout: Layout): Try[ResolvedFocus] = {
    def resolve(currentLayer: Layer, schemaId: SchemaId, focus: Focus): Try[ResolvedFocus] =
      focus match {
        case ModuleFocus(projectId, moduleId) =>
          Success(ResolvedFocus(currentLayer, schemaId, projectId, moduleId))
        case LayerFocus(layerId, focus) =>
          for {
            schema   <- currentLayer(schemaId)
            imported <- schema.imports.findBy(layerId)
            layer    <- load(io, imported.schemaRef.layerRef, layout)
            resolved <- resolve(layer, imported.schemaRef.schemaId, focus)
          } yield resolved
      }

    load(io, ctx.schemaRef.layerRef, layout).flatMap(resolve(_, ctx.schemaRef.schemaId, ctx.focus))
  }

  def update(io: Io, ctx: Context, layout: Layout, newLayer: Layer): Try[IpfsRef] = {
    def doUpdate(currentLayer: Layer, schemaId: SchemaId, focus: Focus): Try[IpfsRef] =
      focus match {
        case ModuleFocus(projectId, moduleId) =>
          save(io, layout, newLayer)
        case LayerFocus(layerId, focus) =>
          for {
            schema   <- currentLayer(schemaId)
            imported <- schema.imports.findBy(layerId)
            layer    <- load(io, imported.schemaRef.layerRef, layout)
            updated  <- doUpdate(layer, schemaId, focus)
            layer <- Success(Lenses.layer.importLayer(schemaId, layerId).modify(layer) { sr =>
                      sr.copy(layerRef = updated)
                    })
            layerRef <- save(io, layout, layer)
          } yield layerRef
      }

    load(io, ctx.schemaRef.layerRef, layout).flatMap(doUpdate(_, ctx.schemaRef.schemaId, ctx.focus))
  }
}
