package fury.layer.schema

import fury._
import fury.error._

import scala.util._

object FurySchema {
  def add(layer: Layer, id: SchemaId): Outcome[Layer] = clone(layer, layer.main, id)

  def clone(layer: Layer, from: SchemaId, to: SchemaId): Outcome[Layer] =
    for {
      cloned <- layer(from)
      _      <- if (layer(to).isFailure) Success(Unit) else Failure(ItemAlreadyDefined(to))
      schema = cloned.copy(id = to)
    } yield layer.copy(schemas = layer.schemas + schema, main = to)

  def rename(layer: Layer, from: SchemaId, to: SchemaId, forced: Boolean = false): Outcome[Layer] =
    for {
      sourceSchema   <- layer(from)
      _              <- if (forced || layer(to).isFailure) Success(Unit) else Failure(ItemAlreadyDefined(to))
      schema         = sourceSchema.copy(id = to)
      updatedSchemas = (layer.schemas - sourceSchema) + schema
      updatedMain    = if (from == layer.main) to else layer.main
    } yield layer.copy(schemas = updatedSchemas, main = updatedMain)

  def remove(layer: Layer, id: SchemaId): Outcome[Layer] =
    for {
      schema      <- layer(id)
      updatedMain = if (id == layer.main) id else layer.main
    } yield layer.copy(schemas = layer.schemas - schema, main = updatedMain)

  def updateMainSchema(layer: Layer, schemaId: SchemaId): Option[Layer] =
    if (layer.main == schemaId) None
    else Some(layer.copy(main = schemaId))
}
