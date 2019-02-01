package fury.layer.schema

import fury.SchemaId.default
import fury._
import probably._

import scala.collection.immutable.TreeSet

object FurySchemaTest extends TestApp {
  private val id = SchemaId("ID")

  override def tests(): Unit = {
    test("main schema can be updated") {
      val layer = Layer()

      FurySchema.updateMainSchema(layer, id)
    }.assert(_.contains(Layer(main = id)))

    test("updating main schema is idempotent") {
      val layer = Layer()

      FurySchema.updateMainSchema(layer, default)
    }.assert(_.isEmpty)

    test("schema cannot be added if target name already exists") {
      val layer = layerWithSchema(id)

      FurySchema.add(layer, id)
    }.assert(_.isFailure)

    test("schema can be added") {
      val layer = Layer()

      FurySchema.add(layer, id)
    }.assert(_.get(id).isSuccess)

    val originalSchema = Schema(id = SchemaId("original"), main = Some(ProjectId("Some project")))
    test("schema can be cloned") {
      val layer = layerWithSchema(originalSchema)

      FurySchema.clone(layer, originalSchema.id, id)
    }.assert(_.get(id).get == originalSchema.copy(id = id))

    test("schema can be removed") {
      val layer = layerWithSchema(id)

      FurySchema.remove(layer, id)
    }.assert(_.get(id).isFailure)

    test("schema can be renamed") {
      val layer = Layer()

      FurySchema.rename(layer, default, id)
    }.assert(result => result.get(id).isSuccess && result.get(default).isFailure)

    test("schema - by default - cannot replace existing schemas") {
      val layer = layerWithSchema(id)

      FurySchema.rename(layer, default, id)
    }.assert(_.isFailure)

    test("schema can replace existing one using force") {
      val layer = layerWithSchema(default, id)

      FurySchema.rename(layer, default, id, forced = true)
    }.assert(result => result.get(id).isSuccess && result.get(default).isFailure)
  }

  private def layerWithSchema(ids: SchemaId*) = {
    val schemas = ids.map(id => Schema(id))
    Layer(schemas = TreeSet(schemas: _*))
  }

  private def layerWithSchema(schema: Schema) = Layer(schemas = TreeSet(schema))
}
