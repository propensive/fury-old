package fury

import scala.util.Try

final class SchemaView(schema: Option[SchemaId]) {
  def apply(layer: Layer): Try[Schema] = layer.apply(schema.getOrElse(layer.main))
}

final class ProjectView(schema: Option[SchemaId], project: Option[ProjectId]) {
  private val schemaVisitor = new SchemaView(schema)

  def apply(layer: Layer): Try[Project] =
    for {
      schema    <- schemaVisitor(layer)
      projectId <- project.orElse(schema.main).ascribe(UnspecifiedProject())
      project   <- schema(projectId)
    } yield project
}

final class ModuleView(
    schema: Option[SchemaId],
    project: Option[ProjectId],
    module: Option[ModuleId]) {
  private val projectVisitor = new ProjectView(schema, project)

  def apply(layer: Layer): Try[Module] =
    for {
      project  <- projectVisitor(layer)
      moduleId <- module.orElse(project.main).ascribe(UnspecifiedModule())
      module   <- project(moduleId)
    } yield module
}
