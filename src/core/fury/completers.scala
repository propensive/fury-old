package fury

import fury.cli._

import scala.util._

object ThemeCompleter extends ParameterValueSource {
  override def matchingValues(prefix: String)(arguments: Arguments): Try[Seq[ParameterValue]] =
    Try(Theme.all.map(t => ParameterValue(t.name)))
}

final class SchemaCompleter(ctx: ExecutionContext) extends ParameterValueSource {
  override def matchingValues(prefix: String)(arguments: Arguments): Try[Seq[ParameterValue]] =
    for {
      layer   <- ctx.layer
      schemas = layer.schemas.filter(_.id.key.startsWith(prefix)).toSeq
    } yield schemas.map(s => ParameterValue(s.id.key))
}

final class ProjectCompleter(ctx: ExecutionContext) extends ParameterValueSource {
  override def matchingValues(prefix: String)(arguments: Arguments): Try[Seq[ParameterValue]] =
    for {
      schemaId <- arguments(FuryCLIParameters.Schema)

      layer    <- ctx.layer
      schema   <- new SchemaView(schemaId).apply(layer)
      projects = schema.projects.filter(_.id.key.startsWith(prefix)).toSeq
    } yield projects.map(s => ParameterValue(s.id.key))
}

final class ModuleCompleter(ctx: ExecutionContext) extends ParameterValueSource {
  override def matchingValues(prefix: String)(arguments: Arguments): Try[Seq[ParameterValue]] =
    for {
      schemaId  <- arguments(FuryCLIParameters.Schema)
      projectId <- arguments(FuryCLIParameters.Project)

      layer   <- ctx.layer
      project <- new ProjectView(schemaId, projectId).apply(layer)
      modules = project.modules.filter(_.id.key.startsWith(prefix)).toSeq
    } yield modules.map(s => ParameterValue(s.id.key))
}

final class DependencyCompleter(ctx: ExecutionContext) extends ParameterValueSource {
  override def matchingValues(prefix: String)(arguments: Arguments): Try[Seq[ParameterValue]] =
    for {
      (project, module) <- projectAndModule(arguments)

      intransitive <- arguments(FuryCLIParameters.Intransitive)
      dependencyPrefix <- ModuleRef.parse(project, prefix, intransitive) match {
                           case Success(moduleRef)           => Try(moduleRef.moduleId.key)
                           case Failure(ItemNotFound(id, _)) => Try(id.string(Theme.NoColor))
                           case Failure(cause)               => Failure(cause)
                         }
      allModules <- allModules
      dependencies = (allModules -- module.after)
        .filter(_.moduleId.key.startsWith(dependencyPrefix))
    } yield dependencies.map(s => ParameterValue(s.moduleId.key)).toSeq

  private def projectAndModule(arguments: Arguments): Try[(Project, Module)] =
    for {
      layer     <- ctx.layer
      schemaId  <- arguments(FuryCLIParameters.Schema)
      projectId <- arguments(FuryCLIParameters.Project)
      moduleId  <- arguments(FuryCLIParameters.Module)
      project   <- new ProjectView(schemaId, projectId).apply(layer)
      module    <- new ModuleView(schemaId, projectId, moduleId).apply(layer)
    } yield (project, module)

  private def allModules =
    for {
      layout   <- ctx.layout
      layer    <- ctx.layer
      current  <- layer.mainSchema
      imported <- current.importedSchemas(ctx.io, layout)
    } yield (current :: imported).flatMap(_.moduleRefs).toSet
}
