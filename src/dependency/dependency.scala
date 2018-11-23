package fury

import mitigation._
import guillotine._
import Args._

import scala.collection.immutable.SortedSet

object DependencyCli {
  
  case class Context(override val cli: Cli[CliParam[_]],
                     override val layout: Layout,
                     override val config: Config,
                     override val workspace: Workspace,
                     optSchema: Option[Schema],
                     optProject: Option[Project],
                     optModule: Option[Module])
      extends MenuContext(cli, layout, config, workspace, optSchema.map(_.id)) {
    def defaultSchemaId: SchemaId = optSchemaId.getOrElse(workspace.main)
    def defaultSchema: Result[Schema, ~ | ItemNotFound] = workspace.schemas.findBy(defaultSchemaId)
  }
  
  def context(cli: Cli[CliParam[_]]) = for {
    layout       <- cli.layout
    config       <- Config.read()(cli.env, layout)
    workspace    <- Workspace.read(layout.furyConfig)(layout)
    cli          <- cli.hint(SchemaArg, workspace.schemas)
    schemaArg    <- ~cli.peek(SchemaArg)
    schema       <- ~workspace.schemas.findBy(schemaArg.getOrElse(workspace.main)).opt
    cli          <- cli.hint(ProjectArg, schema.map(_.projects).getOrElse(Nil))
    optProjectId <- ~schema.flatMap { s => cli.peek(ProjectArg).orElse(s.main) }
    optProject   <- ~schema.flatMap { s => optProjectId.flatMap(s.projects.findBy(_).opt) }
    cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    optModuleId  <- ~cli.peek(ModuleArg).orElse(optProject.flatMap(_.main))
    optModule    <- Answer { for {
                               project  <- optProject
                               moduleId <- optModuleId
                               module   <- project.modules.findBy(moduleId).opt
                             } yield module }
    } yield new Context(cli, layout, config, workspace, schema, optProject, optModule)

  def list(ctx: Context) = {
    import ctx._
    for {
      cli     <- cli.hint(RawArg)
      io      <- cli.io()
      raw     <- ~io(RawArg).successful
      project <- optProject.ascribe(UnspecifiedProject())
      module  <- optModule.ascribe(UnspecifiedModule())
      cols    <- Answer(Terminal.columns.getOrElse(100))
      rows    <- ~module.after.to[List].sorted
      table   <- ~Tables(config).show(Tables(config).dependencies, cols, rows, raw)(identity)
      schema  <- defaultSchema
      io      <- ~(if(!raw) io.println(Tables(config).contextString(layout.pwd, workspace.showSchema, schema, project, module)) else io)
      io      <- ~io.println(table.mkString("\n"))
    } yield io.await()
  }

  def delete(ctx: Context) = {
    import ctx._
    for {
      cli           <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
      cli           <- cli.hint(DependencyArg, optModule.to[List].flatMap(_.after.to[List]))
      cli           <- cli.hint(ForceArg)
      io            <- cli.io()
      dependencyArg <- io(DependencyArg)
      project       <- optProject.ascribe(UnspecifiedProject())
      module        <- optModule.ascribe(UnspecifiedModule())
      moduleRef     <- ModuleRef.parse(project, dependencyArg, false)
      force         <- ~io(ForceArg).successful
      workspace     <- Lenses.updateSchemas(optSchemaId, workspace, force)(Lenses.workspace.after(_, project.id, module.id))(_(_) -= moduleRef)
      io            <- ~io.save(workspace, layout.furyConfig)
    } yield io.await()
  }

  def add(ctx: Context) = {
    import ctx._
    for {
      cli           <- cli.hint(DependencyArg,
                           optProject.to[List].flatMap(workspace.moduleRefStrings(_)))
      cli           <- cli.hint(IntransitiveArg)
      io            <- cli.io()
      project       <- optProject.ascribe(UnspecifiedProject())
      module        <- optModule.ascribe(UnspecifiedModule())
      intransitive  <- ~io(IntransitiveArg).successful
      dependencyArg <- io(DependencyArg)
      moduleRef     <- ModuleRef.parse(project, dependencyArg, intransitive)
      workspace     <- Lenses.updateSchemas(optSchemaId, workspace, true)(Lenses.workspace.after(_, project.id, module.id))(_(_) += moduleRef)
      io            <- ~io.save(workspace, layout.furyConfig)
    } yield io.await()
  }
}
