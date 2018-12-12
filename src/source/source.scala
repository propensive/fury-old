package fury

import mitigation._
import guillotine._
import Args._

object SourceCli {

  case class Context(override val cli: Cli[CliParam[_]],
                     override val layout: Layout,
                     override val config: Config,
                     override val layer: Layer,
                     optSchema: Option[Schema],
                     optProject: Option[Project],
                     optModule: Option[Module])
      extends MenuContext(cli, layout, config, layer, optSchema.map(_.id)) {
    def defaultSchemaId: SchemaId = optSchemaId.getOrElse(layer.main)
    def defaultSchema: Result[Schema, ~ | ItemNotFound] = layer.schemas.findBy(defaultSchemaId)
  }

  def context(cli: Cli[CliParam[_]]) = for {
    layout       <- cli.layout
    config       <- Config.read()(cli.env, layout)
    layer        <- Layer.read(layout.furyConfig)(layout)
    cli          <- cli.hint(SchemaArg, layer.schemas)
    schemaArg    <- ~cli.peek(SchemaArg)
    schema       <- ~layer.schemas.findBy(schemaArg.getOrElse(layer.main)).opt
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
  } yield new Context(cli, layout, config, layer, schema, optProject, optModule)

  def list(ctx: Context) = {
    import ctx._
    for {
      cli     <- cli.hint(RawArg)
      io      <- cli.io()
      raw     <- ~io(RawArg).successful
      module  <- optModule.ascribe(UnspecifiedModule())
      project <- optProject.ascribe(UnspecifiedProject())
      cols    <- Answer(Terminal.columns.getOrElse(100))
      rows    <- ~module.sources.to[List]
      table   <- ~Tables(config).show(Tables(config).sources, cols, rows, raw)(_.repoId)
      schema  <- defaultSchema
      _       <- ~(if(!raw) io.println(Tables(config).contextString(layout.pwd, layer.showSchema, schema, project, module)))
      _       <- ~io.println(table.mkString("\n"))
    } yield io.await()
  }

  def delete(ctx: Context) = {
    import ctx._
    for {
      cli         <- cli.hint(SourceArg, optModule.to[List].flatMap(_.sources))
      cli         <- cli.hint(ForceArg)
      io          <- cli.io()
      sourceArg   <- io(SourceArg)
      source      <- ~Source.unapply(sourceArg)
      module      <- optModule.ascribe(UnspecifiedModule())
      project     <- optProject.ascribe(UnspecifiedProject())
      sourceToDel <- ~module.sources.find(Some(_) == source)
      force       <- ~io(ForceArg).successful
      layer       <- Lenses.updateSchemas(optSchemaId, layer, force)(Lenses.layer.sources(_, project.id, module.id))(_(_) --= sourceToDel)
      _           <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }

    
  def add(ctx: Context) = {
    import ctx._
    for {
      dSchema   <- defaultSchema
      repos     <- ~dSchema.allRepos.opt.to[List].flatten
      sources   <- optProject.to[List].flatMap { project =>
                     repos.map(_.sourceCandidates { n => n.endsWith(".scala") || n.endsWith(".java") }).to[List]
                   }.sequence.map(_.flatten)
      cli       <- cli.hint(SourceArg, sources)
      io        <- cli.io()
      module    <- optModule.ascribe(UnspecifiedModule())
      project   <- optProject.ascribe(UnspecifiedProject())
      sourceArg <- io(SourceArg)
      source    <- ~Source.unapply(sourceArg)
      layer     <- Lenses.updateSchemas(optSchemaId, layer, true)(Lenses.layer.sources(_, project.id, module.id))(_(_) ++= source)
      _         <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }
}
