package fury

object ImportCli {

  def add(ctx: SchemaCtx) = {
    import ctx._
    for {
      cli           <- cli.hint(SchemaArg, workspace.schemas.map(_.id))
      schemaArg     <- ~cli.peek(SchemaArg)
      defaultSchema <- ~workspace.schemas.findBy(schemaArg.getOrElse(workspace.main)).opt
      cli           <- cli.hint(ImportArg, defaultSchema.map(_.importCandidates(layout, cli.shell)).getOrElse(Nil))
      io            <- cli.io()
      schemaRef     <- io(ImportArg)
      workspace     <- Lenses.updateSchemas(schemaArg, workspace, true)(Lenses.workspace.imports(_))(_.modify(_)(_ :+ schemaRef))
      io            <- ~io.save(workspace, layout.furyConfig)
    } yield io.await()
  }
  
  def delete(ctx: SchemaCtx) = {
    import ctx._
    for {
      cli       <- cli.hint(SchemaArg, workspace.schemas.map(_.id))
      schemaArg <- ~cli.peek(SchemaArg)
      dSchema   <- ~workspace.schemas.findBy(schemaArg.getOrElse(workspace.main)).opt
      cli       <- cli.hint(ImportArg, dSchema.map(_.imports).getOrElse(Nil))
      io        <- cli.io()
      schemaId  <- io(SchemaArg).remedy(workspace.main)
      importArg <- io(ImportArg)
      schema    <- workspace.schemas.findBy(schemaId)
      lens      <- ~Lenses.workspace.imports(schema.id)
      workspace <- ~lens.modify(workspace)(_.filterNot(_ == importArg))
      io        <- ~io.save(workspace, layout.furyConfig)
    } yield io.await()
  }
  

  def list(ctx: SchemaCtx) = {
    import ctx._
    for {
      cli       <- cli.hint(SchemaArg, workspace.schemas.map(_.id))
      schemaArg <- ~cli.peek(SchemaArg).getOrElse(workspace.main)
      schema    <- workspace.schemas.findBy(schemaArg)
      cols      <- Answer(Terminal.columns(cli.env).getOrElse(100))
      cli       <- cli.hint(RawArg)
      io        <- cli.io()
      raw       <- ~io(RawArg).successful
      rows      <- schema.importedSchemas(layout, cli.shell)
      table     <- ~Tables(config).show(Tables(config).schemas(Some(workspace.main)), cols, rows, raw)(_.id)
      io        <- ~(if(!raw) io.println(Tables(config).contextString(layout.pwd, workspace.showSchema, schema)) else io)
      io        <- ~io.println(UserMsg { theme => table.mkString("\n") })
    } yield io.await()
  }
}
