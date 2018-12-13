package fury

object ImportCli {

  def add(ctx: SchemaCtx) = {
    import ctx._
    for {
      cli           <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      schemaArg     <- ~cli.peek(SchemaArg)
      defaultSchema <- ~layer.schemas.findBy(schemaArg.getOrElse(layer.main)).opt
      cli           <- cli.hint(ImportArg, defaultSchema.map(_.importCandidates(layout, cli.shell)).getOrElse(Nil))
      io            <- cli.io()
      schemaRef     <- io(ImportArg)
      layer         <- Lenses.updateSchemas(schemaArg, layer, true)(Lenses.layer.imports(_))(_.modify(_)(_ :+ schemaRef))
      io            <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }
  
  def delete(ctx: SchemaCtx) = {
    import ctx._
    for {
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      schemaArg <- ~cli.peek(SchemaArg)
      dSchema   <- ~layer.schemas.findBy(schemaArg.getOrElse(layer.main)).opt
      cli       <- cli.hint(ImportArg, dSchema.map(_.imports).getOrElse(Nil))
      io        <- cli.io()
      schemaId  <- io(SchemaArg).remedy(layer.main)
      importArg <- io(ImportArg)
      schema    <- layer.schemas.findBy(schemaId)
      lens      <- ~Lenses.layer.imports(schema.id)
      layer     <- ~lens.modify(layer)(_.filterNot(_ == importArg))
      io        <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }
  

  def list(ctx: SchemaCtx) = {
    import ctx._
    for {
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      schemaArg <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaArg)
      cols      <- Answer(Terminal.columns(cli.env).getOrElse(100))
      cli       <- cli.hint(RawArg)
      io        <- cli.io()
      raw       <- ~io(RawArg).successful
      rows      <- schema.importedSchemas(layout, cli.shell)
      table     <- ~Tables(config).show(Tables(config).schemas(Some(layer.main)), cols, rows, raw)(_.id)
      io        <- ~(if(!raw) io.println(Tables(config).contextString(layout.pwd, layer.showSchema, schema)) else io)
      io        <- ~io.println(UserMsg { theme => table.mkString("\n") })
    } yield io.await()
  }
}
