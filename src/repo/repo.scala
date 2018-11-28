package fury

import Args._

import mitigation._
import guillotine._

object RepoCli {
  
  def context(cli: Cli[CliParam[_]]) = for {
    layout     <- cli.layout
    config     <- fury.Config.read()(cli.env, layout)
    layer      <- Layer.read(layout.furyConfig)(layout)
  } yield Context(cli, layout, config, layer)

  case class Context(cli: Cli[CliParam[_]], layout: Layout, config: Config, layer: Layer)
  
  def list(ctx: Context) = {
  import ctx._
    for {
      cli       <- ctx.cli.hint(SchemaArg, ctx.layer.schemas.map(_.id))
      cols      <- Answer(Terminal.columns(cli.env).getOrElse(100))
      cli       <- cli.hint(RawArg)
      io        <- cli.io()
      raw       <- ~io(RawArg).successful
      schemaArg <- io(SchemaArg).remedy(ctx.layer.main)
      schema    <- ctx.layer.schemas.findBy(schemaArg)
      rows      <- schema.allRepos(ctx.layout, cli.shell).map(_.to[List].sortBy(_.id))
      table     <- ~Tables(config).show(Tables(config).repositories(ctx.layout, cli.shell), cols, rows, raw)(_.id)
      io        <- ~(if(!raw) io.println(Tables(config).contextString(layout.pwd, layer.showSchema, schema)) else io)
      io        <- ~io.println(UserMsg { theme => table.mkString("\n") })
    } yield io.await()
  }

  def fork(ctx: Context) = {
    import ctx._
    for {
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      schemaArg <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaArg)
      cli       <- cli.hint(DirArg)
      cli       <- cli.hint(RetryArg)
      cli       <- cli.hint(RepoIdArg, schema.repos)
      io        <- cli.io()
      repoId    <- io(RepoIdArg)
      repo      <- schema.repos.findBy(repoId)
      dir       <- io(DirArg)
      retry     <- ~io(RetryArg).successful
      bareRepo  <- repo.repo.fetch(layout, cli.shell)
      io        <- ~io.map { _ => cli.shell.git.sparseCheckout(bareRepo, dir, List(), repo.refSpec.id) }
      newRepo   <- ~repo.copy(local = Some(dir))
      lens      <- ~Lenses.layer.repos(schema.id)
      layer     <- ~(lens.modify(layer)(_ - repo + newRepo))
      io        <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }

  def pull(ctx: Context) = {
    import ctx._
    for {
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      schemaArg <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaArg)
      cli       <- cli.hint(RepoIdArg, schema.repos)
      cli       <- cli.hint(AllArg, Nil)
      io        <- cli.io()
      all       <- ~io(AllArg).opt
      optRepos  <- io(RepoIdArg).opt.map(scala.collection.immutable.SortedSet(_)).orElse(all.map(_ => schema.repos.map(_.id))).ascribe(exoskeleton.MissingArg("repo"))
      repos     <- optRepos.map(schema.repo(_)).sequence
      msgs      <- repos.map(_.repo.update()(cli.shell, layout)).sequence
      io        <- ~msgs.foldLeft(io)(_.println(_))
    } yield io.await()
  }

  def add(ctx: Context) = {
    import ctx._
    for {
      cli            <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      cli            <- cli.hint(RepoArg)
      cli            <- cli.hint(DirArg)
      cli            <- cli.hint(RetryArg)
      cli            <- cli.hint(ImportArg2)
      projectNameOpt <- ~cli.peek(RepoArg).map(fury.Repo.fromString).flatMap(_.projectName.opt)
      cli            <- cli.hint(RepoNameArg, projectNameOpt)
      cli            <- cli.hint(VersionArg)
      io             <- cli.io()
      optImport      <- ~io(ImportArg2).opt
      optSchemaArg   <- ~io(SchemaArg).opt
      schemaArg      <- ~optSchemaArg.getOrElse(layer.main)
      schema         <- layer.schemas.findBy(schemaArg)
      remote         <- ~io(RepoArg).opt
      retry          <- ~io(RetryArg).successful
      dir            <- ~io(DirArg).opt
      version        <- ~io(VersionArg).opt.getOrElse(RefSpec.master)
      repo           <- ~remote.map(fury.Repo.fromString(_))
      suggested      <- (repo.flatMap(_.projectName.opt): Option[RepoId]).orElse(dir.map { d => RepoId(d.value.split("/").last) }).ascribe(exoskeleton.MissingArg("repo"))
      nameArg        <- ~io(RepoNameArg).opt.getOrElse(suggested)
      sourceRepo     <- repo.map(SourceRepo(nameArg, _, version, dir)).orElse(dir.map { d =>
                          SourceRepo(nameArg, fury.Repo(""), RefSpec.master, Some(d))
                        }).ascribe(exoskeleton.MissingArg("repo"))
      lens           <- ~Lenses.layer.repos(schema.id)
      layer          <- ~(lens.modify(layer)(_ + sourceRepo))
      optImportRef   <- ~optImport.map(SchemaRef(sourceRepo.id, _))
      layer          <- optImportRef.map { importRef =>
                          Lenses.updateSchemas(optSchemaArg, layer, true)(Lenses.layer.imports(_))(_.modify(_)(_ :+ importRef))
                        }.getOrElse(~layer)
      _              <- sourceRepo.repo.fetch(layout, cli.shell)
      io             <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }

  def update(ctx: Context) = {
    import ctx._
    for {
      cli       <- cli.hint(AllArg)
      schemaArg <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaArg)
      cli       <- cli.hint(RepoIdArg, schema.repos)
      io        <- cli.io()
      repoId    <- io(RepoIdArg).opt.ascribe(UnspecifiedRepo())
      all       <- ~io(AllArg).successful
      repos     <- if(all) ~schema.repos else schema.repos.findBy(repoId)
    } yield io.await()
  }

  def delete(ctx: Context) = {
    import ctx._
    for {
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      schemaArg <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaArg)
      cli       <- cli.hint(RepoIdArg, schema.repos)
      io        <- cli.io()
      repoId    <- io(RepoIdArg)
      repo      <- schema.repos.findBy(repoId)
      lens      <- ~Lenses.layer.repos(schema.id)
      layer     <- ~(lens(layer) -= repo)
      io        <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }
}
