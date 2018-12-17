/*
  Fury, version 0.1.2. Copyright 2018 Jon Pretty, Propensive Ltd.

  The primary distribution site is: https://propensive.com/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required  by applicable  law or  agreed to  in writing,  software  distributed  under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
  express  or  implied.  See  the  License for  the specific  language  governing  permissions and
  limitations under the License.
                                                                                                  */
package fury

import Args._

import mitigation._
import guillotine._

case class SchemaCtx(cli: Cli[CliParam[_]], layout: Layout, config: Config, layer: Layer)

object SchemaCli {

  def context(cli: Cli[CliParam[_]]) = for {
    layout     <- cli.layout
    config     <- fury.Config.read()(cli.env, layout)
    layer      <- Layer.read(layout.furyConfig)(layout)
  } yield SchemaCtx(cli, layout, config, layer)

  def select(ctx: SchemaCtx) = {
    import ctx._
    for {
      cli           <- ctx.cli.hint(SchemaArg, ctx.layer.schemas.map(_.id))
      io            <- cli.io()
      schemaId      <- io(SchemaArg)
      lens          <- ~Lenses.layer.mainSchema
      layer         <- ~(lens(layer) = schemaId)
      _             <- ~io.save(layer, layout.furyConfig)
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
      rows      <- ~layer.schemas.to[List]
      table     <- ~Tables(config).show(Tables(config).schemas(Some(schema.id)), cols, rows, raw)(_.id)
      _         <- ~(if(!raw) io.println(Tables(config).contextString(layout.pwd, layer.showSchema, schema)))
      _         <- ~io.println(UserMsg { theme => table.mkString("\n") })
    } yield io.await()
  }

  def diff(ctx: SchemaCtx) = {
    import ctx._
    for {
      cli       <- ctx.cli.hint(SchemaArg, ctx.layer.schemas.map(_.id))
      cli       <- ctx.cli.hint(CompareArg, ctx.layer.schemas.map(_.id))
      cli       <- cli.hint(RawArg)
      io        <- cli.io()
      raw       <- ~io(RawArg).successful
      schemaArg <- io(SchemaArg).remedy(layer.main)
      schema    <- layer.schemas.findBy(schemaArg)
      otherArg  <- io(CompareArg)
      other     <- layer.schemas.findBy(otherArg)
      cols      <- Answer(Terminal.columns(cli.env).getOrElse(100))
      rows      <- ~Diff.gen[Schema].diff(schema, other)
      table     <- ~Tables(config).show(Tables(config).differences(schema.id.key, other.id.key), cols, rows, raw)(_.label)
      _         <- ~(if(!raw) io.println(Tables(config).contextString(layout.pwd, layer.showSchema, schema)))
      _         <- ~io.println(UserMsg { theme => table.mkString("\n") })
    } yield io.await()
  }

  def update(ctx: SchemaCtx) = {
    import ctx._
    for {
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      cli       <- cli.hint(SchemaNameArg)
      io        <- cli.io()
      name      <- io(SchemaNameArg)
      schemaId  <- io(SchemaArg).remedy(layer.main)
      schema    <- layer.schemas.findBy(schemaId)
      newSchema <- ~schema.copy(id = name)
      lens      <- ~Lenses.layer.schemas
      layer     <- ~lens.modify(layer)(_ - schema + newSchema)
      layer     <- ~(if(layer.main == schema.id) layer.copy(main = newSchema.id) else layer)
      _         <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }
  
  def add(ctx: SchemaCtx) = {
    import ctx._
    for {
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      cli       <- cli.hint(SchemaNameArg)
      io        <- cli.io()
      name      <- io(SchemaNameArg)
      schemaId  <- io(SchemaArg).remedy(layer.main)
      schema    <- layer.schemas.findBy(schemaId)
      newSchema <- ~schema.copy(id = name)
      lens      <- ~Lenses.layer.schemas
      layer     <- ~lens.modify(layer)(_ + newSchema)
      layer     <- ~layer.copy(main = newSchema.id)
      _         <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }
  
  def delete(ctx: SchemaCtx) = {
    import ctx._
    for {
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      io        <- cli.io()
      schemaId  <- io(SchemaArg).remedy(layer.main)
      schema    <- layer.schemas.findBy(schemaId)
      lens      <- ~Lenses.layer.schemas
      layer     <- ~lens.modify(layer)(_.filterNot(_.id == schema.id))
      _         <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }
  
}
  
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
      _             <- ~io.save(layer, layout.furyConfig)
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
      _         <- ~io.save(layer, layout.furyConfig)
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
      _         <- ~(if(!raw) io.println(Tables(config).contextString(layout.pwd, layer.showSchema, schema)))
      _         <- ~io.println(UserMsg { theme => table.mkString("\n") })
    } yield io.await()
  }
}

object RepoCli {
  def list(ctx: SchemaCtx) = {
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
      _         <- ~(if(!raw) io.println(Tables(config).contextString(layout.pwd, layer.showSchema, schema)))
      _         <- ~io.println(UserMsg { theme => table.mkString("\n") })
    } yield io.await()
  }

  def fork(ctx: SchemaCtx) = {
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
      _         <- ~cli.shell.git.sparseCheckout(bareRepo, dir, List(), repo.refSpec.id)
      newRepo   <- ~repo.copy(local = Some(dir))
      lens      <- ~Lenses.layer.repos(schema.id)
      layer     <- ~(lens.modify(layer)(_ - repo + newRepo))
      _         <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }

  def pull(ctx: SchemaCtx) = {
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
      _         <- ~msgs.foreach(io.println(_))
    } yield io.await()
  }

  def add(ctx: SchemaCtx) = {
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
      _              <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }

  def update(ctx: SchemaCtx) = {
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

  def delete(ctx: SchemaCtx) = {
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
      _         <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }
}
