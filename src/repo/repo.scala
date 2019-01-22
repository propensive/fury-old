/*
  Fury, version 0.2.2. Copyright 2019 Jon Pretty, Propensive Ltd.

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

import fury.error._

import Args._

import guillotine._
import mercator._
import scala.util._

object RepoCli {

  def context(cli: Cli[CliParam[_]]) =
    for {
      layout <- cli.layout
      config <- fury.Config.read()(cli.env, layout)
      layer  <- Layer.read(layout.furyConfig)(layout, cli.shell)
    } yield Context(cli, layout, config, layer)

  case class Context(cli: Cli[CliParam[_]], layout: Layout, config: Config, layer: Layer)

  def list(ctx: Context) = {
    import ctx._
    for {
      cli       <- ctx.cli.hint(SchemaArg, ctx.layer.schemas.map(_.id))
      cols      <- Success(Terminal.columns(cli.env).getOrElse(100))
      cli       <- cli.hint(RawArg)
      invoc     <- cli.read()
      raw       <- ~invoc(RawArg).isSuccess
      schemaArg <- ~invoc(SchemaArg).toOption.getOrElse(ctx.layer.main)
      schema    <- ctx.layer.schemas.findBy(schemaArg)
      rows      <- ~schema.repos.to[List].sortBy(_.id)
      io        <- invoc.io()
      table <- ~Tables(config)
                .show(Tables(config).repositories(ctx.layout, cli.shell), cols, rows, raw)(_.id)
      _ <- ~(if (!raw)
               io.println(Tables(config).contextString(layout.pwd, layer.showSchema, schema)))
      _ <- ~io.println(UserMsg { theme =>
            table.mkString("\n")
          })
    } yield io.await()
  }

  def unfork(ctx: Context) = {
    import ctx._
    for {
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      schemaArg <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaArg)
      cli       <- cli.hint(RepoIdArg, schema.repos)
      invoc     <- cli.read()
      io        <- invoc.io()
      repoId    <- invoc(RepoIdArg)
      repo      <- schema.repos.findBy(repoId)
      newRepo   <- ~repo.copy(local = None)
      lens      <- ~Lenses.layer.repos(schema.id)
      layer     <- ~(lens.modify(layer)(_ - repo + newRepo))
      _         <- ~io.save(layer, layout.furyConfig)
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
      invoc     <- cli.read()
      io        <- invoc.io()
      repoId    <- invoc(RepoIdArg)
      repo      <- schema.repos.findBy(repoId)
      dir       <- invoc(DirArg)
      retry     <- ~invoc(RetryArg).isSuccess
      bareRepo  <- repo.repo.fetch(layout, cli.shell)
      _ <- ~cli.shell.git
            .sparseCheckout(bareRepo, dir, List(), refSpec = repo.track.id, commit = repo.commit.id)
      newRepo <- ~repo.copy(local = Some(dir))
      lens    <- ~Lenses.layer.repos(schema.id)
      layer   <- ~(lens.modify(layer)(_ - repo + newRepo))
      _       <- ~io.save(layer, layout.furyConfig)
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
      invoc     <- cli.read()
      io        <- invoc.io()
      all       <- ~invoc(AllArg).toOption
      optRepos <- invoc(RepoIdArg).toOption
                   .map(scala.collection.immutable.SortedSet(_))
                   .orElse(all.map(_ => schema.repos.map(_.id)))
                   .ascribe(exoskeleton.MissingArg("repo"))
      repos <- optRepos.map(schema.repo(_)(layout, cli.shell)).sequence
      io    <- invoc.io()
      msgs  <- repos.map(_.repo.update()(cli.shell, layout).map(io.println(_))).sequence
      lens  <- ~Lenses.layer.repos(schema.id)
      newRepos <- repos
                   .map(
                       repo =>
                         for {
                           commit <- repo.repo
                                      .getCommitFromTag(layout, cli.shell, repo.track)
                                      .map(Commit(_))
                           newRepo = repo.copy(commit = commit)
                         } yield (newRepo, repo)
                   )
                   .sequence
      newLayer = newRepos.foldLeft(layer) { (layer, repoDiff) =>
        repoDiff match { case (newRepo, oldRepo) => lens.modify(layer)(_ - oldRepo + newRepo) }
      }
      _ <- ~io.save(newLayer, layout.furyConfig)
      _ <- ~newRepos.foreach {
            case (newRepo, _) =>
              io.println(s"Repo [${newRepo.id.key}] checked out to commit [${newRepo.commit.id}]")
          }
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
      projectNameOpt <- ~cli.peek(RepoArg).map(fury.Repo.fromString).flatMap(_.projectName.toOption)
      cli            <- cli.hint(RepoNameArg, projectNameOpt)
      remoteOpt      <- ~cli.peek(RepoArg)
      repoOpt        <- ~remoteOpt.map(fury.Repo.fromString(_))
      versions <- repoOpt.map { repo =>
                   cli.shell.git.lsRemote(repo.url)
                 }.to[List].sequence.map(_.flatten).recover { case e => Nil }
      cli          <- cli.hint(VersionArg, versions)
      invoc        <- cli.read()
      io           <- invoc.io()
      optImport    <- ~invoc(ImportArg2).toOption
      optSchemaArg <- ~invoc(SchemaArg).toOption
      schemaArg    <- ~optSchemaArg.getOrElse(layer.main)
      schema       <- layer.schemas.findBy(schemaArg)
      remote       <- ~invoc(RepoArg).toOption
      retry        <- ~invoc(RetryArg).isSuccess
      dir          <- ~invoc(DirArg).toOption
      version      <- ~invoc(VersionArg).toOption.getOrElse(RefSpec.master)
      repo         <- ~remote.map(fury.Repo.fromString(_))
      suggested <- ~(repo.flatMap(_.projectName.toOption): Option[RepoId]).orElse(dir.map { d =>
                    RepoId(d.value.split("/").last)
                  })
      nameArg <- invoc(RepoNameArg).toOption
                  .orElse(suggested)
                  .ascribe(exoskeleton.MissingArg("name"))
      _ <- repo.map(_.fetch(layout, cli.shell)).ascribe(exoskeleton.MissingArg("repo"))
      tag <- repo
              .map(_.getCommitFromTag(layout, cli.shell, version))
              .getOrElse(Failure(exoskeleton.MissingArg("name")))
      sourceRepo <- repo
                     .map(SourceRepo(nameArg, _, version, Commit(tag), dir))
                     .orElse(dir.map { d =>
                       SourceRepo(nameArg, fury.Repo(""), RefSpec.master, Commit(tag), Some(d))
                     })
                     .ascribe(exoskeleton.MissingArg("repo"))

      lens         <- ~Lenses.layer.repos(schema.id)
      optImportRef <- ~optImport.map(SchemaRef(sourceRepo.id, _))
      layer <- optImportRef.map { importRef =>
                Lenses.updateSchemas(optSchemaArg, layer, true)(Lenses.layer.imports(_))(
                    _.modify(_)(_ :+ importRef))
              }.getOrElse(~layer)
      layer <- ~(lens.modify(layer)(_ + sourceRepo))
      _     <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }

  def update(ctx: Context) = {
    import ctx._
    for {
      cli       <- cli.hint(AllArg)
      schemaArg <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaArg)
      cli       <- cli.hint(RepoIdArg, schema.repos)
      invoc     <- cli.read()
      io        <- invoc.io()
      repoId    <- invoc(RepoIdArg).toOption.ascribe(UnspecifiedRepo())
      all       <- ~invoc(AllArg).isSuccess
      repos     <- if (all) ~schema.repos else schema.repos.findBy(repoId)
    } yield io.await()
  }

  def remove(ctx: Context) = {
    import ctx._
    for {
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      schemaArg <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaArg)
      cli       <- cli.hint(RepoIdArg, schema.repos)
      invoc     <- cli.read()
      io        <- invoc.io()
      repoId    <- invoc(RepoIdArg)
      repo      <- schema.repos.findBy(repoId)
      lens      <- ~Lenses.layer.repos(schema.id)
      layer     <- ~(lens(layer) -= repo)
      _         <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }
}
