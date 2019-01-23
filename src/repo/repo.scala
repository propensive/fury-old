/*
  Fury, version 0.4.0. Copyright 2018-19 Jon Pretty, Propensive Ltd.

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
import optometry._
import mercator._
import scala.util._

import Lenses._

object RepoCli {

  def context(cli: Cli[CliParam[_]]) =
    for {
      layout <- cli.layout
      config <- fury.Config.read()(cli.env, layout)
      layer  <- Layer.read(Io.silent(config), layout.furyConfig, layout)
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
      table     <- ~Tables(config).show(Tables(config).repositories(layout), cols, rows, raw)(_.id)
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
      cli       <- cli.hint(RepoArg, schema.repos)
      invoc     <- cli.read()
      io        <- invoc.io()
      repoId    <- invoc(RepoArg)
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
      cli       <- cli.hint(RepoArg, schema.repos)
      invoc     <- cli.read()
      io        <- invoc.io()
      repoId    <- invoc(RepoArg)
      repo      <- schema.repos.findBy(repoId)
      dir       <- invoc(DirArg)
      bareRepo  <- repo.repo.fetch(io, layout)
      _ <- ~layout.shell.git
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
      cli       <- cli.hint(RepoArg, schema.repos)
      cli       <- cli.hint(AllArg, Nil)
      invoc     <- cli.read()
      io        <- invoc.io()
      all       <- ~invoc(AllArg).toOption
      optRepos <- invoc(RepoArg).toOption
                   .map(scala.collection.immutable.SortedSet(_))
                   .orElse(all.map(_ => schema.repos.map(_.id)))
                   .ascribe(exoskeleton.MissingArg("repo"))
      repos <- optRepos.map(schema.repo(_, layout)).sequence
      io    <- invoc.io()
      msgs  <- repos.map(_.repo.update(layout).map(io.println(_))).sequence
      lens  <- ~Lenses.layer.repos(schema.id)
      newRepos <- repos
                   .map(
                       repo =>
                         for {
                           commit  <- repo.repo.getCommitFromTag(layout, repo.track).map(Commit(_))
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
      cli            <- cli.hint(UrlArg)
      cli            <- cli.hint(DirArg)
      projectNameOpt <- ~cli.peek(UrlArg).map(fury.Repo.fromString).flatMap(_.projectName.toOption)
      cli            <- cli.hint(RepoNameArg, projectNameOpt)
      remoteOpt      <- ~cli.peek(UrlArg)
      repoOpt        <- ~remoteOpt.map(fury.Repo.fromString(_))
      versions <- repoOpt.map { repo =>
                   layout.shell.git.lsRemote(repo.url)
                 }.to[List].sequence.map(_.flatten).recover { case e => Nil }
      cli          <- cli.hint(VersionArg, versions)
      invoc        <- cli.read()
      io           <- invoc.io()
      optSchemaArg <- ~invoc(SchemaArg).toOption
      schemaArg    <- ~optSchemaArg.getOrElse(layer.main)
      schema       <- layer.schemas.findBy(schemaArg)
      remote       <- ~invoc(UrlArg).toOption
      dir          <- ~invoc(DirArg).toOption
      version      <- ~invoc(VersionArg).toOption.getOrElse(RefSpec.master)
      repo         <- ~remote.map(fury.Repo.fromString(_))
      suggested <- ~(repo.flatMap(_.projectName.toOption): Option[RepoId]).orElse(dir.map { d =>
                    RepoId(d.value.split("/").last)
                  })
      nameArg <- invoc(RepoNameArg).toOption
                  .orElse(suggested)
                  .ascribe(exoskeleton.MissingArg("name"))
      _ <- repo.map(_.fetch(io, layout)).ascribe(exoskeleton.MissingArg("repo"))
      tag <- repo
              .map(_.getCommitFromTag(layout, version))
              .getOrElse(Failure(exoskeleton.MissingArg("name")))
      sourceRepo <- repo
                     .map(SourceRepo(nameArg, _, version, Commit(tag), dir))
                     .orElse(dir.map { d =>
                       SourceRepo(nameArg, fury.Repo(""), RefSpec.master, Commit(tag), Some(d))
                     })
                     .ascribe(exoskeleton.MissingArg("repo"))

      lens  <- ~Lenses.layer.repos(schema.id)
      layer <- ~(lens.modify(layer)(_ + sourceRepo))
      _     <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }

  def update(ctx: Context) = {
    import ctx._
    for {
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      cli       <- cli.hint(DirArg)
      cli       <- cli.hint(UrlArg)
      cli       <- cli.hint(ForceArg)
      schemaArg <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaArg)
      cli       <- cli.hint(RepoArg, schema.repos)
      optRepo   <- ~cli.peek(RepoArg).flatMap(schema.repos.findBy(_).toOption)
      versions <- optRepo
                   .to[List]
                   .map(_.repo.path(layout))
                   .map(layout.shell.git.showRefs(_))
                   .sequence
      cli         <- cli.hint(VersionArg, versions.flatten)
      invoc       <- cli.read()
      io          <- invoc.io()
      optSchemaId <- ~invoc(SchemaArg).toOption
      schemaArg   <- ~optSchemaId.getOrElse(layer.main)
      schema      <- layer.schemas.findBy(schemaArg)
      repoArg     <- invoc(RepoArg)
      repo        <- schema.repos.findBy(repoArg)
      dir         <- ~invoc(DirArg).toOption
      version     <- ~invoc(VersionArg).toOption
      remoteArg   <- ~invoc(UrlArg).toOption
      remote      <- ~remoteArg.map(fury.Repo.fromString(_))
      nameArg     <- ~invoc(RepoNameArg).toOption
      force       <- ~invoc(ForceArg).toOption.isDefined
      focus       <- ~Lenses.focus(optSchemaId, force)
      layer       <- focus(layer, _.lens(_.repos(on(repo.id)).repo)) = remote
      layer       <- focus(layer, _.lens(_.repos(on(repo.id)).track)) = version
      layer       <- focus(layer, _.lens(_.repos(on(repo.id)).local)) = dir.map(Some(_))
      layer       <- focus(layer, _.lens(_.repos(on(repo.id)).id)) = nameArg
      _           <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }

  def remove(ctx: Context) = {
    import ctx._
    for {
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      schemaArg <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaArg)
      cli       <- cli.hint(RepoArg, schema.repos)
      invoc     <- cli.read()
      io        <- invoc.io()
      repoId    <- invoc(RepoArg)
      repo      <- schema.repos.findBy(repoId)
      lens      <- ~Lenses.layer.repos(schema.id)
      layer     <- ~(lens(layer) -= repo)
      _         <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }
}
