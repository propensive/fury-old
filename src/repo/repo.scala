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

import fury.strings._, fury.io._, fury.core._

import Args._

import guillotine._
import optometry._
import mercator._
import scala.util._

import Lenses._

object RepoCli {

  def mkContext(cli: Cli[CliParam[_]]) =
    for {
      layout <- cli.layout
      config <- Config.read()(cli.env, layout)
      layer  <- Layer.read(Io.silent(config), layout.layerFile, layout)
    } yield Context(cli, layout, config, layer)

  case class Context(cli: Cli[CliParam[_]], layout: Layout, config: Config, layer: Layer)

  def list(ctx: Context): Try[ExitStatus] = {
    import ctx._
    for {
      cli       <- ctx.cli.hint(SchemaArg, ctx.layer.schemas.map(_.id))
      cli       <- cli.hint(RawArg)
      invoc     <- cli.read()
      raw       <- ~invoc(RawArg).isSuccess
      schemaArg <- ~invoc(SchemaArg).toOption.getOrElse(ctx.layer.main)
      schema    <- ctx.layer.schemas.findBy(schemaArg)
      rows      <- ~schema.repos.to[List].sortBy(_.id)
      io        <- invoc.io()
      table     <- ~Tables(config).show(Tables(config).repositories(layout), cli.cols, rows, raw)(_.id)
      _ <- ~(if (!raw)
               io.println(Tables(config).contextString(layout.base, layer.showSchema, schema)))
      _ <- ~io.println(table.mkString("\n"))
    } yield io.await()
  }

  def unfork(ctx: Context): Try[ExitStatus] = {
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
      _         <- ~Layer.save(io, layer, layout)
    } yield io.await()
  }

  def fork(ctx: Context): Try[ExitStatus] = {
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
      absPath <- (for {
                  absPath <- dir.absolutePath()
                  _       <- Try(absPath.mkdir())
                  _ <- if (absPath.empty) Success(())
                      else Failure(new Exception("Non-empty dir exists"))
                } yield absPath).orElse(Failure(exoskeleton.InvalidArgValue("dir", dir.value)))

      _ <- ~layout.shell.git.sparseCheckout(
              bareRepo,
              absPath,
              List(),
              refSpec = repo.track.id,
              commit = repo.commit.id)
      newRepo <- ~repo.copy(local = Some(absPath))
      lens    <- ~Lenses.layer.repos(schema.id)
      layer   <- ~(lens.modify(layer)(_ - repo + newRepo))
      _       <- ~Layer.save(io, layer, layout)
    } yield io.await()
  }

  def pull(ctx: Context): Try[ExitStatus] = {
    import ctx._
    for {
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      schemaArg <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaArg)
      cli       <- cli.hint(RepoArg, schema.repos)
      cli       <- cli.hint(AllArg, List[String]())
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
      _ <- ~Layer.save(io, newLayer, layout)
      _ <- ~newRepos.foreach {
            case (newRepo, _) =>
              io.println(s"Repo [${newRepo.id.key}] checked out to commit [${newRepo.commit.id}]")
          }
    } yield io.await()
  }

  def add(ctx: Context): Try[ExitStatus] = {
    import ctx._
    for {
      cli            <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      cli            <- cli.hint(UrlArg)
      cli            <- cli.hint(DirArg)
      projectNameOpt <- ~cli.peek(UrlArg).map(Repo.fromString).flatMap(_.projectName.toOption)
      cli            <- cli.hint(RepoNameArg, projectNameOpt)
      remoteOpt      <- ~cli.peek(UrlArg)
      repoOpt        <- ~remoteOpt.map(Repo.fromString(_))
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
      suggested <- ~(repoOpt.flatMap(_.projectName.toOption): Option[RepoId]).orElse(dir.map { d =>
                    RepoId(d.value.split("/").last)
                  })
      urlArg <- cli.peek(UrlArg).ascribe(exoskeleton.MissingArg("url"))
      repo   <- repoOpt.ascribe(exoskeleton.InvalidArgValue("url", urlArg))
      _      <- repo.fetch(io, layout)
      commit <- repo
                 .getCommitFromTag(layout, version)
                 .toOption
                 .ascribe(exoskeleton.InvalidArgValue("version", version.id))
      nameArg <- invoc(RepoNameArg).toOption
                  .orElse(suggested)
                  .ascribe(exoskeleton.MissingArg("name"))
      sourceRepo <- ~SourceRepo(nameArg, repo, version, Commit(commit), dir)
      lens       <- ~Lenses.layer.repos(schema.id)
      layer      <- ~(lens.modify(layer)(_ + sourceRepo))
      _          <- ~Layer.save(io, layer, layout)
    } yield io.await()
  }

  def update(ctx: Context): Try[ExitStatus] = {
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
      remote      <- ~remoteArg.map(Repo.fromString(_))
      nameArg     <- ~invoc(RepoNameArg).toOption
      force       <- ~invoc(ForceArg).toOption.isDefined
      focus       <- ~Lenses.focus(optSchemaId, force)
      layer       <- focus(layer, _.lens(_.repos(on(repo.id)).repo)) = remote
      layer       <- focus(layer, _.lens(_.repos(on(repo.id)).track)) = version
      layer       <- focus(layer, _.lens(_.repos(on(repo.id)).local)) = dir.map(Some(_))
      layer       <- focus(layer, _.lens(_.repos(on(repo.id)).id)) = nameArg
      _           <- ~Layer.save(io, layer, layout)
    } yield io.await()
  }

  def remove(ctx: Context): Try[ExitStatus] = {
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
      _         <- ~Layer.save(io, layer, layout)
    } yield io.await()
  }
}
