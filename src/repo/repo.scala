/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.7.13. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
   ║                                                                                                           ║
   ║ The primary distribution site is: https://propensive.com/                                                 ║
   ║                                                                                                           ║
   ║ Licensed under  the Apache License,  Version 2.0 (the  "License"); you  may not use  this file  except in ║
   ║ compliance with the License. You may obtain a copy of the License at                                      ║
   ║                                                                                                           ║
   ║     http://www.apache.org/licenses/LICENSE-2.0                                                            ║
   ║                                                                                                           ║
   ║ Unless required  by applicable law  or agreed to in  writing, software  distributed under the  License is ║
   ║ distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. ║
   ║ See the License for the specific language governing permissions and limitations under the License.        ║
   ╚═══════════════════════════════════════════════════════════════════════════════════════════════════════════╝
*/
package fury

import fury.strings._, fury.io._, fury.core._, fury.model._

import Args._

import guillotine._
import optometry._
import mercator._
import scala.util._

import Lenses._

object RepoCli {

  case class Context(cli: Cli[CliParam[_]], layout: Layout, layer: Layer)
  
  def context(cli: Cli[CliParam[_]]) = for {
    layout <- cli.layout
    layer  <- Layer.read(Log.silent, layout)
  } yield Context(cli, layout, layer)

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
      log       <- invoc.logger()
      table     <- ~Tables().show(Tables().repositories(layout), cli.cols, rows, raw)(_.id)
      _         <- ~(if(!raw) log.println(Tables().contextString(layout.baseDir, layer.showSchema, schema)))
      _         <- ~log.println(UserMsg { theme => table.mkString("\n") })
    } yield log.await()
  }

  def unfork(ctx: Context): Try[ExitStatus] = {
    import ctx._
    for {
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      schemaArg <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaArg)
      cli       <- cli.hint(RepoArg, schema.repos)
      invoc     <- cli.read()
      log       <- invoc.logger()
      repoId    <- invoc(RepoArg)
      repo      <- schema.repos.findBy(repoId)
      newRepo   <- ~repo.copy(local = None)
      lens      <- ~Lenses.layer.repos(schema.id)
      layer     <- ~(lens.modify(layer)(_ - repo + newRepo))
      _         <- ~Layer.save(log, layer, layout)
    } yield log.await()
  }

  def fork(ctx: Context): Try[ExitStatus] = {
    import ctx._
    for {
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      schemaArg <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaArg)
      cli       <- cli.hint(DirArg)
      cli       <- cli.hint(RepoArg, schema.repos)
      cli       <- cli.hint(HttpsArg)
      invoc     <- cli.read()
      log       <- invoc.logger()
      repoId    <- invoc(RepoArg)
      repo      <- schema.repos.findBy(repoId)
      dir       <- invoc(DirArg)
      https     <- ~invoc(HttpsArg).isSuccess
      bareRepo  <- repo.repo.fetch(log, layout, https)
      absPath   <- { for {
                     absPath <- ~(layout.pwd.resolve(dir))
                     _       <- Try(absPath.mkdir())

                     _       <- if(absPath.empty) Success(())
                                else Failure(new Exception("Non-empty dir exists"))

                   } yield absPath }.orElse(Failure(exoskeleton.InvalidArgValue("dir", dir.value)))

      _         <- ~Shell(layout.env).git.sparseCheckout(bareRepo, absPath, List(), refSpec = repo.track.id, commit =
                       repo.commit.id)

      newRepo   <- ~repo.copy(local = Some(absPath))
      lens      <- ~Lenses.layer.repos(schema.id)
      layer     <- ~(lens.modify(layer)(_ - repo + newRepo))
      _         <- ~Layer.save(log, layer, layout)
    } yield log.await()
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
      log       <- invoc.logger()
      all       <- ~invoc(AllArg).toOption
      
      optRepos  <- invoc(RepoArg).toOption.map(scala.collection.immutable.SortedSet(_)).orElse(all.map(_ =>
                       schema.repos.map(_.id))).ascribe(exoskeleton.MissingArg("repo"))

      repos     <- optRepos.map(schema.repo(_, layout)).sequence
      log       <- invoc.logger()
      msgs      <- repos.map(_.repo.update(layout).map(log.info(_))).sequence
      lens      <- ~Lenses.layer.repos(schema.id)

      newRepos  <- repos.map { repo => for {
                      commit  <- repo.repo.getCommitFromTag(layout, repo.track)
                      newRepo = repo.copy(commit = commit)
                   } yield (newRepo, repo) }.sequence

      newLayer   = newRepos.foldLeft(layer) { (layer, repoDiff) => repoDiff match {
                     case (newRepo, oldRepo) => lens.modify(layer)(_ - oldRepo + newRepo) }
                   }

      _         <- ~Layer.save(log, newLayer, layout)

      _         <- ~newRepos.foreach { case (newRepo, _) =>
                     log.info(msg"Repository ${newRepo} checked out to commit ${newRepo.commit}")
                   }

    } yield log.await()
  }

  def add(ctx: Context): Try[ExitStatus] = {
    import ctx._
    for {
      cli            <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      cli            <- cli.hint(UrlArg)
      cli            <- cli.hint(DirArg)
      cli            <- cli.hint(HttpsArg)
      projectNameOpt <- ~cli.peek(UrlArg).map(Repo(_)).flatMap(_.projectName.toOption)
      cli            <- cli.hint(RepoNameArg, projectNameOpt)
      remoteOpt      <- ~cli.peek(UrlArg)
      repoOpt        <- ~remoteOpt.map(Repo(_))
      
      versions       <- repoOpt.map { repo =>
                          Shell(layout.env).git.lsRemote(Repo.fromString(repo.ref, true))
                        }.to[List].sequence.map(_.flatten).recover { case e => Nil }

      cli            <- cli.hint(VersionArg, versions)
      invoc          <- cli.read()
      log            <- invoc.logger()
      optSchemaArg   <- ~invoc(SchemaArg).toOption
      schemaArg      <- ~optSchemaArg.getOrElse(layer.main)
      schema         <- layer.schemas.findBy(schemaArg)
      remote         <- ~invoc(UrlArg).toOption
      dir            <- ~invoc(DirArg).toOption
      https          <- ~invoc(HttpsArg).isSuccess
      version        <- ~invoc(VersionArg).toOption.getOrElse(RefSpec.master)

      suggested      <- ~(repoOpt.flatMap(_.projectName.toOption): Option[RepoId]).orElse(dir.map { d =>
                          RepoId(d.value.split("/").last)
                        })

      urlArg         <- cli.peek(UrlArg).ascribe(exoskeleton.MissingArg("url"))
      repo           <- repoOpt.ascribe(exoskeleton.InvalidArgValue("url", urlArg))
      _              <- repo.fetch(log, layout, https)

      commit         <- repo.getCommitFromTag(layout, version).toOption.ascribe(
                            exoskeleton.InvalidArgValue("version", version.id))

      nameArg        <- invoc(RepoNameArg).toOption.orElse(suggested).ascribe(exoskeleton.MissingArg("name"))
      sourceRepo     <- ~SourceRepo(nameArg, repo, version, commit, dir)
      lens           <- ~Lenses.layer.repos(schema.id)
      layer          <- ~(lens.modify(layer)(_ + sourceRepo))
      _              <- ~Layer.save(log, layer, layout)
    } yield log.await()
  }

  def update(ctx: Context): Try[ExitStatus] = {
    import ctx._
    for {
      cli         <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      cli         <- cli.hint(DirArg)
      cli         <- cli.hint(UrlArg)
      cli         <- cli.hint(ForceArg)
      schemaArg   <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema      <- layer.schemas.findBy(schemaArg)
      cli         <- cli.hint(RepoArg, schema.repos)
      optRepo     <- ~cli.peek(RepoArg).flatMap(schema.repos.findBy(_).toOption)
      versions    <- optRepo.to[List].map(_.repo.path(layout)).map(Shell(layout.env).git.showRefs(_)).sequence
      cli         <- cli.hint(VersionArg, versions.flatten)
      invoc       <- cli.read()
      log         <- invoc.logger()
      optSchemaId <- ~invoc(SchemaArg).toOption
      schemaArg   <- ~optSchemaId.getOrElse(layer.main)
      schema      <- layer.schemas.findBy(schemaArg)
      repoArg     <- invoc(RepoArg)
      repo        <- schema.repos.findBy(repoArg)
      dir         <- ~invoc(DirArg).toOption
      version     <- ~invoc(VersionArg).toOption
      remoteArg   <- ~invoc(UrlArg).toOption
      remote      <- ~remoteArg.map(Repo(_))
      nameArg     <- ~invoc(RepoNameArg).toOption
      force       <- ~invoc(ForceArg).isSuccess
      focus       <- ~Lenses.focus(optSchemaId, force)
      layer       <- focus(layer, _.lens(_.repos(on(repo.id)).repo)) = remote
      layer       <- focus(layer, _.lens(_.repos(on(repo.id)).track)) = version
      layer       <- focus(layer, _.lens(_.repos(on(repo.id)).local)) = dir.map(Some(_))
      layer       <- focus(layer, _.lens(_.repos(on(repo.id)).id)) = nameArg
      _           <- ~Layer.save(log, layer, layout)
    } yield log.await()
  }

  def remove(ctx: Context): Try[ExitStatus] = {
    import ctx._
    for {
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      schemaArg <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaArg)
      cli       <- cli.hint(RepoArg, schema.repos)
      invoc     <- cli.read()
      log       <- invoc.logger()
      repoId    <- invoc(RepoArg)
      repo      <- schema.repos.findBy(repoId)
      lens      <- ~Lenses.layer.repos(schema.id)
      layer     <- ~(lens(layer) -= repo)
      _         <- ~Layer.save(log, layer, layout)
    } yield log.await()
  }
}
