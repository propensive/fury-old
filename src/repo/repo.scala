/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.8.0. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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
  
  def context(cli: Cli[CliParam[_]])(implicit log: Log) = for {
    layout <- cli.layout
    layer  <- Layer.read(layout)
  } yield Context(cli, layout, layer)

  def list(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli       <- ctx.cli.hint(SchemaArg, ctx.layer.schemas.map(_.id))
      cli       <- cli.hint(RawArg)
      call     <- cli.call()
      raw       <- ~call(RawArg).isSuccess
      schemaArg <- ~call(SchemaArg).toOption.getOrElse(ctx.layer.main)
      schema    <- ctx.layer.schemas.findBy(schemaArg)
      rows      <- ~schema.repos.to[List].sortBy(_.id)
      table     <- ~Tables().show(Tables().repositories(layout), cli.cols, rows, raw)(_.id)
      _         <- ~(if(!raw) log.info(Tables().contextString(layer, layer.showSchema, schema)))
      _         <- ~log.rawln(table.mkString("\n"))
    } yield log.await()
  }

  def unfork(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      schemaArg <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaArg)
      cli       <- cli.hint(RepoArg, schema.repos)
      call     <- cli.call()
      repoId    <- call(RepoArg)
      repo      <- schema.repos.findBy(repoId)
      newRepo   <- ~repo.copy(local = None)
      lens      <- ~Lenses.layer.repos(schema.id)
      layer     <- ~(lens.modify(layer)(_ - repo + newRepo))
      _         <- ~Layer.save(layer, layout)
    } yield log.await()
  }

  def fork(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      schemaArg <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaArg)
      cli       <- cli.hint(DirArg)
      cli       <- cli.hint(RepoArg, schema.repos)
      cli       <- cli.hint(HttpsArg)
      call     <- cli.call()
      repoId    <- call(RepoArg)
      repo      <- schema.repos.findBy(repoId)
      dir       <- call(DirArg)
      https     <- ~call(HttpsArg).isSuccess
      bareRepo  <- repo.repo.fetch(layout, https)
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
      _         <- ~Layer.save(layer, layout)
    } yield log.await()
  }

  def pull(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      cli       <- cli.hint(HttpsArg)
      schemaArg <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaArg)
      cli       <- cli.hint(RepoArg, schema.repos)
      cli       <- cli.hint(AllArg, List[String]())
      call      <- cli.call()
      https     <- ~call(HttpsArg).isSuccess
      all       <- ~call(AllArg).toOption
      
      optRepos  <- call(RepoArg).toOption.map(scala.collection.immutable.SortedSet(_)).orElse(all.map(_ =>
                       schema.repos.map(_.id))).ascribe(exoskeleton.MissingArg("repo"))

      repos     <- optRepos.map(schema.repo(_, layout)).sequence
      succeeded <- ~repos.map(_.pull(layout, https)).forall(_.isSuccess)
      lens      <- ~Lenses.layer.repos(schema.id)

      newRepos  <- repos.map { repo => for {
                      commit  <- repo.repo.getCommitFromTag(layout, repo.track)
                      newRepo = repo.copy(commit = commit)
                   } yield (newRepo, repo) }.sequence

      newLayer   = newRepos.foldLeft(layer) { (layer, repoDiff) => repoDiff match {
                     case (newRepo, oldRepo) => lens.modify(layer)(_ - oldRepo + newRepo) }
                   }

      _         <- ~Layer.save(newLayer, layout)

      _         <- ~newRepos.foreach { case (newRepo, _) =>
                     log.info(msg"Repository ${newRepo} checked out to commit ${newRepo.commit}")
                   }

    } yield log.await()
  }

  def add(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli            <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      cli            <- cli.hint(UrlArg, GitHub.repos(cli.peek(UrlArg).getOrElse("")))
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
      call          <- cli.call()
      optSchemaArg   <- ~call(SchemaArg).toOption
      schemaArg      <- ~optSchemaArg.getOrElse(layer.main)
      schema         <- layer.schemas.findBy(schemaArg)
      dir            <- ~call(DirArg).toOption
      https          <- ~call(HttpsArg).isSuccess
      version        <- ~call(VersionArg).toOption.getOrElse(RefSpec.master)
      urlArg         <- cli.peek(UrlArg).ascribe(exoskeleton.MissingArg("url"))
      repo           <- repoOpt.ascribe(exoskeleton.InvalidArgValue("url", urlArg))
      suggested      <- repo.projectName
      _              <- repo.fetch(layout, https)

      commit         <- repo.getCommitFromTag(layout, version).toOption.ascribe(
                            exoskeleton.InvalidArgValue("version", version.id))

      nameArg        <- ~call(RepoNameArg).getOrElse(suggested)
      sourceRepo     <- ~SourceRepo(nameArg, repo, version, commit, dir)
      lens           <- ~Lenses.layer.repos(schema.id)
      layer          <- ~(lens.modify(layer)(_ + sourceRepo))
      _              <- ~Layer.save(layer, layout)
    } yield log.await()
  }

  def update(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
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
      call       <- cli.call()
      optSchemaId <- ~call(SchemaArg).toOption
      schemaArg   <- ~optSchemaId.getOrElse(layer.main)
      schema      <- layer.schemas.findBy(schemaArg)
      repoArg     <- call(RepoArg)
      repo        <- schema.repos.findBy(repoArg)
      dir         <- ~call(DirArg).toOption
      version     <- ~call(VersionArg).toOption
      remoteArg   <- ~call(UrlArg).toOption
      remote      <- ~remoteArg.map(Repo(_))
      nameArg     <- ~call(RepoNameArg).toOption
      force       <- ~call(ForceArg).isSuccess
      focus       <- ~Lenses.focus(optSchemaId, force)
      layer       <- focus(layer, _.lens(_.repos(on(repo.id)).repo)) = remote
      layer       <- focus(layer, _.lens(_.repos(on(repo.id)).track)) = version
      layer       <- focus(layer, _.lens(_.repos(on(repo.id)).local)) = dir.map(Some(_))
      layer       <- focus(layer, _.lens(_.repos(on(repo.id)).id)) = nameArg
      _           <- ~Layer.save(layer, layout)
    } yield log.await()
  }

  def remove(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      schemaArg <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaArg)
      cli       <- cli.hint(RepoArg, schema.repos)
      call     <- cli.call()
      repoId    <- call(RepoArg)
      repo      <- schema.repos.findBy(repoId)
      lens      <- ~Lenses.layer.repos(schema.id)
      layer     <- ~(lens(layer) -= repo)
      _         <- ~Layer.save(layer, layout)
    } yield log.await()
  }
}
