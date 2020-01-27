/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.8.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.                                         ║
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

case class RepoCli(cli: Cli)(implicit log: Log) {

  def list: Try[ExitStatus] = for {
    layout <- cli.layout
    conf   <- Layer.readFuryConf(layout)
    layer  <- Layer.read(layout, conf)
    cli       <- cli.hint(RawArg)
    call     <- cli.call()
    raw       <- ~call(RawArg).isSuccess
    schemaArg <- ~SchemaId.default
    schema    <- layer.schemas.findBy(schemaArg)
    rows      <- ~schema.allRepos(layout).to[List].sortBy(_.id)
    table     <- ~Tables().show(Tables().repositories(layout), cli.cols, rows, raw)(_.id)
    _         <- ~log.infoWhen(!raw)(conf.focus())
    _         <- ~log.rawln(table.mkString("\n"))
  } yield log.await()

  def unfork: Try[ExitStatus] = for {
    layout <- cli.layout
    conf   <- Layer.readFuryConf(layout)
    layer  <- Layer.read(layout, conf)
    schemaArg <- ~SchemaId.default
    schema    <- layer.schemas.findBy(schemaArg)
    cli       <- cli.hint(RepoArg, schema.repos)
    call      <- cli.call()
    repoId    <- call(RepoArg)
    repo      <- schema.repos.findBy(repoId)
    _         <- repo.isForked()
    newRepo   <- repo.unfork(layout, true)
    lens      <- ~Lenses.layer.repos(schema.id)
    layer     <- ~(lens.modify(layer)(_ - repo + newRepo))
    _         <- ~Layer.save(layer, layout)
  } yield log.await()

  def fork: Try[ExitStatus] = for {
    layout <- cli.layout
    conf   <- Layer.readFuryConf(layout)
    layer  <- Layer.read(layout, conf)
    schemaArg <- ~SchemaId.default
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
                      repo.commit.id, Some(repo.repo.universal(false)))

    newRepo   <- ~repo.copy(local = Some(absPath))
    lens      <- ~Lenses.layer.repos(schema.id)
    layer     <- ~(lens.modify(layer)(_ - repo + newRepo))
    _         <- ~Layer.save(layer, layout)
  } yield log.await()

  def pull: Try[ExitStatus] = for {
    layout <- cli.layout
    conf   <- Layer.readFuryConf(layout)
    layer  <- Layer.read(layout, conf)
    cli       <- cli.hint(HttpsArg)
    schemaArg <- ~SchemaId.default
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

  def add: Try[ExitStatus] = for {
    layout <- cli.layout
    conf   <- Layer.readFuryConf(layout)
    layer  <- Layer.read(layout, conf)
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

    cli            <- cli.hint(RefSpecArg, versions)
    call          <- cli.call()
    optSchemaArg   <- ~Some(SchemaId.default)
    schemaArg      <- ~optSchemaArg.getOrElse(layer.main)
    schema         <- layer.schemas.findBy(schemaArg)
    dir            <- ~call(DirArg).toOption
    https          <- ~call(HttpsArg).isSuccess
    refSpec        <- ~call(RefSpecArg).toOption.getOrElse(RefSpec.master)
    urlArg         <- cli.peek(UrlArg).ascribe(exoskeleton.MissingArg("url"))
    repo           <- repoOpt.ascribe(exoskeleton.InvalidArgValue("url", urlArg))
    suggested      <- repo.projectName
    _              <- repo.fetch(layout, https)

    commit         <- repo.getCommitFromTag(layout, refSpec).toOption.ascribe(
                          exoskeleton.InvalidArgValue("refspec", refSpec.id))

    nameArg        <- ~call(RepoNameArg).getOrElse(suggested)
    sourceRepo     <- ~SourceRepo(nameArg, repo, refSpec, commit, dir)
    lens           <- ~Lenses.layer.repos(schema.id)
    layer          <- ~(lens.modify(layer)(_ + sourceRepo))
    _              <- ~Layer.save(layer, layout)
  } yield log.await()

  def update: Try[ExitStatus] = for {
    layout <- cli.layout
    conf   <- Layer.readFuryConf(layout)
    layer  <- Layer.read(layout, conf)
    cli         <- cli.hint(DirArg)
    cli         <- cli.hint(UrlArg)
    cli         <- cli.hint(ForceArg)
    schemaArg   <- ~SchemaId.default
    schema      <- layer.schemas.findBy(schemaArg)
    cli         <- cli.hint(RepoArg, schema.repos)
    optRepo     <- ~cli.peek(RepoArg).flatMap(schema.repos.findBy(_).toOption)
    refSpecs    <- optRepo.to[List].map(_.repo.path(layout)).map(Shell(layout.env).git.showRefs(_)).sequence
    cli         <- cli.hint(RefSpecArg, refSpecs.flatten)
    call        <- cli.call()
    optSchemaId <- ~Some(SchemaId.default)
    schemaArg   <- ~optSchemaId.getOrElse(layer.main)
    schema      <- layer.schemas.findBy(schemaArg)
    repoArg     <- call(RepoArg)
    repo        <- schema.repos.findBy(repoArg)
    dir         <- ~call(DirArg).toOption
    refSpec     <- ~call(RefSpecArg).toOption
    remoteArg   <- ~call(UrlArg).toOption
    remote      <- ~remoteArg.map(Repo(_))
    nameArg     <- ~call(RepoNameArg).toOption
    force       <- ~call(ForceArg).isSuccess
    focus       <- ~Lenses.focus()
    layer       <- ~(focus(layer, _.lens(_.repos(on(repo.id)).repo)) = remote)
    layer       <- ~(focus(layer, _.lens(_.repos(on(repo.id)).track)) = refSpec)
    layer       <- ~(focus(layer, _.lens(_.repos(on(repo.id)).local)) = dir.map(Some(_)))
    layer       <- ~(focus(layer, _.lens(_.repos(on(repo.id)).id)) = nameArg)
    commit      <- refSpec.fold(~repo.commit) { v => repo.repo.getCommitFromTag(layout, v) }
    layer       <- ~(focus(layer, _.lens(_.repos(on(repo.id)).commit)) = Some(commit))
    _           <- ~Layer.save(layer, layout)
  } yield log.await()

  def remove: Try[ExitStatus] = for {
    layout <- cli.layout
    conf   <- Layer.readFuryConf(layout)
    layer  <- Layer.read(layout, conf)
    schemaArg <- ~SchemaId.default
    schema    <- layer.schemas.findBy(schemaArg)
    cli       <- cli.hint(RepoArg, schema.repos)
    call      <- cli.call()
    repoId    <- call(RepoArg)
    repo      <- schema.repos.findBy(repoId)
    lens      <- ~Lenses.layer.repos(schema.id)
    layer     <- ~(lens(layer) -= repo)
    _         <- ~Layer.save(layer, layout)
  } yield log.await()
}
