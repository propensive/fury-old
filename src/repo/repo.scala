/*

    Fury, version 0.12.3. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury

import fury.strings._, fury.io._, fury.core._, fury.model._

import Args._

import guillotine._
import optometry._
import mercator._
import scala.util._

case class RepoCli(cli: Cli)(implicit log: Log) {

  def list: Try[ExitStatus] = for {
    layout    <- cli.layout
    conf      <- Layer.readFuryConf(layout)
    layer     <- Layer.retrieve(conf)
    cli       <- cli.hint(RawArg)
    table     <- ~Tables().repositories(layout)
    cli       <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
    cli       <- cli.hint(RepoArg, layer.repos.map(_.id))
    call      <- cli.call()
    raw       <- ~call(RawArg).isSuccess
    repoId    <- ~cli.peek(RepoArg)
    col       <- ~cli.peek(ColumnArg)
    rows      <- ~layer.allRepos(layout).to[List].sortBy(_.id)
    table     <- ~Tables().show(table, cli.cols, rows, raw, col, repoId, "repo")
    _         <- ~log.infoWhen(!raw)(conf.focus())
    _         <- ~log.rawln(table)
  } yield log.await()

  def checkout: Try[ExitStatus] = for {
    layout    <- cli.layout
    conf      <- Layer.readFuryConf(layout)
    layer     <- Layer.retrieve(conf)
    cli       <- cli.hint(HttpsArg)
    cli       <- cli.hint(RepoArg, layer.repos.map(_.id))
    call      <- cli.call()
    repoId    <- call(RepoArg)
    repo      <- layer.repos.findBy(repoId)
    local     <- ~layer.localRepo(layout).toOption
    https     <- ~call(HttpsArg).isSuccess
    _         <- repo.checkout(layout, local, https)
    layer     <- ~(Layer(_.mainRepo)(layer) = Some(repo.id))
    _         <- Layer.commit(layer, conf, layout)
    _         <- (layout.pwd / ".fury.conf.bak").delete()
  } yield log.await()

  def unfork: Try[ExitStatus] = for {
    layout <- cli.layout
    conf   <- Layer.readFuryConf(layout)
    layer  <- Layer.retrieve(conf)
    cli       <- cli.hint(RepoArg, layer.repos)
    call      <- cli.call()
    repoId    <- call(RepoArg)
    repo      <- layer.repos.findBy(repoId)
    _         <- repo.isForked()
    newRepo   <- repo.unfork(layout, true)
    layer     <- ~Layer(_.repos).modify(layer)(_ - repo + newRepo)
    _         <- Layer.commit(layer, conf, layout)
  } yield log.await()

  def fork: Try[ExitStatus] = for {
    layout    <- cli.layout
    conf      <- Layer.readFuryConf(layout)
    layer     <- Layer.retrieve(conf)
    cli       <- cli.hint(DirArg)
    cli       <- cli.hint(RepoArg, layer.repos)
    cli       <- cli.hint(HttpsArg)
    call      <- cli.call()
    repoId    <- call(RepoArg)
    repo      <- layer.repos.findBy(repoId)
    dir       <- call(DirArg)
    https     <- ~call(HttpsArg).isSuccess
    bareRepo  <- repo.repo.fetch(layout, https)
    absPath   <- { for {
                    absPath <- ~(layout.pwd.resolve(dir))
                    _       <- Try(absPath.mkdir())
                    _       <- if(absPath.empty) Success(()) else Failure(new Exception("Non-empty dir exists"))
                  } yield absPath }.orElse(Failure(exoskeleton.InvalidArgValue("dir", dir.value)))

    _         <- ~GitDir(bareRepo)(layout.env).sparseCheckout(absPath, List(), refSpec = repo.track, commit =
                      repo.commit, Some(repo.repo.universal(false)))

    newRepo   <- ~repo.copy(local = Some(absPath))
    layer     <- ~Layer(_.repos).modify(layer)(_ - repo + newRepo)
    _         <- Layer.commit(layer, conf, layout)
  } yield log.await()

  def pull: Try[ExitStatus] = for {
    layout <- cli.layout
    conf   <- Layer.readFuryConf(layout)
    layer  <- Layer.retrieve(conf)
    cli       <- cli.hint(HttpsArg)
    cli       <- cli.hint(RepoArg, layer.repos)
    cli       <- cli.hint(AllArg, List[String]())
    call      <- cli.call()
    https     <- ~call(HttpsArg).isSuccess
    all       <- ~call(AllArg).toOption
    
    optRepos  <- call(RepoArg).toOption.map(scala.collection.immutable.SortedSet(_)).orElse(all.map(_ =>
                      layer.repos.map(_.id))).ascribe(exoskeleton.MissingArg("repo"))

    repos     <- optRepos.map(layer.repo(_, layout)).sequence
    succeeded <- ~repos.map(_.pull(layout, https)).forall(_.isSuccess)

    newRepos  <- repos.map { repo => for {
                    commit  <- repo.repo.getCommitFromTag(layout, repo.track)
                    newRepo = repo.copy(commit = commit)
                  } yield (newRepo, repo) }.sequence

    newLayer   = newRepos.foldLeft(layer) { (layer, repoDiff) => repoDiff match {
                    case (newRepo, oldRepo) => Layer(_.repos).modify(layer)(_ - oldRepo + newRepo) }
                  }

    _         <- Layer.commit(layer, conf, layout)

    _         <- ~newRepos.foreach { case (newRepo, _) =>
                    log.info(msg"Repository ${newRepo} checked out to commit ${newRepo.commit}")
                  }

  } yield log.await()

  def add: Try[ExitStatus] = for {
    layout <- cli.layout
    conf   <- Layer.readFuryConf(layout)
    layer  <- Layer.retrieve(conf)
    cli            <- cli.hint(UrlArg)//, GitHub.repos(cli.peek(UrlArg).getOrElse("")))
    cli            <- cli.hint(DirArg)
    cli            <- cli.hint(HttpsArg)
    projectNameOpt <- ~cli.peek(UrlArg).map(Repo(_)).flatMap(_.projectName.toOption)
    cli            <- cli.hint(RepoNameArg, projectNameOpt)
    remoteOpt      <- ~cli.peek(UrlArg)
    repoOpt        <- ~remoteOpt.map(Repo(_))
    
    versions       <- repoOpt.map(GitDir.lsRemote(_)(layout.env)).to[List].sequence.map(_.flatten).recover {
                          case e => Nil }

    cli            <- cli.hint(RefSpecArg, versions)
    call           <- cli.call()
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
    layer          <- ~Layer(_.repos).modify(layer)(_ + sourceRepo)
    _              <- Layer.commit(layer, conf, layout)
  } yield log.await()

  def update: Try[ExitStatus] = for {
    layout <- cli.layout
    conf   <- Layer.readFuryConf(layout)
    layer  <- Layer.retrieve(conf)
    cli         <- cli.hint(DirArg)
    cli         <- cli.hint(UrlArg)
    cli         <- cli.hint(ForceArg)
    cli         <- cli.hint(RepoArg, layer.repos)
    optRepo     <- ~cli.peek(RepoArg).flatMap(layer.repos.findBy(_).toOption)
    refSpecs    <- optRepo.to[List].map(_.repo.path(layout)).map(GitDir(_)(layout.env).showRefs).sequence
    cli         <- cli.hint(RefSpecArg, refSpecs.flatten)
    call        <- cli.call()
    repoArg     <- call(RepoArg)
    repo        <- layer.repos.findBy(repoArg)
    dir         <- ~call(DirArg).toOption
    refSpec     <- ~call(RefSpecArg).toOption
    remoteArg   <- ~call(UrlArg).toOption
    remote      <- ~remoteArg.map(Repo(_))
    nameArg     <- ~call(RepoNameArg).toOption
    force       <- ~call(ForceArg).isSuccess
    layer       <- ~remote.fold(layer)(Layer(_.repos(repo.id).repo)(layer) = _)
    layer       <- ~refSpec.fold(layer)(Layer(_.repos(repo.id).track)(layer) = _)
    layer       <- ~dir.map(Some(_)).fold(layer)(Layer(_.repos(repo.id).local)(layer) = _)
    layer       <- ~nameArg.fold(layer)(Layer(_.repos(repo.id).id)(layer) = _)
    commit      <- refSpec.fold(~repo.commit) { v => repo.repo.getCommitFromTag(layout, v) }
    layer       <- ~(Layer(_.repos(repo.id).commit)(layer) = commit)
    _           <- Layer.commit(layer, conf, layout)
  } yield log.await()

  def remove: Try[ExitStatus] = for {
    layout <- cli.layout
    conf   <- Layer.readFuryConf(layout)
    layer  <- Layer.retrieve(conf)
    cli       <- cli.hint(RepoArg, layer.repos)
    call      <- cli.call()
    repoId    <- call(RepoArg)
    repo      <- layer.repos.findBy(repoId)
    layer     <- ~Layer(_.repos).modify(layer)(_ - repo)
    _         <- Layer.commit(layer, conf, layout)
  } yield log.await()
}
