/*

    Fury, version 0.17.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury

import fury.text._, fury.io._, fury.core._, fury.model._

import Args._

import guillotine._
import optometry._
import mercator._
import euphemism._
import escritoire._
import antiphony._

import scala.concurrent._, ExecutionContext.Implicits.global, duration._
import scala.collection.immutable.SortedSet

import scala.util._

case class RepoCli(cli: Cli)(implicit val log: Log) extends CliApi {

  def list: Try[ExitStatus] = {
    val getTable: Try[Tabulation[Repo]] = getLayout.map(Tables().repos(_)(log))
    
    implicit val columnHints: ColumnArg.Hinter =
      ColumnArg.hint(getTable.map(_.headings.map(_.name.toLowerCase)))

    (cli -< RawArg -< ColumnArg -< RepoArg).action { for {
      table <- getTable
      rows  <- getLayer >> (_.repos)
      repo  <- opt(RepoArg).sequence >> (printTable(table, rows, _, "repo"))
    } yield log.await() }
  }
  
  def doAuth: Try[OauthToken] = for {
    // These futures should be managed in the session
    // This was duplicated from build.scala
    code     <- ~Rnd.token(18)
    uri      <- ~(Https(ManagedConfig().service) / "await").query("code" -> code)
    future   <- ~Future(blocking(Http.get(uri.key, Set()).to[Try]))
    uri      <- ~(Https(ManagedConfig().service) / "auth").query("code" -> code)
    _        <- ~log.info(msg"Please visit $uri to authenticate using GitHub.")
    _        <- ~Future(blocking(Shell(cli.env).tryXdgOpen(uri)))
    response <- Await.result(future, Duration.Inf)
    json     <- Json.parse(new String(response, "UTF-8")).to[Try]
    token    <- json.token.as[String].to[Try]
  } yield OauthToken(token)

  def checkin: Try[ExitStatus] = (cli -< RepoNameArg).action {
    for {
      layout <- getLayout
      repo   <- get(RepoNameArg) >>= (Repo.checkin(layout, _))
      layer  <- getLayer >> (Layer(_.repos).modify(_)(_ + repo))
      layer  <- ~layer.copy(mainRepo = None).checkinSources(repo.id)
      _      <- commit(layer)
    } yield log.await()
  }

  def checkout: Try[ExitStatus] = (cli -< RepoArg -< GrabArg).action { for {
    layout    <- getLayout
    layer     <- getLayer
    call      <- cli.call()
    layer     <- if(get(GrabArg).isSuccess) ~layer else for {
                   repoId <- get(RepoArg)
 
                   _      <- if((layout.baseDir / ".git").exists()) Failure(AlreadyCheckedOut(repoId))
                             else Success(())
 
                   repo   <- layer.repos.findBy(repoId)
                   gitDir <- ~GitDir((layout.baseDir / ".tmp").uniquify)(cli.env)
                   _      <- gitDir.clone(repo.remote, branch = repo.branch, commit = repo.commit)
 
                   _      <- (gitDir.dir.childPaths.flatMap { f =>
                               if((f.parent.parent / f.name).exists()) List(Path(f.name)) else Nil
                             }).filterNot(_ == Path(".fury")) match {
                               case Nil => Success(())
                               case fs  => Failure(WorkingDirectoryConflict(fs))
                             }
 
                   _      <- gitDir.dir.childPaths.traverse { f => f.moveTo(f.parent.parent / f.name) }
                   _      <- gitDir.dir.delete()
                   layer  <- ~layer.checkoutSources(repoId)
                   layer  <- ~(Layer(_.repos).modify(layer)(_ - repo))
                 } yield layer
    gitDir   <- ~GitDir(layout.baseDir)(cli.env)
    layer    <- ~layer.copy(mainRepo = Some(get(RepoArg).getOrElse(RepoId(layout))))
    _        <- commit(layer)
  } yield log.await() }
  
  def unfork: Try[ExitStatus] = (cli -< RepoArg).action {
    for {
      repo      <- getRepo
      _         <- repo.isForked()
      newRepo   <- getLayout >>= (repo.unfork(_))
      layer     <- getLayer >> (Layer(_.repos).modify(_)(_ - repo + newRepo))
      _         <- commit(layer)
    } yield log.await()
  }

  def fork: Try[ExitStatus] = (cli -< PathArg -< ProjectArg -< RepoArg).action { for {
    absPath <- absPath
    layout  <- getLayout
    repo    <- getRepo

    gitDir  <- {
                 for {
                   _ <- Try(absPath.mkdir())
                   _ <- if(absPath.empty) Success(()) else Failure(new Exception("Non-empty dir exists"))
                 } yield GitDir(absPath)(layout.env)
               }.orElse(Failure(exoskeleton.InvalidArgValue("dir", absPath.value)))

    bareRepo <- getGitDir

    _       <- ~gitDir.sparseCheckout(bareRepo.dir, List(), branch = repo.branch, commit = repo.commit,
                  Some(repo.remote))

    newRepo <- ~repo.copy(local = Some(gitDir.dir))
    layer   <- getLayer >> (Layer(_.repos).modify(_)(_ - repo + newRepo))
    _       <- commit(layer)
  } yield log.await() }

  def add: Try[ExitStatus] = (cli -< RemoteArg -< PathArg -< RepoNameArg -< BranchArg -< TagArg).action { for {
    layout   <- getLayout
    refSpec  <- cli.atMostOne(BranchArg, TagArg)
    layer    <- (getLayer, get(RemoteArg), uniqueRepoName) >>= (RepoApi(_).add(_, _, refSpec, layout))
    _        <- commit(layer)
  } yield log.await() }

  def update: Try[ExitStatus] =
    (cli -< RemoteArg -< RepoArg -< RepoNameArg -< BranchArg -< TagArg).action { for {
      repo     <- getRepo
      oldId    <- ~repo.id
      spec     <- cli.atMostOne(BranchArg, TagArg)
      newId    <- opt(RepoNameArg).sequence
      remote   <- opt(RemoteArg).sequence
      layer    <- (getLayer, getLayout) >>= (RepoApi(_).update(oldId, newId, remote, spec, _))
      _        <- commit(layer)
    } yield log.await() }

  def pull: Try[ExitStatus] =
    (cli -< RepoArg -< AllArg).action { for {
      repos <- cli.exactlyOne(RepoArg, AllArg).map(_.fold(Some(_), _ => None))
      layer <- (getLayer, getLayout) >>= (RepoApi(_).pull(repos, _))
      _     <- commit(layer)
    } yield log.await() }

  def remove: Try[ExitStatus] = (cli -< RepoArg).action {
    ((getLayer, getRepo >> (_.id)) >>= (RepoApi(_).remove(_)) >>= commit) >> finish
  }
}

case class RepoApi(layer: Layer) {

  def remove(id: RepoId): Try[Layer] =
    layer.repos.findBy(id).map { r => Layer(_.repos).modify(layer)(_ - r) }

  def add(remote: Remote, name: RepoId, refSpec: Option[Either[Branch, Tag]], layout: Layout)
         (implicit log: Log)
         : Try[Layer] = for {
    gitDir <- remote.fetch(layout)
    branch <- refSpec.map(_.fold(gitDir.checkBranch(_), gitDir.someBranchFromTag(_))).sequence
    branch <- branch.map(~_).getOrElse(gitDir.branch)
    commit <- refSpec.map(_.fold({ _ => gitDir.findCommit(branch) }, gitDir.findCommit(_))).getOrElse(gitDir.commit)
  } yield Layer(_.repos).modify(layer)(_ + Repo(name, remote, branch, commit, None))

  def pull(repo: Option[RepoId], layout: Layout)(implicit log: Log): Try[Layer] = for {
    repos    <- repo.fold(~layer.repos.to[List])(layer.repos.findBy(_).map(List(_)))
    
    newRepos <- repos.traverse { repo => for {
                  gitDir <- repo.remote.fetch(layout)
                  commit <- gitDir.findCommit(repo.branch)
                  msg    <- gitDir.message(commit)

                  _       = if(commit != repo.commit)
                              log.info(msg"Updated ${repo.id} to head of ${repo.branch}, $commit ($msg)")

                } yield repo.copy(commit = commit) }
    
  } yield newRepos.foldLeft(layer) { (layer, next) => Layer(_.repos(next.id))(layer) = next }
  
  def update(id: RepoId,
             name: Option[RepoId],
             remote: Option[Remote],
             refSpec: Option[Either[Branch, Tag]],
             layout: Layout)
            (implicit log: Log)
            : Try[Layer] = for {
    repo   <- layer.repos.findBy(id)
    repo   <- ~name.fold(repo)(Repo(_.id)(repo) = _)
    repo   <- ~remote.fold(repo)(Repo(_.remote)(repo) = _)
    gitDir <- repo.remote.fetch(layout)
    commit <- refSpec.map(_.fold(gitDir.findCommit(_), gitDir.findCommit(_))).sequence
    branch <- refSpec.map(_.fold(gitDir.checkBranch(_), gitDir.someBranchFromTag(_))).sequence
    repo   <- ~branch.fold(repo)(Repo(_.branch)(repo) = _)
    repo   <- ~commit.fold(repo)(Repo(_.commit)(repo) = _)
  } yield Layer(_.repos(id))(layer) = repo
}