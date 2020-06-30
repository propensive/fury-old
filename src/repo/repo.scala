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
      _      = printTable(table, rows, opt(RepoArg), "repo")
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

  def pull: Try[ExitStatus] = for {
    layout    <- cli.layout
    conf      <- Layer.readFuryConf(layout)
    layer     <- Layer.retrieve(conf)
    cli       <- cli.hint(RepoArg, layer.repos)
    cli       <- cli.hint(AllArg, List[String]())
    
    tryDir     = for {
                   repoId     <- cli.preview(RepoArg)()
                   repo       <- layer.repos.findBy(repoId)
                   dir        <- repo.remote.fetch(layout)
                 } yield dir

    cli       <- cli.hint(TagArg, tryDir.flatMap(_.tags).getOrElse(Nil))
    cli       <- cli.hint(CommitArg, tryDir.flatMap(_.allCommits).getOrElse(Nil))
    call      <- cli.call()
    all       <- ~call(AllArg).toOption
    tag       <- ~call(TagArg).toOption
    tagCommit <- call.atMostOne(TagArg, CommitArg)
    _         <- call.atMostOne(TagArg, AllArg)
    _         <- call.atMostOne(CommitArg, AllArg)
    gitDir    <- tryDir
    gitCommit <- ~tagCommit.map(_.fold(gitDir.findCommit(_), Success(_)).toOption)
    
    optRepos  <- call(RepoArg).toOption.map(SortedSet(_)).orElse(all.map(_ =>
                      layer.repos.map(_.id))).ascribe(MissingParam(RepoArg))

    repos     <- optRepos.traverse(layer.repos.findBy(_))
    succeeded <- ~repos.map(_.pull(layout)(log)).forall(_.isSuccess)

    newRepos  <- repos.traverse { repo => for {
                   gitDir <- ~repo.remote.gitDir(layout)
                   
                   commit <- gitCommit.flatten.ascribe(CannotUpdateRepo(repo.id)).orElse(
                                 gitDir.findCommit(repo.branch))

                 } yield (repo.copy(commit = commit), repo) }

    newLayer   = newRepos.foldLeft(layer) { (layer, repoDiff) => repoDiff match {
                   case (newRepo, oldRepo) => Layer(_.repos).modify(layer)(_ - oldRepo + newRepo) }
                 }

    _         <- commit(newLayer)

    _         <- ~newRepos.foreach { case (newRepo, _) =>
                    log.info(msg"Repository ${newRepo} checked out to commit ${newRepo.commit}")
                  }

  } yield log.await()

  def add: Try[ExitStatus] = (cli -< RemoteArg -< PathArg -< RepoNameArg -< BranchArg -< TagArg).action { for {
    refSpec   <- getRefSpec
    gitCommit <- cliCommit
    remoteDir <- getRemoteDir
    branch    <- refSpec.fold(Success(_), remoteDir.someBranchFromTag(_))
    repo      <- (uniqueRepoName, get(RemoteArg), get(PathArg) >> (Some(_))) >> (Repo(_, _, branch, gitCommit, _))
    _         <- getLayer >> (Layer(_.repos).modify(_)(_ + repo)) >> commit
  } yield log.await() }

  def update: Try[ExitStatus] =
    (cli -< PathArg -< RemoteArg -< RepoArg -< RepoNameArg -< BranchArg -< TagArg).action { for {
      layer     <- getLayer
      repo      <- getRepo
      gitDir    <- getGitDir
      refSpec   <- getRefSpec
      gitCommit <- refSpec.fold(gitDir.findCommit(_), gitDir.findCommit(_))
      layer     <- ~cliCommit.toOption.fold(layer)(Layer(_.repos(repo.id).commit)(layer) = _)
      layer     <- ~opt(RepoNameArg).fold(layer)(Layer(_.repos(repo.id).id)(layer) = _)
      layer     <- ~opt(RemoteArg).fold(layer)(Layer(_.repos(repo.id).remote)(layer) = _)
      layer     <- ~opt(BranchArg).fold(layer)(Layer(_.repos(repo.id).branch)(layer) = _)
      layer     <- ~opt(PathArg).fold(layer) { path => Layer(_.repos(repo.id).local)(layer) = Some(path) }
      _         <- commit(layer)
    } yield log.await() }

  def remove: Try[ExitStatus] = (cli -< RepoArg).action { for {
    repo     <- getRepo
    project  <- getProject
    newLayer <- getLayer >> (Layer(_.repos).modify(_)(_ - repo))
    _        <- commit(newLayer)
  } yield log.await() }
}
