/*

    Fury, version 0.13.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

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
import euphemism._
import antiphony._

import scala.concurrent._, ExecutionContext.Implicits.global, duration._
import scala.collection.immutable.SortedSet

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

  def commit: Try[ExitStatus] = for {
    layout    <- cli.layout
    conf      <- Layer.readFuryConf(layout)
    layer     <- Layer.retrieve(conf)
    cli       <- cli.hint(HttpsArg)
    cli       <- cli.hint(ServiceArg)
    call      <- cli.call()
    service   <- ~call(ServiceArg).toOption.getOrElse(ManagedConfig().service)
    token     <- ManagedConfig().token.ascribe(NotAuthenticated()).orElse(doAuth)
    _         <- Layer.share(service, layer, token)
    local     <- layer.local(layout)
    _         <- layout.gitConf.copyTo(layout.baseConf)
    commit    <- GitDir(layout.baseDir)(layout.env).commit
    branch    <- GitDir(layout.baseDir)(layout.env).branch
    layer     <- ~(Layer(_.repos(local.id).branch)(layer) = branch)
    layer     <- ~(Layer(_.repos(local.id).commit)(layer) = commit)
    _         <- Layer.commit(layer, conf, layout)
  } yield log.await()

  def doAuth: Try[OauthToken] = for {
    // These futures should be managed in the session
    // This was duplicated from build.scala
    code      <- ~Rnd.token(18)
    uri       <- ~(Https(ManagedConfig().service) / "await").query("code" -> code)
    future    <- ~Future(blocking(Http.get(uri.key, Set()).to[Try]))
    uri       <- ~(Https(ManagedConfig().service) / "auth").query("code" -> code)
    _         <- ~log.info(msg"Please visit $uri to authenticate using GitHub.")
    _         <- ~Future(blocking(Shell(cli.env).tryXdgOpen(uri)))
    response  <- Await.result(future, Duration.Inf)
    json      <- Json.parse(new String(response, "UTF-8")).to[Try]
    token     <- json.token.as[String].to[Try]
  } yield OauthToken(token)

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
    _         <- ~(layout.pwd / ".fury.conf.bak").delete()
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

    _         <- ~bareRepo.sparseCheckout(absPath, List(), branch = repo.branch, commit = repo.commit,
                     Some(repo.repo.universal(false)))

    newRepo   <- ~repo.copy(local = Some(absPath))
    layer     <- ~Layer(_.repos).modify(layer)(_ - repo + newRepo)
    _         <- Layer.commit(layer, conf, layout)
  } yield log.await()

  def pull: Try[ExitStatus] = for {
    layout    <- cli.layout
    conf      <- Layer.readFuryConf(layout)
    layer     <- Layer.retrieve(conf)
    cli       <- cli.hint(HttpsArg)
    cli       <- cli.hint(RepoArg, layer.repos)
    cli       <- cli.hint(AllArg, List[String]())
    optRepo   <- ~cli.peek(RepoArg).flatMap(layer.repos.findBy(_).toOption)
    optDir    <- ~optRepo.flatMap(_.repo.fetch(layout, true).toOption)
    cli       <- cli.hint(TagArg, optDir.flatMap(_.tags.toOption).to[List].flatten)
    cli       <- cli.hint(CommitArg, optDir.flatMap(_.allCommits.toOption).to[List].flatten)
    call      <- cli.call()
    https     <- ~call(HttpsArg).isSuccess
    all       <- ~call(AllArg).toOption
    tag       <- ~call(TagArg).toOption
    tagCommit <- call.atMostOne(TagArg, CommitArg)
    _         <- call.atMostOne(TagArg, AllArg)
    _         <- call.atMostOne(CommitArg, AllArg)
    gitDir    <- optDir.ascribe(RepoNotFound())
    commit    <- ~tagCommit.map(_.fold(gitDir.commitFromTag(_), Success(_)).toOption)
    
    optRepos  <- call(RepoArg).toOption.map(SortedSet(_)).orElse(all.map(_ =>
                      layer.repos.map(_.id))).ascribe(MissingParam(RepoArg))

    repos     <- optRepos.map(layer.repo(_, layout)).sequence
    succeeded <- ~repos.map(_.pull(layout, https)).forall(_.isSuccess)

    newRepos  <- repos.traverse { repo => for {
                   gitDir <- ~repo.repo.gitDir(layout)
                   
                   commit <- commit.flatten.ascribe(CannotUpdateRepo(repo.id)).orElse(
                                 gitDir.commitFromBranch(repo.branch))

                 } yield (repo.copy(commit = commit), repo) }

    newLayer   = newRepos.foldLeft(layer) { (layer, repoDiff) => repoDiff match {
                   case (newRepo, oldRepo) => Layer(_.repos).modify(layer)(_ - oldRepo + newRepo) }
                 }

    _         <- Layer.commit(layer, conf, layout)

    _         <- ~newRepos.foreach { case (newRepo, _) =>
                    log.info(msg"Repository ${newRepo} checked out to commit ${newRepo.commit}")
                  }

  } yield log.await()

  def add: Try[ExitStatus] = for {
    layout         <- cli.layout
    conf           <- Layer.readFuryConf(layout)
    layer          <- Layer.retrieve(conf)
    cli            <- cli.hint(RepoUrlArg, GitHub.repos(cli.peek(RepoUrlStringArg)).getOrElse(Nil))
    cli            <- cli.hint(DirArg)
    cli            <- cli.hint(HttpsArg)
    projectNameOpt <- ~cli.peek(RepoUrlArg).flatMap(_.projectName.toOption)
    cli            <- cli.hint(RepoNameArg, projectNameOpt)
    optRepo        <- ~cli.peek(RepoUrlArg)
    optGitDir      <- ~optRepo.map(RemoteGitDir(cli.env, _))
    tags           <- ~optGitDir.map(_.tags().getOrElse(Nil)).getOrElse(Nil)
    branches       <- ~optGitDir.map(_.branches().getOrElse(Nil)).getOrElse(Nil)
    cli            <- cli.hint(BranchArg, branches)
    cli            <- cli.hint(TagArg, tags)
    call           <- cli.call()
    dir            <- ~call(DirArg).toOption
    https          <- ~call(HttpsArg).isSuccess
    branchTag      <- call.atMostOne(BranchArg, TagArg).map(_.getOrElse(Left(Branch.master)))
    repo           <- call(RepoUrlArg)
    gitDir         <- ~RemoteGitDir(cli.env, repo)
    suggested      <- repo.projectName
    gitDir         <- repo.fetch(layout, https)
    commit         <- branchTag.fold(gitDir.commitFromBranch(_), gitDir.commitFromTag(_))
    branch         <- branchTag.fold(Success(_), gitDir.someBranchFromTag(_))
    nameArg        <- ~call(RepoNameArg).getOrElse(suggested)
    sourceRepo     <- ~SourceRepo(nameArg, repo, branch, commit, dir)
    layer          <- ~Layer(_.repos).modify(layer)(_ + sourceRepo)
    _              <- Layer.commit(layer, conf, layout)
  } yield log.await()

  def update: Try[ExitStatus] = for {
    layout    <- cli.layout
    conf      <- Layer.readFuryConf(layout)
    layer     <- Layer.retrieve(conf)
    cli       <- cli.hint(DirArg)
    cli       <- cli.hint(HttpsArg)
    cli       <- cli.hint(RepoUrlArg, GitHub.repos(cli.peek(RepoUrlStringArg)).getOrElse(Nil))
    cli       <- cli.hint(RepoArg, layer.repos)
    optRepo   <- ~cli.peek(RepoArg).flatMap(layer.repos.findBy(_).toOption)
    branches  <- optRepo.to[List].map(_.repo.path(layout)).map(GitDir(_)(layout.env).branches).sequence
    tags      <- optRepo.to[List].map(_.repo.path(layout)).map(GitDir(_)(layout.env).tags).sequence
    cli       <- cli.hint(BranchArg, branches.flatten)
    cli       <- cli.hint(TagArg, tags.flatten)
    call      <- cli.call()
    repoArg   <- call(RepoArg)
    repo      <- layer.repos.findBy(repoArg)
    gitDir    <- ~repo.repo.gitDir(layout)
    dir       <- ~call(DirArg).toOption
    branch    <- ~call(BranchArg).toOption
    nameArg   <- ~call(RepoNameArg).toOption
    branchTag <- call.atMostOne(BranchArg, TagArg).map(_.getOrElse(Left(Branch.master)))
    commit    <- branchTag.fold(gitDir.commitFromBranch(_), gitDir.commitFromTag(_))
    urlArg    <- ~call(RepoUrlArg).toOption
    https     <- ~call(HttpsArg).isSuccess
    layer     <- ~urlArg.fold(layer)(Layer(_.repos(repo.id).repo)(layer) = _)
    layer     <- ~branch.fold(layer)(Layer(_.repos(repo.id).branch)(layer) = _)
    layer     <- ~dir.map(Some(_)).fold(layer)(Layer(_.repos(repo.id).local)(layer) = _)
    layer     <- ~nameArg.fold(layer)(Layer(_.repos(repo.id).id)(layer) = _)
    layer     <- ~(Layer(_.repos(repo.id).commit)(layer) = commit)
    _         <- repo.repo.fetch(layout, https)
    _         <- Layer.commit(layer, conf, layout)
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
