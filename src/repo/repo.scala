/*

    Fury, version 0.18.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

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

    (cli -< RawArg -< ColumnArg -< RepoArg -< LayerArg).action { for {
      rows  <- getLayer >> (_.repos)
      repo  <- (getTable, opt(RepoArg)) >> (printTable(_, rows, _, "repo"))
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

  def checkin: Try[ExitStatus] = (cli -< RepoNameArg -< LayerArg).action {
    for {
      layout <- getLayout
      repo   <- get(RepoNameArg) >>= (Repo.checkin(layout, _))
      layer  <- getLayer >> (Layer(_.repos).modify(_)(_ + repo))
      layer  <- ~layer.copy(mainRepo = None).checkinSources(repo.id)
      _      <- commit(layer)
    } yield log.await()
  }

  def checkout: Try[ExitStatus] = (cli -< RepoArg -< GrabArg -< LayerArg).action { for {
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
                             }).filterNot(_ == path".fury") match {
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
  
  def unfork: Try[ExitStatus] = (cli -< RepoArg -< LayerArg).action {
    for {
      repo      <- getRepo
      _         <- repo.isForked()
      newRepo   <- getLayout >>= (repo.unfork(_))
      layer     <- getLayer >> (Layer(_.repos).modify(_)(_ - repo + newRepo))
      _         <- commit(layer)
    } yield log.await()
  }

  def fork: Try[ExitStatus] = (cli -< LayerArg -< PathArg -< ProjectArg -< RepoArg).action { for {
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

  def add: Try[ExitStatus] = (cli -< RemoteArg -< PathArg -< RepoNameArg -< BranchArg -< TagArg -< LayerArg).action { for {
    layout  <- getLayout
    _       <- cli.atMostOne(BranchArg, TagArg)
    _       <- cli.atLeastOne(RemoteArg, PathArg)
    _       <- cli.atMostOne(PathArg, BranchArg)
    _       <- cli.atMostOne(PathArg, TagArg)
    refSpec <- (opt(BranchArg), opt(TagArg), opt(CommitArg)) >> (_.orElse(_).orElse(_))
    id      <- findUniqueRepoName
    path    <- relPathOpt
    pointer <- getPointer
    remote  <- opt(RemoteArg)
    _       <- getHierarchy >>= (RepoApi(_).add(pointer, id, remote, refSpec, path, layout)) >>= commit
  } yield log.await() }

  def update: Try[ExitStatus] =
    (cli -< CommitArg -< LayerArg -< RemoteArg -< RepoArg -< RepoNameArg -< BranchArg -< TagArg).action { for {
      repo      <- getRepo
      _         <- cli.atMostOne(BranchArg, TagArg)
      _         <- cli.atMostOne(BranchArg, CommitArg)
      _         <- cli.atMostOne(TagArg, CommitArg)
      refSpec   <- (opt(BranchArg), opt(TagArg), opt(CommitArg)) >> (_.orElse(_).orElse(_))
      newId     <- opt(RepoNameArg)
      remote    <- opt(RemoteArg)
      layout    <- getLayout
      hierarchy <- getHierarchy
      pointer   <- getPointer
      _         <- RepoApi(hierarchy).update(pointer, repo.id, newId, remote, refSpec, layout) >>= commit
    } yield log.await() }

  def pull: Try[ExitStatus] =
    (cli -< LayerArg -< RepoArg -< AllArg).action {
      val repos = cli.exactlyOne(RepoArg, AllArg).map(_.fold(Some(_), _ => None))
      ((getHierarchy, getPointer, repos, getLayout) >>= (RepoApi(_).pull(_, _, _)) >>= commit) >> finish
    }

  def remove: Try[ExitStatus] = (cli -< RepoArg).action {
    ((getHierarchy, getPointer, getRepo >> (_.id)) >>= (RepoApi(_).remove(_, _)) >>= commit) >> finish
  }
}

case class RepoApi(hierarchy: Hierarchy) {

  def remove(pointer: Pointer, id: RepoId)(implicit log: Log): Try[Hierarchy] = hierarchy.on(pointer) { layer =>
    layer.repos.findBy(id).map { r => Layer(_.repos).modify(layer)(_ - r) }
  }

  def add(pointer: Pointer,
          id: RepoId,
          remote: Option[Remote],
          refSpec: Option[RefSpec],
          path: Option[Path],
          layout: Layout)
         (implicit log: Log)
         : Try[Hierarchy] = for {
    layer      <- hierarchy(pointer)
    pathRemote <- path.map(GitDir(_)(layout.env).remote).sequence
    remote     <- remote.orElse(pathRemote).ascribe(NoRemoteInferred())
    gitDir     <- remote.fetch(layout)
    refSpec    <- refSpec.fold[Try[RefSpec]](gitDir.commit)(Try(_))
    commit     <- gitDir.resolve(refSpec)
    branch     <- gitDir.chooseBranch(refSpec)
    hierarchy  <- hierarchy(pointer) = Layer(_.repos).modify(layer)(_ + Repo(id, remote, branch, commit, path))
  } yield hierarchy

  def pull(pointer: Pointer, repo: Option[RepoId], layout: Layout)(implicit log: Log): Try[Layer] = for {
    layer    <- hierarchy(pointer)
    repos    <- repo.fold(~layer.repos.to[List])(layer.repos.findBy(_).map(List(_)))
    
    newRepos <- repos.traverse { repo => for {
                  gitDir <- repo.remote.fetch(layout)
                  commit <- gitDir.findCommit(repo.branch)
                  msg    <- gitDir.message(commit)

                  _       = if(commit != repo.commit)
                              log.info(msg"Updated ${repo.id} to head of ${repo.branch}, $commit ($msg)")

                } yield repo.copy(commit = commit) }
    
  } yield newRepos.foldLeft(layer) { (layer, next) => Layer(_.repos(next.id))(layer) = next }
  
  def update(pointer: Pointer,
             id: RepoId,
             name: Option[RepoId],
             remote: Option[Remote],
             refSpec: Option[RefSpec],
             layout: Layout)
            (implicit log: Log)
            : Try[Hierarchy] = for {
    layer     <- hierarchy(pointer)
    repo      <- layer.repos.findBy(id)
    repo      <- ~name.fold(repo)(Repo(_.id)(repo) = _)
    repo      <- ~remote.fold(repo)(Repo(_.remote)(repo) = _)
    gitDir    <- repo.remote.fetch(layout)
    commit    <- refSpec.fold(~repo.commit)(gitDir.resolve)
    branch    <- refSpec.fold(~repo.branch)(gitDir.chooseBranch)
    repo      <- ~repo.copy(branch = branch, commit = commit)
    hierarchy <- hierarchy(pointer) = Layer(_.repos(id))(layer) = repo
  } yield hierarchy
}

case class WorkspaceCli(cli: Cli)(implicit val log: Log) extends CliApi {

  def list: Try[ExitStatus] = {
    val table: Tabulation[Workspace] = Tables().workspaces
    
    implicit val columnHints: ColumnArg.Hinter =
      ColumnArg.hint(table.headings.map(_.name.toLowerCase))

    (cli -< RawArg -< ColumnArg -< WorkspaceArg -< LayerArg).action { for {
      rows      <- getLayer >> (_.workspaces)
      workspace <- opt(WorkspaceArg) >> (printTable(table, rows, _, "workspace"))
    } yield log.await() }
  }
  
  def add: Try[ExitStatus] = (cli -< PathArg -< WorkspaceNameArg -< LayerArg).action { for {
    layout  <- getLayout
    path    <- relPathOpt
    pointer <- getPointer
    name    <- get(WorkspaceNameArg)
    _       <- getHierarchy >>= (WorkspaceApi(_).add(pointer, name, path, layout)) >>= commit
  } yield log.await() }

  def update: Try[ExitStatus] =
    (cli -< LayerArg -< WorkspaceArg -< WorkspaceNameArg).action { for {
      workspace <- getWorkspace
      newId     <- opt(WorkspaceNameArg)
      layout    <- getLayout
      hierarchy <- getHierarchy
      pointer   <- getPointer
      _         <- WorkspaceApi(hierarchy).update(pointer, workspace.id, newId, layout) >>= commit
    } yield log.await() }
  
  def remove: Try[ExitStatus] = (cli -< WorkspaceArg).action {
    ((getHierarchy, getPointer, getWorkspace >> (_.id)) >>= (WorkspaceApi(_).remove(_, _)) >>= commit) >> finish
  }
}

case class WorkspaceApi(hierarchy: Hierarchy) {

  def remove(pointer: Pointer, id: RepoId)(implicit log: Log): Try[Hierarchy] = hierarchy.on(pointer) { layer =>
    layer.workspaces.find(_.id == id).ascribe(ItemNotFound(id)).map { workspace =>
      Layer(_.workspaces).modify(layer)(_ - workspace)
    }
  }

  def add(pointer: Pointer, id: RepoId, path: Option[Path], layout: Layout)(implicit log: Log): Try[Hierarchy] =
    hierarchy.on(pointer) { layer => ~Layer(_.workspaces).modify(layer)(_ + Workspace(id, path)) }

  def update(pointer: Pointer, id: RepoId, name: Option[RepoId], layout: Layout)
            (implicit log: Log)
            : Try[Hierarchy] = hierarchy.on(pointer) { layer => for {
    oldWorkspace <- layer.workspaces.find(_.id == id).ascribe(ItemNotFound(id))
    newWorkspace <- ~name.fold(oldWorkspace) { name => oldWorkspace.copy(id = name) }
  } yield layer.copy(workspaces = layer.workspaces - oldWorkspace + newWorkspace) }
}