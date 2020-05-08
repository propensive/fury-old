/*

    Fury, version 0.15.1. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

import fury.model._, fury.io._, fury.strings._, fury.ogdl._
import mercator._

import scala.util._


object Repo {
  implicit val msgShow: MsgShow[Repo] = r => UserMsg(_.repo(r.id.key))
  implicit val stringShow: StringShow[Repo] = _.id.key
  implicit def diff: Diff[Repo] = Diff.gen[Repo]

  def checkin(layout: Layout, repoId: RepoId)(implicit log: Log): Try[Repo] = for {
    gitDir <- ~GitDir(layout)
    dirty  <- gitDir.diffShortStat()
    _      <- if(dirty.isEmpty) Try(()) else Failure(RepoDirty(repoId, dirty.get.value))
    commit <- gitDir.commit
    branch <- gitDir.branch
    remote <- gitDir.remote
    pushed <- gitDir.remoteHasCommit(commit, branch)
    _      <- if(pushed) Try(()) else Failure(RemoteNotSynched(repoId, remote.ref))
    dest   <- Try((Xdg.runtimeDir / str"${repoId.key}.bak").uniquify())
    files  <- gitDir.trackedFiles
    _      <- ~log.info(msg"Moving working directory contents to $dest")
    _      <- files.traverse { f => f.in(layout.baseDir).moveTo(f.in(dest)) }
    _      <- (layout.baseDir / ".git").moveTo(dest / ".git")
    _      <- ~log.info(msg"Moved ${files.length + 1} files to ${dest}")
  } yield Repo(repoId, remote, branch, commit, None)

  def local(layout: Layout, layer: Layer)(implicit log: Log): Try[Option[Repo]] = {
    val gitDir = GitDir(layout)
    if(gitDir.commit.isFailure) Success(None)
    else for {
      repoId <- ~layer.uniqueRepoId(layout.baseDir)
      remote <- gitDir.remote
      branch <- gitDir.branch
      commit <- gitDir.commit
    } yield Some(Repo(repoId, remote, branch, commit, None))
  }
}

case class Repo(id: RepoId, remote: Remote, branch: Branch, commit: Commit, local: Option[Path]) {
  def listFiles(layout: Layout)(implicit log: Log): Try[List[Path]] = for {
    gitDir <- localDir(layout).map(Success(_)).getOrElse(remote.get(layout))
    files  <- localDir(layout).fold(gitDir.lsTree(commit))(Success(gitDir.dir.children.map(Path(_))).waive)
  } yield files

  def branch(layout: Layout)(implicit log: Log): Branch =
    localDir(layout).flatMap(_.branch.toOption).getOrElse(branch)

  def fullCheckout(layout: Layout)(implicit log: Log): Checkout =
    Checkout(id, remote, localDir(layout), commit, branch, List())

  def localDir(layout: Layout)(implicit log: Log): Option[GitDir] = local.map(GitDir(_)(layout.env))

  def changes(layout: Layout)(implicit log: Log): Try[Option[DiffStat]] = for {
    repoDir <- localDir(layout).map(Success(_)).getOrElse(remote.fetch(layout))
    changes <- repoDir.diffShortStat()
  } yield changes

  def pull(layout: Layout)(implicit log: Log): Try[Commit] =
    remote.pull(commit, layout)

  def isForked(): Try[Unit] = local.ascribe(RepoNotForked(id)).map { _ => () }
  def isNotForked(): Try[Unit] = local.fold(Try(())) { dir => Failure(RepoAlreadyForked(id, dir)) }

  def current(layout: Layout)(implicit log: Log): Try[Commit] = for {
    dir    <- localDir(layout).map(Success(_)).getOrElse(remote.fetch(layout))
    commit <- dir.commit
  } yield Commit(commit.id)

  def sourceCandidates(layout: Layout)
                      (pred: String => Boolean)
                      (implicit log: Log)
                      : Try[Set[Source]] =
    listFiles(layout).map(_.filter { f => pred(f.filename) }.map { p =>
        ExternalSource(id, p.parent, Glob.All): Source }.to[Set])
  
  def unfork(layout: Layout)(implicit log: Log): Try[Repo] = for {
    _          <- if(local.isDefined) Success(()) else Failure(RepoNotForked(id))
    dir        <- ~local.get
    forkCommit <- GitDir(dir)(layout.env).commit
    relDir     <- ~(dir.relativizeTo(layout.pwd))
    _          <- Try(if(forkCommit != commit) log.info(msg"Updating $id commit to $forkCommit of $relDir"))
    changes    <- changes(layout)
    
    _          <- Try(changes.foreach { cs => log.warn(
                      msg"Uncommitted changes ($cs) in $relDir will not apply to the unforked repo") })

  } yield copy(local = None, commit = forkCommit)

  def conflict(layout: Layout, local: Option[Repo])
              (implicit log: Log)
              : Try[List[Path]] = for {
    current   <- ~layout.baseDir.children.to[Set].map(Path(_))
    removed   <- local.flatMap(_.local).fold(Try(List[Path]()))(GitDir(_)(layout.env).trackedFiles)
    bareRepo  <- remote.fetch(layout)
    files     <- bareRepo.lsRoot(commit)
    remaining <- Try((current -- removed) - Path(".fury/config"))
  } yield remaining.intersect(files.to[Set]).to[List]

  def doCleanCheckout(layout: Layout)(implicit log: Log): Try[Unit] = for {
    _          <- ~log.info(msg"Checking out ${remote} to ${layout.baseDir.relativizeTo(layout.pwd)}")
    bareRepo   <- remote.fetch(layout)
    _          <- ~layout.confFile.moveTo(layout.confFileBackup)
    gitDir     <- ~GitDir(layout)
    repo       <- gitDir.sparseCheckout(bareRepo.dir, List(), branch, commit, Some(remote))
    _          <- ~layout.confFileBackup.moveTo(layout.confFile)
  } yield ()
}
