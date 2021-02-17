/*

    Fury, version 0.35.0. Copyright 2018-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

import fury.model._, fury.io._, fury.text._, fury.ogdl._
import mercator._
import optometry._
import jovian._

import scala.util._

object Workspace extends Lens.Partial[Workspace] {
  implicit val msgShow: MsgShow[Workspace] = w => UserMsg(_.repo(w.id.key))
  implicit val stringShow: StringShow[Workspace] = _.id.key
  implicit def diff: Diff[Workspace] = Diff.gen[Workspace]
}

case class Workspace(id: WorkspaceId, local: Option[Path])

object Repo extends Lens.Partial[Repo] {
  implicit val msgShow: MsgShow[Repo] = r => UserMsg(_.repo(r.id.key))
  implicit val stringShow: StringShow[Repo] = _.id.key
  implicit def diff: Diff[Repo] = Diff.gen[Repo]

  def checkin(layout: Layout, repoId: RepoId)(implicit log: Log): Try[Repo] = for {
    gitDir    <- ~GitDir(layout)
    commit    <- gitDir.commit
    dirty     <- gitDir.diffShortStat(Some(commit))
    untracked <- gitDir.untrackedFiles
    _         <- if(dirty.isEmpty) Try(()) else Failure(RepoDirty(repoId, dirty.get.value))
    _         <- if(untracked.isEmpty) Try(()) else Failure(UntrackedFiles(untracked))
    branch    <- gitDir.branch
    remote    <- gitDir.remote
    pushed    <- gitDir.remoteHasCommit(commit, branch)
    _         <- if(pushed) Try(()) else Failure(RemoteNotSynched(repoId, remote.ref))
    dest      <- Try((Xdg.runtimeDir / str"${repoId.key}.bak").uniquify())
    files     <- gitDir.trackedFiles
    _         <- ~log.info(msg"Moving working directory contents to $dest")
    _         <- files.filter(_ != path".fury").traverse { f => f.in(layout.baseDir).moveTo(f.in(dest)) }
    _         <- (layout.baseDir / ".git").moveTo(dest / ".git")
    _         <- ~log.info(msg"Moved ${files.length + 1} files to ${dest}")
  } yield Repo(repoId, remote, branch, commit, None)
}

case class Repo(id: RepoId, remote: Remote, branch: Branch, commit: Commit, local: Option[Path]) {
  def listFiles(layout: Layout)(implicit log: Log): Try[List[Path]] = for {
    gitDir <- localDir(layout).map(Success(_)).getOrElse(remote.get(layout))
    files  <- localDir(layout).fold(gitDir.lsTree(commit))(Success(gitDir.dir.children.map(Path(_))).waive)
  } yield files

  def ref(layer: Pointer): RepoRef = RepoRef(id, layer)

  def branch(layout: Layout)(implicit log: Log): Branch =
    localDir(layout).flatMap(_.branch.toOption).getOrElse(branch)

  def fullCheckout(layout: Layout)(implicit log: Log): Snapshot =
    Snapshot(id, remote, localDir(layout), commit, branch, List())

  def localDir(layout: Layout)(implicit log: Log): Option[GitDir] =
    local.map { p => GitDir(layout.baseDir.resolve(p))(layout.env) }

  def changes(layout: Layout)(implicit log: Log): Try[Option[DiffStat]] = for {
    repoDir <- localDir(layout).map(Success(_)).getOrElse(remote.fetch(layout))
    commit  <- repoDir.commit
    changes <- repoDir.diffShortStat(Some(commit))
  } yield changes

  def pull(layout: Layout)(implicit log: Log): Try[Commit] =
    remote.pull(commit, layout)

  def isForked(): Try[Unit] = local.ascribe(RepoNotForked(id)).map { _ => () }
  def isNotForked(): Try[Unit] = local.fold(Try(())) { dir => Failure(RepoAlreadyForked(id, dir)) }

  def current(layout: Layout)(implicit log: Log): Try[Commit] = for {
    dir    <- localDir(layout).map(Success(_)).getOrElse(remote.fetch(layout))
    commit <- dir.commit
  } yield Commit(commit.id)

  def sourceCandidates(layout: Layout)(pred: String => Boolean)(implicit log: Log): Try[Set[Source]] =
    listFiles(layout).map(_.filter { f => pred(f.filename) }.map { p =>
        RepoSource(id, p.parent, Glob.All): Source }.to[Set])
  
  def unfork(layout: Layout)(implicit log: Log): Try[Repo] = {
    for {
      dir        <- local.ascribe(RepoNotForked(id))
      relDir     =  dir.relativizeTo(layout.pwd)
      forkCommit =  GitDir(dir)(layout.env).commit
      goalCommit =  forkCommit match {
        case Success(fc) =>
          if(fc != commit) log.info(msg"Updating $id commit to $fc of $relDir")
          changes(layout) match {
            case Success(Some(changes)) =>
              log.warn(msg"Uncommitted changes ($changes) in $relDir will not apply to the unforked repo")
            case Failure(e) =>
              log.warn(msg"Could not check $id at $relDir for uncommitted changes. Cause: ${e.getMessage}")
            case _ => ()
          }
          fc
        case Failure(_) =>
          log.warn(msg"Unforking $id from $relDir which does not exist or is not a Git repository")
          commit
      }
    } yield copy(local = None, commit = goalCommit)
  }

  def conflict(layout: Layout, local: Option[Repo])
              (implicit log: Log)
              : Try[List[Path]] = for {
    current   <- ~layout.baseDir.children.to[Set].map(Path(_))
    removed   <- local.flatMap(_.local).fold(Try(List[Path]()))(GitDir(_)(layout.env).trackedFiles)
    bareRepo  <- remote.fetch(layout)
    files     <- bareRepo.lsRoot(commit)
    remaining <- Try((current -- removed) - path".fury/config")
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
