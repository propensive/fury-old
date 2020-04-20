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
package fury.core

import fury.model._, fury.io._, fury.strings._, fury.ogdl._
import mercator._

import scala.util._


object SourceRepo {
  implicit val msgShow: MsgShow[SourceRepo] = r => UserMsg(_.repo(r.id.key))
  implicit val stringShow: StringShow[SourceRepo] = _.id.key
  implicit def diff: Diff[SourceRepo] = Diff.gen[SourceRepo]

  def checkin(layout: Layout, local: SourceRepo, https: Boolean)(implicit log: Log): Try[Unit] = for {
    gitDir <- ~GitDir(layout.baseDir)(layout.env)
    dirty  <- gitDir.diffShortStat()
    _      <- if(dirty.isEmpty) Try(()) else Failure(RepoDirty(local.id, dirty.get.value))
    commit <- gitDir.commit
    branch <- gitDir.branch
    origin <- gitDir.origin
    pushed <- gitDir.remoteHasCommit(commit, branch)
    _      <- if(pushed) Try(()) else Failure(RemoteNotSynched(local.id, origin.ref))
    name   <- local.repo.projectName
    dest   <- Try((Xdg.runtimeDir / str"$name.bak").uniquify())
    files  <- gitDir.trackedFiles
    _      <- ~log.info(msg"Moving working directory contents to $dest")
    _      <- files.traverse { f => f.in(layout.baseDir).moveTo(f.in(dest)) }
    _      <- (layout.baseDir / ".git").moveTo(dest / ".git")
    _      <- ~log.info(msg"Moved ${files.length + 1} files to ${dest}")
  } yield ()
}

case class SourceRepo(id: RepoId, repo: Repo, branch: Branch, commit: Commit, local: Option[Path]) {
  def listFiles(layout: Layout, https: Boolean)(implicit log: Log): Try[List[Path]] = for {
    gitDir <- localDir(layout).map(Success(_)).getOrElse(repo.get(layout, https))
    files  <- localDir(layout).fold(gitDir.lsTree(commit))(Success(gitDir.dir.children.map(Path(_))).waive)
  } yield files

  def branch(layout: Layout)(implicit log: Log): Branch =
    localDir(layout).flatMap(_.branch.toOption).getOrElse(branch)

  def fullCheckout(layout: Layout)(implicit log: Log): Checkout =
    Checkout(id, repo, localDir(layout), commit, branch, List())

  private[this] var thisLocalDir: Option[Option[Path]] = None

  def localDir(layout: Layout)(implicit log: Log): Option[GitDir] = local.map(GitDir(_)(layout.env)).orElse {
    Repo.local(layout).map(_.equivalentTo(repo)) match {
      case Success(true) =>
        thisLocalDir.getOrElse {
          log.info(msg"Commandeering the working directory as the repository $id")
          GitDir(layout.baseDir)(layout.env).diffShortStat(Some(commit)).foreach { diff =>
            diff.foreach { diff =>
              log.warn(msg"The working directory differs from the repo in the layer: $diff")
            }
          }
          val result = Some(layout.baseDir)
          thisLocalDir = Some(result)
          result
        }.map(GitDir(_)(layout.env))

      case _ =>
        None
    }
  }

  def changes(layout: Layout, https: Boolean)(implicit log: Log): Try[Option[DiffStat]] = for {
    repoDir <- localDir(layout).map(Success(_)).getOrElse(repo.fetch(layout, https))
    changes <- repoDir.diffShortStat()
  } yield changes

  def pull(layout: Layout, https: Boolean)(implicit log: Log): Try[Commit] =
    repo.pull(commit, layout, https)

  def isForked(): Try[Unit] = local.ascribe(RepoNotForked(id)).map { _ => () }
  def isNotForked(): Try[Unit] = local.fold(Try(())) { dir => Failure(RepoAlreadyForked(id, dir)) }

  def current(layout: Layout, https: Boolean)(implicit log: Log): Try[Commit] = for {
    dir    <- localDir(layout).map(Success(_)).getOrElse(repo.fetch(layout, https))
    commit <- dir.commit
  } yield Commit(commit.id)

  def sourceCandidates(layout: Layout, https: Boolean)
                      (pred: String => Boolean)
                      (implicit log: Log)
                      : Try[Set[Source]] =
    listFiles(layout, https).map(_.filter { f => pred(f.filename) }.map { p =>
        ExternalSource(id, p.parent, Glob.All): Source }.to[Set])
  
  def unfork(layout: Layout, https: Boolean)(implicit log: Log): Try[SourceRepo] = for {
    _          <- if(local.isDefined) Success(()) else Failure(RepoNotForked(id))
    dir        <- ~local.get
    forkCommit <- GitDir(dir)(layout.env).commit
    relDir     <- ~(dir.relativizeTo(layout.pwd))
    _          <- Try(if(forkCommit != commit) log.info(msg"Updating $id commit to $forkCommit of $relDir"))
    changes    <- changes(layout, https)
    
    _          <- Try(changes.foreach { cs => log.warn(
                      msg"Uncommitted changes ($cs) in $relDir will not apply to the unforked repo") })

  } yield copy(local = None, commit = forkCommit)

  def checkout(layout: Layout, local: Option[SourceRepo], https: Boolean)(implicit log: Log): Try[Unit] = for {
    _         <- isNotForked()
    conflicts <- conflict(layout, local, https).map { fs => if(fs.isEmpty) None else Some(fs) }
    _         <- conflicts.fold(Try(())) { files => Failure(ConflictingFiles(files)) }
    _         <- local.fold(Try(())) { local => SourceRepo.checkin(layout, local, https) }
    _         <- doCleanCheckout(layout, https)
  } yield ()

  def conflict(layout: Layout, local: Option[SourceRepo], https: Boolean)
              (implicit log: Log)
              : Try[List[Path]] = for {
    current   <- ~layout.baseDir.children.to[Set].map(Path(_))
    removed   <- local.flatMap(_.local).fold(Try(List[Path]()))(GitDir(_)(layout.env).trackedFiles)
    bareRepo  <- repo.fetch(layout, https)
    files     <- bareRepo.lsRoot(commit)
    remaining <- Try((current -- removed) - Path(".fury.conf"))
  } yield remaining.intersect(files.to[Set]).to[List]

  def doCleanCheckout(layout: Layout, https: Boolean)(implicit log: Log): Try[Unit] = for {
    _          <- ~log.info(msg"Checking out ${repo} to ${layout.baseDir.relativizeTo(layout.pwd)}")
    bareRepo   <- repo.fetch(layout, https)
    _          <- ~layout.confFile.moveTo(layout.confFileBackup)
    gitDir     <- ~GitDir(layout.baseDir)(layout.env)
    sourceRepo <- gitDir.sparseCheckout(bareRepo.dir, List(), branch, commit, Some(repo.universal(false)))
    _          <- ~layout.confFileBackup.moveTo(layout.confFile)
  } yield ()
}