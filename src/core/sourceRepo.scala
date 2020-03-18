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
package fury.core

import fury.model._, fury.io._, fury.strings._, fury.ogdl._
import mercator._

import scala.util._


object SourceRepo {
  implicit val msgShow: MsgShow[SourceRepo] = r => UserMsg(_.repo(r.id.key))
  implicit val stringShow: StringShow[SourceRepo] = _.id.key
  implicit def diff: Diff[SourceRepo] = Diff.gen[SourceRepo]

  def checkin(layout: Layout, local: SourceRepo, https: Boolean)(implicit log: Log): Try[Unit] = for {
    dirty  <- Shell(layout.env).git.diffShortStat(layout.pwd)
    _      <- if(dirty.isEmpty) Try(()) else Failure(RepoDirty(local.id, dirty))
    commit <- Shell(layout.env).git.getCommit(layout.pwd)
    branch <- Shell(layout.env).git.currentBranch(layout.pwd)
    origin <- Shell(layout.env).git.getOrigin(layout.pwd)
    pushed <- Shell(layout.env).git.remoteHasCommit(layout.pwd, commit, branch)
    _      <- if(pushed) Try(()) else Failure(RemoteNotSynched(local.id, origin))
    name   <- local.repo.projectName
    dest   <- Try((Xdg.runtimeDir / str"$name.bak").lowestNumberedNonexistentDir())
    files  <- Shell(layout.env).git.getTrackedFiles(layout.pwd)
    _      <- ~log.info(msg"Moving working directory contents to $dest")
    _      <- files.traverse { f => (layout.pwd / f).moveTo(dest / f) }
    _      <- (layout.pwd / ".git").moveTo(dest / ".git")
    _      <- ~log.info(msg"Moved ${files.length + 1} files to ${dest}")
  } yield ()
}

case class SourceRepo(id: RepoId, repo: Repo, track: RefSpec, commit: Commit, local: Option[Path]) {
  def listFiles(layout: Layout, https: Boolean)(implicit log: Log): Try[List[Path]] = for {
    dir    <- localDir(layout).map(Success(_)).getOrElse(repo.get(layout, https))
    /*refSpec <- ~Shell(layout.env).git.getTag(dir, track.id).toOption.orElse(Shell(layout.env).git.
                  getBranchHead(dir, track.id).toOption).getOrElse(track)*/
    files  <- localDir(layout).map(Success(dir.children.map(Path(_))).waive).getOrElse(
                  Shell(layout.env).git.lsTree(dir, commit))
  } yield files

  def tracking(layout: Layout): Option[RefSpec] = localDir(layout).fold(Option(track)) { dir =>
    Shell(layout.env).git.getBranch(dir).toOption.map(RefSpec(_))
  }

  def fullCheckout(layout: Layout): Checkout = Checkout(id, repo, localDir(layout), commit, track, List())

  def localDir(layout: Layout): Option[Path] = local.orElse {
    Repo.local(layout).map(_.simplified == repo.simplified) match {
      case Success(true) => Some(layout.baseDir)
      case _             => None
    }
  }

  def changes(layout: Layout, https: Boolean)(implicit log: Log): Try[Option[String]] = for {
    repoDir <- localDir(layout).map(Success(_)).getOrElse(repo.fetch(layout, https))
    changes <- Shell(layout.env).git.diffShortStat(repoDir)
  } yield if(changes.isEmpty) None else Some(changes)

  def importCandidates(schema: Schema, layout: Layout, https: Boolean)(implicit log: Log): Try[List[String]] =
    for {
      repoDir     <- repo.get(layout, https)

      confString  <- Shell(layout.env).git.showFile(repoDir, ".fury.conf").orElse {
                       Shell(layout.env).git.showFile(repoDir, ".focus.fury")
                     }

      conf        <- ~Ogdl.read[FuryConf](confString, identity(_))
      layer       <- Layer.retrieve(conf, quiet = false)
      schemas     <- ~layer.schemas.to[List]
    } yield schemas.map { schema => str"${id.key}:${schema.id.key}" }

  def pull(layout: Layout, https: Boolean)(implicit log: Log): Try[Commit] =
    repo.pull(commit, track, layout, https)

  def isForked(): Try[Unit] = local.ascribe(RepoNotForked(id)).map { _ => () }
  def isNotForked(): Try[Unit] = local.fold(Try(())) { dir => Failure(RepoAlreadyForked(id, dir)) }

  def current(layout: Layout, https: Boolean)(implicit log: Log): Try[RefSpec] = for {
    dir    <- localDir(layout).map(Success(_)).getOrElse(repo.fetch(layout, https))
    commit <- Shell(layout.env).git.getCommit(dir)
  } yield RefSpec(commit.id)

  def sourceCandidates(layout: Layout, https: Boolean)
                      (pred: String => Boolean)
                      (implicit log: Log)
                      : Try[Set[Source]] =
    listFiles(layout, https).map(_.filter { f => pred(f.filename) }.map { p =>
        ExternalSource(id, p.parent, Glob.All): Source }.to[Set])
  
  def unfork(layout: Layout, https: Boolean)(implicit log: Log): Try[SourceRepo] = for {
    _          <- if(local.isDefined) Success(()) else Failure(RepoNotForked(id))
    dir        <- ~local.get
    forkCommit <- Shell(layout.env).git.getCommit(dir)
    relDir     <- ~(dir.relativizeTo(layout.pwd))
    _          <- Try(if(forkCommit != commit) log.info(msg"Updating $id commit to $forkCommit of $relDir"))
    changes    <- changes(layout, https)
    
    _          <- Try(changes.foreach { cs => log.warn(
                      msg"Uncommitted changes ($cs) in $relDir will not apply to the unforked repo") })

  } yield copy(local = None, commit = forkCommit)

  def checkout(layout: Layout, local: Option[SourceRepo], https: Boolean)(implicit log: Log): Try[Unit] = for {
    _         <- isNotForked()
    conflicts <- conflict(layout, local, https).map { fs => if(fs.isEmpty) None else Some(fs) }
    _         <- conflicts.fold(Try(())) { files => Failure(ConflictingFiles(files.map(Path(_)))) }
    _         <- local.fold(Try(())) { local => SourceRepo.checkin(layout, local, https) }
    _         <- doCleanCheckout(layout, https)
  } yield ()

  def conflict(layout: Layout, local: Option[SourceRepo], https: Boolean)
              (implicit log: Log)
              : Try[List[String]] = for {
    current   <- ~layout.pwd.children.to[Set]
    removed   <- local.flatMap(_.local).fold(Try(List[String]()))(Shell(layout.env).git.getTrackedFiles(_))
    bareRepo  <- repo.fetch(layout, https)
    files     <- Shell(layout.env).git.lsRoot(bareRepo, commit)
    remaining <- Try(current -- removed)
  } yield remaining.intersect(files.to[Set]).to[List]

  def doCleanCheckout(layout: Layout, https: Boolean)(implicit log: Log): Try[Unit] = for {
    bareRepo   <- repo.fetch(layout, https)
    sourceRepo <- Shell(layout.env).git.sparseCheckout(bareRepo, layout.pwd, List(), track, commit,
                      Some(repo.universal(false)))
  } yield ()

}
