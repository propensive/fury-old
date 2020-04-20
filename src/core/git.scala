/*

    Fury, version 0.14.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

import fury.io._, fury.strings._, fury.model._

import guillotine._
import gastronomy._
import mercator._
import euphemism._

import scala.util._
import scala.collection.mutable.HashMap
import java.util.UUID
import java.util.jar.{JarFile, Manifest => JManifest}
import java.util.zip.ZipFile
import java.io._
import org.apache.commons.compress.archivers.zip.{ParallelScatterZipCreator, ZipArchiveEntry,
    ZipArchiveOutputStream}
import java.util.zip.ZipInputStream

object GitDir {

  def supplementEnv(env: Environment): Environment =
    env.append("GIT_SSH_COMMAND", "ssh -o BatchMode=yes").append("GIT_TERMINAL_PROMPT", "0")

  def apply(dir: Path)(implicit env: Environment): GitDir = GitDir(supplementEnv(env), dir)
  def apply(layout: Layout): GitDir = GitDir(layout.baseDir)(layout.env)
  implicit val hashable: Hashable[GitDir] = (acc, value) => implicitly[Hashable[Path]].digest(acc, value.dir)
}

object DiffStat {
  implicit val stringShow: StringShow[DiffStat] = _.value
  implicit val msgShow: MsgShow[DiffStat] = ds => UserMsg(_.italic(ds.value))
}

case class DiffStat(value: String)

case class RemoteGitDir(env: Environment, repo: Repo) {
  
  private case class RefSpec(commit: Commit, name: String)
  
  private def parseRefSpec(value: String): List[RefSpec] =
    value.split("\n").map { line => RefSpec(Commit(line.take(40)), line.split("/", 3).last) }.to[List]

  /*def commits(): Try[List[Commit]] =
    sh"git ls-remote --refs --tags --heads ${repo.ref}".exec[Try[String]]()(implicitly,
        GitDir.supplementEnv(env)).map(parseRefSpec(_).map(_.commit))*/

  def tags(): Try[List[Tag]] =
    sh"git ls-remote --refs --tags ${repo.ref}".exec[Try[String]]()(implicitly,
        GitDir.supplementEnv(env)).map(parseRefSpec(_).map { rs => Tag(rs.name) })

  def branches(): Try[List[Branch]] =
    sh"git ls-remote --refs --heads ${repo.ref}".exec[Try[String]]()(implicitly,
        GitDir.supplementEnv(env)).map(parseRefSpec(_).map { rs => Branch(rs.name) })

  /*def lsRemoteRefSpec(repo: Repo, branch: Branch)(implicit env: Environment): Try[Commit] =
    sh"git ls-remote ${repo.ref} ${branch.id}".exec[Try[String]]()(implicitly,
        supplementEnv(env)).map { r => Commit(r.take(40)) }*/

}

case class GitDir(env: Environment, dir: Path) {

  private implicit val environment: Environment = env
  private def git = List("git", "-C", dir.value)
  
  def cloneBare(repo: Repo): Try[Unit] =
    sh"git clone --mirror ${repo.ref} ${dir.value}".exec[Try[String]].map { out =>
      (dir / ".done").touch()
    }

  def remote: Try[Repo] = sh"$git config --get remote.origin.url".exec[Try[String]].map(Repo.parse(_, true))

  def diffShortStat(other: Option[Commit] = None): Try[Option[DiffStat]] = { other match {
    case None =>
      sh"git --work-tree ${dir.value} -C ${(dir / ".git").value} diff --shortstat"
    case Some(commit) =>
      sh"git --work-tree ${dir.value} -C ${(dir / ".git").value} diff --shortstat ${commit.id}"
  } }.exec[Try[String]].map { s => if(s.isEmpty) None else Some(DiffStat(s)) }

  def mergeConflicts: Try[(Commit, Commit)] =
    sh"$git log --merge --format=%H".exec[Try[String]].map(_.split("\n", 2) match {
      case Array(left, right) => (Commit(left), Commit(right))
    })

  def mergeBase(left: Commit, right: Commit): Try[Commit] =
    sh"$git merge-base ${left.id} ${right.id}".exec[Try[String]].map(Commit(_))

  def logMessage(commit: Commit): Try[String] =
    sh"$git log ${commit.id} --format=%s --max-count=1".exec[Try[String]]

  def sparseCheckout(from: Path, sources: List[Path], branch: Branch, commit: Commit, remote: Option[Repo])
                    : Try[Unit] = for {
    _ <- sh"$git init".exec[Try[String]]
    _ <- if(!sources.isEmpty) sh"$git config core.sparseCheckout true".exec[Try[String]] else Success(())
    _ <- ~(dir / ".git" / "info" / "sparse-checkout").writeSync(sources.map(_.value + "/*\n").mkString)
    _ <- sh"$git remote add origin ${from.value}".exec[Try[String]]
    _ <- sh"$git fetch --all".exec[Try[String]]
    _ <- sh"$git checkout ${commit.id}".exec[Try[String]]

    _ <- ~remote.foreach { repo => for {
           _ <- sh"$git remote remove origin".exec[Try[String]]
           _ <- sh"$git remote add origin ${repo.ref}".exec[Try[String]]
           _ <- sh"$git checkout -b ${branch.id}".exec[Try[String]]
           _ <- sh"$git fetch".exec[Try[String]]
           _ <- sh"$git branch -u origin/${branch.id}".exec[Try[String]]
         } yield () }

    _ <- sources.map(_.in(dir)).traverse(_.setReadOnly())
    _ <- ~(dir / ".done").touch()
  } yield ()

  def lsTree(commit: Commit): Try[List[Path]] = for {
    string <- sh"$git ls-tree -r --name-only ${commit.id}".exec[Try[String]]
  } yield string.split("\n").to[List].map(Path(_))

  def lsRoot(commit: Commit): Try[List[Path]] = for {
    string <- sh"$git ls-tree --name-only ${commit.id}".exec[Try[String]]
  } yield string.split("\n").to[List].map(Path(_))

  def branches: Try[List[Branch]] = for {
    refs  <- sh"$git show-ref --heads".exec[Try[String]]
  } yield refs.split("\n").to[List].map(_.split("/", 3).last).map(Branch(_))

  def tags: Try[List[Tag]] = for {
    refs  <- sh"$git show-ref --tags".exec[Try[String]]
  } yield refs.split("\n").to[List].map(_.split("/", 3).last).map(Tag(_))

  def remoteHasCommit(commit: Commit, branch: Branch): Try[Boolean] =
    sh"$git rev-list origin/${branch.id}".exec[Try[String]].map(_.split("\n").contains(commit.id))

  def fetch(branch: Option[Branch]): Try[Unit] =
    sh"$git fetch origin ${branch.getOrElse(Branch.master).id}".exec[Try[String]].map(_.unit)

  def branch: Try[Branch] = sh"$git rev-parse --abbrev-ref HEAD".exec[Try[String]].map(Branch(_))
  def cat(path: Path): Try[String] = sh"$git show HEAD:${path.value}".exec[Try[String]]
  def cat(commit: Commit, path: Path): Try[String] = sh"$git show ${commit.id}:${path.value}".exec[Try[String]]
  def commit: Try[Commit] = sh"$git rev-parse HEAD".exec[Try[String]].map(Commit(_))
  def commitFromTag(tag: Tag): Try[Commit] = sh"$git rev-parse ${tag.id}".exec[Try[String]].map(Commit(_))
  
  def commitFromBranch(branch: Branch): Try[Commit] =
    sh"$git rev-parse ${branch.id}".exec[Try[String]].map(Commit(_))

  def branchesFromCommit(commit: Commit): Try[List[Branch]] =
    sh"$git branch --contains ${commit.id}".exec[Try[String]].map { out =>
      out.split("\n").to[List].map(_.drop(2)).map(Branch(_))
    }

  def contains(commit: Commit): Try[Unit] =
    sh"$git branch --contains ${commit.id}".exec[Try[String]].map(_.unit).recoverWith { case e =>
      Failure(CommitNotInRepo(commit))
    }

  def someBranchFromCommit(commit: Commit): Try[Branch] =
    branchesFromCommit(commit).flatMap { bs =>
      bs.find(_ == Branch.master).orElse(bs.headOption).ascribe(BranchNotFound(commit))
    }

  def someBranchFromTag(tag: Tag): Try[Branch] = commitFromTag(tag).flatMap(someBranchFromCommit(_))

  def trackedFiles: Try[List[Path]] =
    sh"$git ls-tree --name-only HEAD".exec[Try[String]].map(_.split("\n").to[List].map(Path(_)))

  def allCommits: Try[List[Commit]] =
    sh"$git rev-list --all".exec[Try[String]].map(_.split("\n").to[List].map(Commit(_)))

  def allTrackedFiles: Try[List[Path]] =
    sh"$git ls-tree -r --name-only HEAD".exec[Try[String]].map(_.split("\n").to[List].map(Path(_)))

  def branchHead(branch: Branch): Try[Commit] =
    sh"$git show-ref -s heads/${branch.id}".exec[Try[String]].map(Commit(_))

  def getTag(tag: Branch): Try[Commit] = sh"$git show-ref -s tags/${tag.id}".exec[Try[String]].map(Commit(_))
}
