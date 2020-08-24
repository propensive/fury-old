/*

    Fury, version 0.18.2. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

import fury.io._, fury.text._, fury.model._

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

  private var useHttps: Boolean = false

  def supplementEnv(env: Environment): Environment =
    env.append("GIT_SSH_COMMAND", "ssh -o BatchMode=yes").append("GIT_TERMINAL_PROMPT", "0")

  def apply(dir: Path)(implicit env: Environment): GitDir = GitDir(supplementEnv(env), dir)
  def apply(layout: Layout): GitDir = GitDir(layout.baseDir)(layout.env)
  implicit val hashable: Hashable[GitDir] = (acc, value) => implicitly[Hashable[Path]].digest(acc, value.dir)

  def sshOrHttps(remote: Remote)(fn: String => Command)(implicit env: Environment): Try[String] = {
    val neither = env.workDir.flatMap { d => remote.local(Path(d)) }.getOrElse(remote.ref)
    if(useHttps) fn(remote.https.getOrElse(neither)).exec[Try[String]]
    else fn(remote.ssh.getOrElse(neither)).exec[Try[String]].orElse {
        fn(remote.https.getOrElse(neither)).exec[Try[String]].map { result =>
      useHttps = true
      result
    } }
  }
}

object DiffStat {
  implicit val stringShow: StringShow[DiffStat] = _.value
  implicit val msgShow: MsgShow[DiffStat] = ds => UserMsg(_.italic(ds.value))
}

case class DiffStat(value: String)

case class RemoteGitDir(env: Environment, remote: Remote) {
  
  private case class RefSpec(commit: Commit, name: String)
  implicit val newEnv = GitDir.supplementEnv(env)

  private def parseRefSpec(value: String): List[RefSpec] =
    value.split("\n").map { line => RefSpec(Commit(line.take(40)), line.split("/", 3).last) }.to[List]

  def tags(): Try[List[Tag]] = GitDir.sshOrHttps(remote) { r => sh"git ls-remote --refs --tags ${r}"
      }.map(parseRefSpec(_).map(_.name).map(Tag(_)))

  def branches(): Try[List[Branch]] = GitDir.sshOrHttps(remote) { r => sh"git ls-remote --refs --heads $r"
      }.map(parseRefSpec(_).map(_.name).map(Branch(_)))
  
  def findCommit(branch: Branch): Try[Option[Commit]] =
    GitDir.sshOrHttps(remote) { r => sh"git ls-remote --refs --heads $r"
  }.map(parseRefSpec(_).find(_.name == branch.id).map(_.commit))
  
  def findCommit(tag: Tag): Try[Option[Commit]] =
    GitDir.sshOrHttps(remote) { r => sh"git ls-remote --refs --tags $r"
  }.map(parseRefSpec(_).find(_.name == tag.id).map(_.commit))
}

case class GitDir(env: Environment, dir: Path) {

  private implicit val environment: Environment = env
  private def git = sh"git -C $dir"
  
  def cloneBare(remote: Remote): Try[Unit] =
    GitDir.sshOrHttps(remote) { r => sh"git clone --mirror $r $dir" }.map { out => (dir / ".done").touch() }

  def clone(remote: Remote, branch: Branch, commit: Commit): Try[Unit] = for {
    _ <- GitDir.sshOrHttps(remote) { r => sh"git clone $r --branch=$branch $dir" }
    _ <- sh"$git reset $commit".exec[Try[String]]
  } yield ()

  def remote: Try[Remote] = sh"$git config --get remote.origin.url".exec[Try[String]].map(Remote.parse(_))

  def diffShortStat(other: Option[Commit] = None): Try[Option[DiffStat]] = { other match {
    case None =>
      sh"git --work-tree $dir -C ${dir / ".git"} diff --shortstat"
    case Some(commit) =>
      sh"git --work-tree $dir -C ${dir / ".git"} diff --shortstat $commit"
  } }.exec[Try[String]].map { s => if(s.isEmpty) None else Some(DiffStat(s)) }

  def mergeConflicts: Try[(Commit, Commit)] =
    sh"$git log --merge --format=%H".exec[Try[String]].map(_.split("\n", 2) match {
      case Array(left, right) => (Commit(left), Commit(right))
    })

  def mergeBase(left: Commit, right: Commit): Try[Commit] =
    sh"$git merge-base $left $right".exec[Try[String]].map(Commit(_))

  def logMessage(commit: Commit): Try[String] =
    sh"$git log $commit --format=%s --max-count=1".exec[Try[String]]

  def sparseCheckout(from: Path, sources: List[Path], branch: Branch, commit: Commit, remote: Option[Remote])
                    : Try[Unit] = for {
    _ <- sh"$git init".exec[Try[String]]
    // FIXME: Something in here is not checkout out the working tree
    // Do a standard checkout, not a separated one
    _ <- if(!sources.isEmpty) sh"$git config core.sparseCheckout true".exec[Try[String]] else Success(())
    _ <- ~(dir / ".git" / "info" / "sparse-checkout").writeSync(sources.map(_.value + "/*\n").mkString)
    _ <- sh"$git remote add origin $from".exec[Try[String]]
    _ <- sh"$git fetch --all".exec[Try[String]]
    _ <- sh"$git checkout $commit".exec[Try[String]]

    _ <- ~remote.foreach { remote => for {
           _ <- sh"$git remote remove origin".exec[Try[String]]
           fullRef = remote.https.getOrElse(remote.ref)
           _ <- sh"$git remote add origin ${fullRef}".exec[Try[String]]
           _ <- sh"$git checkout -b ${branch.id}".exec[Try[String]]
           _ <- sh"$git fetch".exec[Try[String]]
           _ <- sh"$git branch -u origin/$branch".exec[Try[String]]
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

  def tags: Try[List[Tag]] = Try {
    sh"$git show-ref --tags".exec[Try[String]].toOption.getOrElse("").split("\n").to[List].map(_.split("/",
        3).last).map(Tag(_))
  }

  def remoteHasCommit(commit: Commit, branch: Branch): Try[Boolean] =
    sh"$git rev-list origin/$branch".exec[Try[String]].map(_.split("\n").contains(commit.id))

  def add(path: Path, force: Boolean = false): Try[Unit] = {
    val forceArg: List[String] = if(force) List("--force") else Nil
    sh"$git add $forceArg $path".exec[Try[String]].munit
  }

  def fetch(branch: Branch): Try[Unit] = sh"$git fetch origin $branch".exec[Try[String]].munit
  def fetch(): Try[Unit] = sh"$git fetch --all".exec[Try[String]].munit
  def branch: Try[Branch] = sh"$git rev-parse --abbrev-ref HEAD".exec[Try[String]].map(Branch(_))
  def cat(path: Path): Try[String] = sh"$git show HEAD:$path".exec[Try[String]]
  def cat(commit: Commit, path: Path): Try[String] = sh"$git show $commit:$path".exec[Try[String]]
  def commit: Try[Commit] = sh"$git rev-parse HEAD".exec[Try[String]].map(Commit(_))
  def findCommit(tag: Tag): Try[Commit] = sh"$git rev-parse $tag".exec[Try[String]].map(Commit(_))
  def findCommit(branch: Branch): Try[Commit] = checkBranch(branch) >>= (_ => sh"$git rev-parse $branch".exec[Try[String]].map(Commit(_)))

  def branchesFromCommit(commit: Commit): Try[List[Branch]] =
    sh"$git branch --contains $commit --format='%(refname:short)'".exec[Try[String]].map { out =>
      out.split("\n").to[List].map(Branch(_))
    }

  def checkBranch(branch: Branch): Try[Branch] =
    branches.flatMap(_.find(_ == branch).ascribe(BranchDoesNotExist(branch)))

  def contains(commit: Commit): Try[Unit] =
    sh"$git branch --contains $commit --format='%(refname:short)'".exec[Try[String]].munit.recoverWith {
        case e => Failure(CommitNotInRepo(commit)) }

  def checkCommit(commit: Commit): Try[Commit] = contains(commit).map(commit.waive)

  def resolve(refSpec: RefSpec): Try[Commit] = refSpec match {
    case c: Commit => checkCommit(c)
    case t: Tag    => findCommit(t)
    case b: Branch => findCommit(b)
  }

  def chooseBranch(refSpec: RefSpec): Try[Branch] = refSpec match {
    case c: Commit => someBranchFromCommit(c)
    case b: Branch => checkBranch(b)
    case t: Tag    => someBranchFromTag(t)
  }

  def message(commit: Commit): Try[String] = sh"$git log --pretty=%s -1 $commit".exec[Try[String]]

  def someBranchFromCommit(commit: Commit): Try[Branch] =
    branchesFromCommit(commit).flatMap(_.headOption.ascribe(BranchNotFound(commit)))

  def someBranchFromTag(tag: Tag): Try[Branch] = findCommit(tag).flatMap(someBranchFromCommit(_))

  def trackedFiles: Try[List[Path]] =
    sh"$git ls-tree --name-only HEAD".exec[Try[String]].map(_.split("\n").to[List].map(Path(_)))

  def allCommits: Try[List[Commit]] =
    sh"$git rev-list --all".exec[Try[String]].map(_.split("\n").to[List].map(Commit(_)))

  def allTrackedFiles: Try[List[Path]] =
    sh"$git ls-tree -r --name-only HEAD".exec[Try[String]].map(_.split("\n").to[List].map(Path(_)))

  def untrackedFiles: Try[List[Path]] =
    sh"$git ls-files --exclude-standard --others".exec[Try[String]].map(_.split("\n").to[List].map(Path(_)))

  def branchHead(branch: Branch): Try[Commit] =
    sh"$git show-ref -s heads/$branch".exec[Try[String]].map(Commit(_))

  def getTag(tag: Branch): Try[Commit] = sh"$git show-ref -s tags/$tag".exec[Try[String]].map(Commit(_))
}