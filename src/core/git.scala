/*

    Fury, version 0.12.3. Copyright 2018-20 Jon Pretty, Propensive OÜ.

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
import mercator._
import euphemism._

import scala.util._
import scala.collection.mutable.HashMap
import java.util.UUID
import java.util.jar.{JarFile, Manifest => JManifest}
import java.util.zip.ZipFile
import java.io._
import org.apache.commons.compress.archivers.zip.{ParallelScatterZipCreator, ZipArchiveEntry, ZipArchiveOutputStream}
import java.util.zip.ZipInputStream

object Cached {
  val lsRemote: HashMap[String, Try[List[String]]] = new HashMap()
  val lsRemoteRefSpec: HashMap[(String, String), Try[String]] = new HashMap()
}

object GitDir {

  def supplementEnv(env: Environment): Environment = env.append("GIT_SSH_COMMAND", "ssh -o BatchMode=yes")

  def lsRemote(repo: Repo)(implicit env: Environment): Try[List[String]] =
    Cached.lsRemote.getOrElseUpdate(repo.ref, sh"git ls-remote --tags --heads ${repo.ref}".exec[Try[String]]()(
        implicitly, supplementEnv(env)).map(_.split("\n").to[List].map(_.split("/").last)))

  def lsRemoteRefSpec(repo: Repo, refSpec: RefSpec)(implicit env: Environment): Try[String] =
    Cached.lsRemoteRefSpec.getOrElseUpdate((repo.ref, refSpec.id),
        sh"git ls-remote ${repo.ref} ${refSpec.id}".exec[Try[String]]()(implicitly,
        supplementEnv(env)).map(_.take(40)))

  def apply(dir: Path)(implicit env: Environment): GitDir =
    GitDir(supplementEnv(env), dir)
}

object DiffStat {
  implicit val stringShow: StringShow[DiffStat] = _.value
  implicit val msgShow: MsgShow[DiffStat] = ds => UserMsg(_.italic(ds.value))
}

case class DiffStat(value: String)

case class GitDir(env: Environment, dir: Path) {

  private implicit val environment: Environment = env
  private def git = List("git", "-c", dir.value)
  
  def cloneBare(repo: Repo): Try[Unit] =
    sh"git clone --mirror ${repo.ref} ${dir.value}".exec[Try[String]].map { out =>
      (dir / ".done").touch()
    }

  def origin: Try[Repo] = sh"$git config --get remote.origin.url".exec[Try[String]].map(Repo.parse(_, true))

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

  def sparseCheckout(from: Path, sources: List[Path], refSpec: RefSpec, commit: Commit, remote: Option[Repo])
                    : Try[Unit] = for {
    _ <- sh"$git init".exec[Try[String]]
    _ <- if(!sources.isEmpty) sh"$git config core.sparseCheckout true".exec[Try[String]] else Success(())
    _ <- ~(dir / ".git" / "info" / "sparse-checkout").writeSync(sources.map(_.value + "/*\n").mkString)
    _ <- sh"$git remote add origin ${from.value}".exec[Try[String]]
    _ <- sh"$git fetch --all".exec[Try[String]]
    _ <- sh"$git checkout ${commit.id}".exec[Try[String]]

    _ <- ~remote.foreach { repo =>
           for {
             _ <- sh"$git remote remove origin".exec[Try[String]]
             _ <- sh"$git remote add origin ${repo.ref}".exec[Try[String]]
             _ <- sh"$git checkout -b ${refSpec.id}".exec[Try[String]]
             _ <- sh"$git fetch".exec[Try[String]]
             _ <- sh"$git branch -u origin/${refSpec.id}".exec[Try[String]]
           } yield ()
         }

    _ <- sources.map(_.in(dir)).traverse(_.setReadOnly())
    _ <- ~(dir / ".done").touch()
  } yield ()

  def lsTree(commit: Commit): Try[List[Path]] = for {
    string <- sh"$git ls-tree -r --name-only ${commit.id}".exec[Try[String]]
  } yield string.split("\n").to[List].map(Path(_))

  def lsRoot(commit: Commit): Try[List[Path]] = for {
    string <- sh"$git ls-tree --name-only ${commit.id}".exec[Try[String]]
  } yield string.split("\n").to[List].map(Path(_))

  def showRefs: Try[List[RefSpec]] = for {
    refs  <- sh"$git show-ref --heads --tags".exec[Try[String]]
  } yield refs.split("\n").to[List].map(_.split("/").last).map(RefSpec(_))

  def remoteHasCommit(commit: Commit, track: RefSpec): Try[Boolean] =
    sh"$git rev-list origin/${track.id}".exec[Try[String]].map(_.split("\n").contains(commit.id))

  def fetch(refspec: Option[RefSpec]): Try[Unit] =
    sh"$git fetch origin ${refspec.to[List].map(_.id)}".exec[Try[String]].map(_.unit)

  def branch: Try[RefSpec] = sh"$git rev-parse --abbrev-ref HEAD".exec[Try[String]].map(RefSpec(_))
  def cat(path: Path): Try[String] = sh"$git show HEAD:${path.value}".exec[Try[String]]
  def cat(commit: Commit, path: Path): Try[String] = sh"$git show ${commit.id}:${path.value}".exec[Try[String]]
  def commit: Try[Commit] = sh"$git rev-parse HEAD".exec[Try[String]].map(Commit(_))
  def commitFromTag(tag: RefSpec): Try[Commit] = sh"$git rev-parse ${tag.id}".exec[Try[String]].map(Commit(_))

  def trackedFiles: Try[List[Path]] =
    sh"$git ls-tree --name-only HEAD".exec[Try[String]].map(_.split("\n").to[List].map(Path(_)))

  def allTrackedFiles: Try[List[Path]] =
    sh"$git ls-tree -r --name-only HEAD".exec[Try[String]].map(_.split("\n").to[List].map(Path(_)))

  def branchHead(branch: RefSpec): Try[Commit] =
    sh"$git show-ref -s heads/${branch.id}".exec[Try[String]].map(Commit(_))

  def getTag(tag: RefSpec): Try[Commit] = sh"$git show-ref -s tags/${tag.id}".exec[Try[String]].map(Commit(_))
}
