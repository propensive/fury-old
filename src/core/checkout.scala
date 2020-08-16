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
package fury.core

import fury.model._, fury.io._, fury.text._

import gastronomy._
import guillotine._
import mercator._

import scala.util._

case class Snapshots(snapshots: Map[SnapshotHash, Snapshot] = Map()) {
  def apply(repoId: RepoId): Try[Snapshot] =
    snapshots.find(_._2.repoId == repoId).map(_._2).ascribe(ItemNotFound(repoId))
  
  def apply(hash: SnapshotHash): Try[Snapshot] =
    snapshots.get(hash).ascribe(ItemNotFound(hash))
  def ++(that: Snapshots): Snapshots = Snapshots(snapshots ++ that.snapshots)

  def dir(source: Source, layout: Layout): Try[Path] = sourceBase(source, layout).map(source.dir in _)
  
  def files(source: Source, layout: Layout): Try[Stream[Path]] =
    dir(source, layout).map { dir => source.glob(dir, dir.walkTree) }
  
  def copy(source: Source, layout: Layout, destination: Path)(implicit log: Log): Try[Unit] = for {
    baseDir  <- dir(source, layout)
    allFiles <- files(source, layout)
    _        <- allFiles.to[List].map { f =>
                  f.relativizeTo(baseDir).in(destination).mkParents().map(f.copyTo(_))
                }.sequence
  } yield ()

  def fileCount(source: Source, layout: Layout): Try[Int] = files(source, layout).map(_.length)

  def totalSize(source: Source, layout: Layout): Try[ByteSize] =
    files(source, layout).map(_.map(_.size).reduce(_ + _))

  def linesOfCode(source: Source, layout: Layout): Try[Int] =
    for(pathStream <- files(source, layout); lines <- pathStream.traverse(_.lines))
    yield lines.map(_.size).sum
  
  def sourceBase(source: Source, layout: Layout): Try[Path] = source match {
    case RepoSource(repoId, dir, glob) =>
      apply(repoId).map { checkout => checkout.local.fold(checkout.path)(_.dir) }
    case LocalSource(dir, glob) =>
      ~layout.baseDir
  }
  
  def sourceHash(layer: Layer, source: Source, layout: Layout): Try[Digest] = source match {
    case RepoSource(repoId, dir, glob) => layer.repos.findBy(repoId).map((dir, _).digest[Md5])
    case LocalSource(dir, glob)        => Success((-1, dir).digest[Md5])
  }
}

case class Snapshot(repoId: RepoId,
                    remote: Remote,
                    local: Option[GitDir],
                    commit: Commit,
                    branch: Branch,
                    sources: List[Path]) {

  def hash: SnapshotHash = SnapshotHash(this.digest[Md5])
  def path: Path = Installation.srcsDir / hash.hash.encoded[Hex].take(16)

  def get(layout: Layout)(implicit log: Log): Try[GitDir] = for {
    repoDir    <- remote.get(layout)
    workingDir <- checkout(layout)
  } yield workingDir

  private def checkout(layout: Layout)(implicit log: Log): Try[GitDir] =
    local.map(Success(_)).getOrElse {
      val sourceDesc: UserMsg = sources match {
        case List() =>
          UserMsg(_.path("*"))
        case head :: Nil =>
          msg"$head"
        case head :: tail =>
          val init = tail.foldLeft(msg"${'{'}$head") { case (str, next) => msg"$str${','} $next" }
          msg"$init${'}'}"
      }

      if(path.exists && !(path / ".done").exists) {
        log.info(msg"Found incomplete checkout of ${sourceDesc}")
        path.delete()
      }

      val gitDir = GitDir(path)(layout.env)

      if(!path.exists) {
        log.info(msg"Checking out $sourceDesc from repository $repoId")
        path.mkdir()
        gitDir.sparseCheckout(remote.path(layout), sources, branch = branch,
            commit = commit, None).flatMap { _ => (path / ".git").delete() }.map(gitDir.waive).recoverWith {
          case e: ShellFailure if e.stderr.contains("Sparse checkout leaves no entry on working directory") =>
            Failure(NoSourcesError(repoId, commit, sourceDesc))
          case e: Exception =>
            Failure(e)
        }
      } else Success(gitDir)
    }
}