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

import fury.ogdl._, fury.io._, fury.model._, fury.text._

import gastronomy._
import kaleidoscope._
import jovian._
import mercator._

import scala.util._

object Source {
  implicit val stringShow: StringShow[Source] = _ match {
    case RepoSource(repoId, dir, _) => str"${repoId}:${dir.value}"
    case LocalSource(dir, _) => str"${dir.value}"
    case WorkspaceSource(workspaceId, path) => str"$workspaceId:$path"
  }
  implicit val ogdlReader: OgdlReader[Source] = src => unapply(src.id()).get // FIXME
  implicit val ogdlWriter: OgdlWriter[Source] = src => Ogdl(Vector("id" -> Ogdl(src.key)))
  implicit val parser: Parser[Source] = unapply(_)
  implicit val index: Index[Source] = FieldIndex("id")
  implicit val keyName: KeyName[Source] = () => msg"source"

  implicit val sourceDiff: Diff[Source] =
    (l, r) => if(l == r) Nil else List(Difference(msg"source", msg"", msg"$l", msg"$r"))

  implicit val msgShow: MsgShow[Source] = v => UserMsg { theme =>
    v match {
      case RepoSource(repoId, dir, glob) =>
        msg"$repoId${':'}$dir${'/'}${'/'}$glob".string(theme)
      case LocalSource(dir, glob) =>
        msg"$dir${'/'}${'/'}$glob".string(theme)
      case WorkspaceSource(workspaceId, path) =>
        msg"$workspaceId${':'}$path".string(theme)
    }
  }

  def unapply(string: String): Option[Source] = string.only {
    case r"$repo@([a-z][a-z0-9\.\-]*[a-z0-9]):$dir@([^\*\{\}\[\]]*)//$pattern@(.*)" =>
      RepoSource(RepoId(repo), Path(dir), Glob(pattern))
    case r"$repo@([a-z][a-z0-9\.\-]*[a-z0-9]):$dir@([^\*\{\}\[\]]*)" =>
      RepoSource(RepoId(repo), Path(dir), Glob.All)
    case r"$dir@([^\*\{\}\[\]]*)//$pattern@(.*)" =>
      LocalSource(Path(dir), Glob(pattern))
    case r"$dir@([^\*\{\}\[\]]*)" =>
      LocalSource(Path(dir), Glob.All)
  }

  def repoId(src: Source): Option[RepoId] = src.only { case RepoSource(repoId, _, _) => repoId }

  def rewriteLocal(source: Source, localId: Option[RepoId]): Source =
    localId.fold(source) { repoId => source match {
      case LocalSource(dir, glob) => RepoSource(repoId, dir, glob)
      case source => source
    } }
}

sealed abstract class Source extends Key(msg"source") {
  def key: String
  def completion: String
  def hash(layer: Layer): Try[Digest]
  def path: Path
  def glob: Glob
  def repoIdentifier: RepoId

  def base(snapshots: Snapshots, layout: Layout): Try[Path]
  def dir(snapshots: Snapshots, layout: Layout): Try[Path] = base(snapshots, layout).map(path in _)
  
  def files(snapshots: Snapshots, layout: Layout): Try[Stream[Path]] =
    dir(snapshots, layout).map { dir => glob(dir, dir.walkTree) }
  
  def copyTo(snapshots: Snapshots, layout: Layout, destination: Path)(implicit log: Log): Try[Unit] = for {
    baseDir  <- dir(snapshots, layout)
    allFiles <- files(snapshots, layout)
    _        <- allFiles.to[List].map { f =>
                  f.relativizeTo(baseDir).in(destination).mkParents().map(f.copyTo(_))
                }.sequence
  } yield ()

  def fileCount(snapshots: Snapshots, layout: Layout): Try[Int] = files(snapshots, layout).map(_.length)

  def totalSize(snapshots: Snapshots, layout: Layout): Try[ByteSize] =
    files(snapshots, layout).map(_.map(_.size).reduce(_ + _))

  def linesOfCode(snapshots: Snapshots, layout: Layout): Try[Int] = for {
    pathStream <- files(snapshots, layout)
    lines      <- pathStream.traverse(_.lines)
  } yield lines.map(_.size).sum
}

case class RepoSource(repoId: RepoId, path: Path, glob: Glob) extends Source {
  def key: String = str"${repoId}:${path.value}//$glob"
  def completion: String = str"${repoId}:${path.value}"
  def repoIdentifier: RepoId = repoId
  def hash(layer: Layer): Try[Digest] = layer.repos.findBy(repoId).map((path, _).digest[Md5])
  def base(snapshots: Snapshots, layout: Layout): Try[Path] =
    snapshots(repoId).map { checkout => checkout.local.fold(checkout.path)(_.dir) }
}

case class LocalSource(path: Path, glob: Glob) extends Source {
  def key: String = str"$path//$glob"
  def completion: String = path.value
  def hash(layer: Layer): Try[Digest] = Success((-1, path).digest[Md5])
  def repoIdentifier: RepoId = RepoId("local")
  def base(snapshots: Snapshots, layout: Layout): Try[Path] = Success(layout.baseDir)
}

case class WorkspaceSource(workspaceId: WorkspaceId, path: Path) extends Source {
  def key: String = str"${path.value}"
  def glob: Glob = Glob.All
  def completion: String = str"${workspaceId}:$path"
  def hash(layer: Layer): Try[Digest] = Success((-1, path).digest[Md5])
  def repoIdentifier: RepoId = RepoId("local")
  def base(snapshots: Snapshots, layout: Layout): Try[Path] = Success(layout.baseDir)
}

