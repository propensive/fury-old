/*

    Fury, version 0.33.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

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

  implicit val msgShow: MsgShow[Source] = v => Message { theme =>
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
    case r"ws:$repo@([a-z][a-z0-9\.\-]*[a-z0-9]):$dir@([^\*\{\}\[\]]*)" =>
      WorkspaceSource(WorkspaceId(repo), Path(dir))
    case r"$repo@([a-z][a-z0-9\.\-]*[a-z0-9]):$dir@([^\*\{\}\[\]]*)//$pattern@(.*)" =>
      RepoSource(RepoId(repo), Path(dir), Glob(pattern))
    case r"$repo@([a-z][a-z0-9\.\-]*[a-z0-9]):$dir@([^\*\{\}\[\]]*)" =>
      RepoSource(RepoId(repo), Path(dir), Glob.All)
    case r"$dir@([^\*\{\}\[\]]*)//$pattern@(.*)" =>
      LocalSource(Path(dir), Glob(pattern))
    case r"$dir@([^\*\{\}\[\]]*)" =>
      LocalSource(Path(dir), Glob.All)
  }

  def fromInclude(include: Include): Set[Source] = include.kind match {
    case Jarfile(dependency)      => Set()
    case JsFile(dependency)       => Set()
    case TarFile(workspace, path) => Set(WorkspaceSource(workspace, path))
    case TgzFile(workspace, path) => Set(WorkspaceSource(workspace, path))
    case ClassesDir(dependency)   => Set()
    case FileRef(rootId, path)    => rootId match {
      case id: RepoId                => Set(RepoSource(id, path, Glob.All))
      case id: WorkspaceId           => Set(WorkspaceSource(id, path))
    }
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
  def editable: Boolean
  def completion: String
  def hash(layer: Layer): Try[Digest]
  def path: Path
  def glob: Glob
  def rootId: RootId

  //def files(layer: Layer, snapshot: Snapshot, layout: Layout): Try[Stream[Path]] =
  //  dir(layer, snapshot, layout).map { dir => glob(dir, dir.walkTree) }
  
  /*def copyTo(layer: Layer, snapshot: Snapshot, layout: Layout, destination: Path)(implicit log: Log): Try[Unit] = for {
    baseDir  <- dir(layer, snapshot, layout)
    allFiles <- files(layer, snapshot, layout)
    _        <- allFiles.to[List].map { f =>
                  f.relativizeTo(baseDir).in(destination).mkParents().map(f.copyTo(_))
                }.sequence
  } yield ()*/

  //def fileCount(layer: Layer, snapshot: Snapshot, layout: Layout): Try[Int] =
  //  files(layer, snapshot, layout).map(_.length)

  //def totalSize(layer: Layer, snapshot: Snapshot, layout: Layout): Try[ByteSize] =
  //  files(layer, snapshot, layout).map(_.map(_.size).reduce(_ + _))

  //def linesOfCode(layer: Layer, snapshot: Snapshot, layout: Layout): Try[Int] = for {
  //  pathStream <- files(layer, snapshot, layout)
  //  lines      <- pathStream.traverse(_.lines)
  //} yield lines.map(_.size).sum
}

case class RepoSource(repoId: RepoId, path: Path, glob: Glob) extends Source {
  def editable = false
  def rootId: RootId = repoId
  def key: String = str"${repoId}:${path.value}//$glob"
  def completion: String = str"${repoId}:${path.value}"
  def hash(layer: Layer): Try[Digest] = layer.repos.findBy(repoId).map((path, _).digest[Md5])
}

case class LocalSource(path: Path, glob: Glob) extends Source {
  def editable = true
  def rootId: RootId = RepoId("local")
  def key: String = str"$path//$glob"
  def completion: String = path.value
  def hash(layer: Layer): Try[Digest] = Success((-1, path).digest[Md5])
}

case class WorkspaceSource(workspaceId: WorkspaceId, path: Path) extends Source {
  def editable = false
  def rootId: RootId = workspaceId
  def key: String = str"ws:$workspaceId:${path.value}"
  def glob: Glob = Glob.All
  def completion: String = str"${workspaceId}:$path"
  def hash(layer: Layer): Try[Digest] = Success((-1, path).digest[Md5])
}
