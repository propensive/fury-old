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

import fury.ogdl._, fury.io._, fury.model._, fury.strings._

import gastronomy._
import kaleidoscope._
import mercator._

import scala.util._

object Source {
  implicit val stringShow: StringShow[Source] = _.key
  implicit val ogdlReader: OgdlReader[Source] = src => unapply(src.id()).get // FIXME
  implicit val ogdlWriter: OgdlWriter[Source] = src => Ogdl(Vector("id" -> Ogdl(src.key)))
  implicit val parser: Parser[Source] = unapply(_)
  implicit val index: Index[Source] = FieldIndex("id")
  implicit val keyName: KeyName[Source] = () => msg"source"

  implicit val sourceDiff: Diff[Source] =
    (l, r) => if(l == r) Nil else List(Difference(msg"source", msg"", msg"$l", msg"$r"))

  implicit val msgShow: MsgShow[Source] = v => UserMsg { theme =>
    v match {
      case ExternalSource(repoId, dir, glob) =>
        msg"$repoId${':'}$dir${'/'}${'/'}$glob".string(theme)
      case SharedSource(dir, glob) =>
        msg"${RepoId("shared")}${':'}$dir${'/'}${'/'}$glob".string(theme)
      case LocalSource(dir, glob) =>
        msg"$dir${'/'}${'/'}$glob".string(theme)
    }
  }

  def unapply(string: String): Option[Source] = string.only {
    case r"shared:$dir@([^\*\{\}\[\]]*)//$pattern@(.*)" =>
      SharedSource(Path(dir), Glob(pattern))
    case r"shared:$dir@([^\*\{\}\[\]]*)" =>
      SharedSource(Path(dir), Glob.All)
    case r"$repo@([a-z][a-z0-9\.\-]*[a-z0-9]):$dir@([^\*\{\}\[\]]*)//$pattern@(.*)" =>
      ExternalSource(RepoId(repo), Path(dir), Glob(pattern))
    case r"$repo@([a-z][a-z0-9\.\-]*[a-z0-9]):$dir@([^\*\{\}\[\]]*)" =>
      ExternalSource(RepoId(repo), Path(dir), Glob.All)
    case r"$dir@([^\*\{\}\[\]]*)//$pattern@(.*)" =>
      LocalSource(Path(dir), Glob(pattern))
    case r"$dir@([^\*\{\}\[\]]*)" =>
      LocalSource(Path(dir), Glob.All)
  }

  def repoId(src: Source): Option[RepoId] = src.only { case ExternalSource(repoId, _, _) => repoId }

  def rewriteLocal(source: Source, localId: Option[RepoId]): Source =
    localId.fold(source) { repoId => source match {
      case LocalSource(dir, glob) => ExternalSource(repoId, dir, glob)
      case source => source
    } }
}

sealed abstract class Source extends Key(msg"source") {
  def key: String
  def completion: String
  def hash(layer: Layer, layout: Layout): Try[Digest]
  def dir: Path
  def glob: Glob
  def repoIdentifier: RepoId

  def base(checkouts: Checkouts, layout: Layout): Try[Path]
  def dir(checkouts: Checkouts, layout: Layout): Try[Path] = base(checkouts, layout).map(dir in _)
  
  def files(checkouts: Checkouts, layout: Layout): Try[Stream[Path]] =
    dir(checkouts, layout).map { dir => glob(dir, dir.walkTree) }
  
  def copyTo(checkouts: Checkouts, layout: Layout, destination: Path)(implicit log: Log): Try[Unit] = for {
    baseDir  <- dir(checkouts, layout)
    allFiles <- files(checkouts, layout)
    _        <- allFiles.to[List].map { f =>
                  f.relativizeTo(baseDir).in(destination).mkParents().map(f.copyTo(_))
                }.sequence
  } yield ()

  def fileCount(checkouts: Checkouts, layout: Layout): Try[Int] = files(checkouts, layout).map(_.length)

  def totalSize(checkouts: Checkouts, layout: Layout): Try[ByteSize] =
    files(checkouts, layout).map(_.map(_.size).reduce(_ + _))

  def linesOfCode(checkouts: Checkouts, layout: Layout): Try[Int] = for {
    pathStream <- files(checkouts, layout)
    lines      <- pathStream.traverse(_.lines)
  } yield lines.map(_.size).sum
}

case class ExternalSource(repoId: RepoId, dir: Path, glob: Glob) extends Source {
  def key: String = str"${repoId}:${dir.value}//$glob"
  def completion: String = str"${repoId}:${dir.value}"
  def repoIdentifier: RepoId = repoId
  def hash(layer: Layer, layout: Layout): Try[Digest] = layer.repo(repoId, layout).map((dir, _).digest[Md5])
  
  def base(checkouts: Checkouts, layout: Layout): Try[Path] =
    checkouts(repoId).map { checkout => checkout.local.fold(checkout.path)(_.dir) }
}

case class SharedSource(dir: Path, glob: Glob) extends Source {
  def key: String = str"shared:${dir}//$glob"
  def completion: String = key
  def hash(layer: Layer, layout: Layout): Try[Digest] = Success((-2, dir).digest[Md5])
  def repoIdentifier: RepoId = RepoId("shared")
  def base(checkouts: Checkouts, layout: Layout): Try[Path] = Success(layout.sharedDir)
}

case class LocalSource(dir: Path, glob: Glob) extends Source {
  def key: String = str"${dir.value}//$glob"
  def completion: String = dir.value
  def hash(layer: Layer, layout: Layout): Try[Digest] = Success((-1, dir).digest[Md5])
  def repoIdentifier: RepoId = RepoId("local")
  def base(checkouts: Checkouts, layout: Layout): Try[Path] = Success(layout.baseDir)
}

