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

import fury.ogdl._, fury.io._, fury.model._, fury.strings._

import gastronomy._
import kaleidoscope._
import mercator._

import scala.util._

import language.higherKinds

object Source {
  implicit val stringShow: StringShow[Source] = _.description
  implicit val ogdlReader: OgdlReader[Source] = src => unapply(src()).get // FIXME
  implicit val ogdlWriter: OgdlWriter[Source] = src => Ogdl(src.description)

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
}

trait Source {
  def description: String
  def hash(schema: Schema, layout: Layout): Try[Digest]
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
    _        <- allFiles.to[List].traverse { f =>
                  f.relativizeTo(baseDir).in(destination).mkParents().map(f.copyTo(_))
                }
  } yield ()
}

case class ExternalSource(repoId: RepoId, dir: Path, glob: Glob) extends Source {
  def description: String = str"${repoId}:${dir.value}//$glob"
  def repoIdentifier: RepoId = repoId
  def hash(schema: Schema, layout: Layout): Try[Digest] = schema.repo(repoId, layout).map((dir, _).digest[Md5])
  
  def base(checkouts: Checkouts, layout: Layout): Try[Path] =
    checkouts(repoId).map { checkout => checkout.local.getOrElse(checkout.path) }
}

case class SharedSource(dir: Path, glob: Glob) extends Source {
  def description: String = str"shared:${dir}//$glob"
  def hash(schema: Schema, layout: Layout): Try[Digest] = Success((-2, dir).digest[Md5])
  def repoIdentifier: RepoId = RepoId("shared")
  def base(checkouts: Checkouts, layout: Layout): Try[Path] = Success(layout.sharedDir)
}

case class LocalSource(dir: Path, glob: Glob) extends Source {
  def description: String = str"${dir.value}//$glob"
  def hash(schema: Schema, layout: Layout): Try[Digest] = Success((-1, dir).digest[Md5])
  def repoIdentifier: RepoId = RepoId("local")
  def base(checkouts: Checkouts, layout: Layout): Try[Path] = Success(layout.baseDir)
}

