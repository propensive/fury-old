package fury.core

import fury.ogdl._, fury.io._, fury.model._, fury.strings._

import gastronomy._
import kaleidoscope._

import scala.util._

object Source {
  implicit val stringShow: StringShow[Source] = _.description
  implicit val ogdlReader: OgdlReader[Source] = src => unapply(src()).get // FIXME
  implicit val ogdlWriter: OgdlWriter[Source] = src => Ogdl(src.description)

  implicit val sourceDiff: Diff[Source] =
    (l, r) => if(l == r) Nil else List(Difference(msg"source", msg"", msg"$l", msg"$r"))

  implicit val msgShow: MsgShow[Source] = v => UserMsg { theme =>
    v match {
      case ExternalSource(repoId, path) =>
        msg"${theme.repo(repoId.key)}${theme.gray(":")}${theme.path(path.value)}".string(theme)
      case SharedSource(path) =>
        msg"${theme.repo("shared")}${theme.gray(":")}${theme.path(path.value)}".string(theme)
      case LocalSource(path) =>
        msg"${theme.path(path.value)}".string(theme)
    }
  }

  def unapply(string: String): Option[Source] = string match {
    case r"shared:$path@(.*)" =>
      Some(SharedSource(Path(path)))
    case r"$repo@([a-z][a-z0-9\.\-]*[a-z0-9]):$path@(.*)" =>
      Some(ExternalSource(RepoId(repo), Path(path)))
    case r"$path@(.*)" =>
      Some(LocalSource(Path(path)))
    case _ =>
      None
  }

  def repoId(src: Source): Option[RepoId] = src match {
    case ExternalSource(repoId, _) => Some(repoId)
    case _                         => None
  }
}

trait Source {
  def description: String
  def hash(schema: Schema, layout: Layout): Try[Digest]
  def path: Path
  def repoIdentifier: RepoId
}

case class ExternalSource(repoId: RepoId, path: Path) extends Source {
    def description: String = str"${repoId}:${path.value}"
    def repoIdentifier: RepoId = repoId
    
    def hash(schema: Schema, layout: Layout): Try[Digest] =
      schema.repo(repoId, layout).map((path, _).digest[Md5])
  }
  
  case class SharedSource(path: Path) extends Source {
    def description: String = str"shared:${path.value}"
    def hash(schema: Schema, layout: Layout): Try[Digest] = Success((-2, path).digest[Md5])
    def repoIdentifier: RepoId = RepoId("-")
  }
  
  case class LocalSource(path: Path) extends Source {
    def description: String = str"${path.value}"
    def hash(schema: Schema, layout: Layout): Try[Digest] = Success((-1, path).digest[Md5])
    def repoIdentifier: RepoId = RepoId("-")
  }
  