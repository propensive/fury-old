package fury.layer

import java.nio.file.{Files, Path}

import fury._
import fury.error._

import scala.util.Failure
import kaleidoscope._

import collection.JavaConverters._

final class LayerRevisions(directory: Path) {

  def store(layer: Layer): Outcome[Unit] = {
    val revision = layer.revision
    val path     = fury.io.Path(directory.resolve(revision + ".bak").toString)
    path.write(layer)
  }

  def fetchPrevious(revision: Long): Outcome[Layer] =
    revisions
      .filter(_.revision < revision)
      .sortWith(_.revision < _.revision)
      .lastOption
      .map(_.layer)
      .getOrElse(Failure(NoPreviousRevision))

  def lastRevision: Option[Long] = revisions match {
    case Nil => None
    case revs =>
      Debug.debug(s"last revision: ${revs.map(_.revision).max}")
      Some(revs.map(_.revision).max)
  }

  private def revisions: Seq[LayerRevision] = {
    def parseRevision(path: Path): Option[Long] = path.getFileName.toString match {
      case r"""${rev: String}@(\d+).bak""" => Some(rev.toLong)
      case _                               => None
    }

    for {
      file <- Files.list(directory).iterator().asScala.toList
      rev  <- parseRevision(file)
    } yield new LayerRevision(rev, file)
  }

  private class LayerRevision(val revision: Long, path: Path) {
    def layer: Outcome[Layer] = fury.io.Path(path.toString).read[Layer]
  }
}

case object NoPreviousRevision extends FuryException