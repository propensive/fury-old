package fury.layer

import java.nio.file.{Files, Path}

import fury._
import fury.error._

import scala.util.{Failure, Success, Try}
import kaleidoscope._

import collection.JavaConverters._

final class LayerRevisions(directory: Path) {

  def store(layer: Layer): Outcome[Unit] = {
    val revision = layer.revision
    val path     = fury.io.Path(directory.resolve(revision + ".bak").toString)
    path.write(layer)
  }

  def discardNewerThan(revision: Long): Outcome[Unit] = Try(
      for {
        rev  <- revisions.filter(_.revision >= revision)
        path = rev.path
      } Files.delete(path)
  )

  def lastRevision: Outcome[Layer] = {
    val sortedRevisions = revisions.sortWith(_.revision > _.revision)
    sortedRevisions match {
      case Nil     => Failure(NoPreviousRevision)
      case hd :: _ => hd.layer
    }
  }

  private def revisions: Seq[LayerRevision] = {
    def parseRevision(path: Path): Option[Long] = path.getFileName.toString match {
      case r"""${rev: String}@(\d+).bak""" => Some(rev.toLong)
      case _                               => None
    }

    for {
      file <- Files.list(directory).iterator().asScala.toList
      rev  <- parseRevision(file)
    } yield LayerRevision(rev, file)
  }

  private case class LayerRevision(revision: Long, path: Path) {
    def layer: Outcome[Layer] = fury.io.Path(path.toString).read[Layer]
  }
}

case object NoPreviousRevision extends FuryException
