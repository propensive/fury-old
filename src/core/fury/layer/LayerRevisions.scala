package fury.layer

import java.nio.file.{Files, Path}

import fury._
import fury.error._
import kaleidoscope._

import scala.collection.JavaConverters._
import scala.util.{Failure, Try}

final class LayerRevisions(directory: Path, retained: Int) {

  def store(layer: Layer): Outcome[Unit] = {
    val revision = layer.revision
    val path     = fury.io.Path(directory.resolve(revision + ".bak").toString)

    for {
      _ <- path.write(layer)
      _ <- discardStaleRevisions()
    } yield Unit
  }

  def discardNewerThan(revision: Long): Outcome[Unit] = {
    val invalidRevisions = revisions.filter(newerThan(revision))
    discardRevisions(invalidRevisions)
  }

  def previous: Outcome[Layer] = revisions match {
    case Nil     => Failure(NoPreviousRevision)
    case hd :: _ => hd.layer
  }

  private def discardStaleRevisions(): Outcome[Unit] = {
    val staleRevisions = revisions.drop(retained)
    discardRevisions(staleRevisions)
  }

  private def newerThan(revision: Long) = (rev: LayerRevision) => rev.revision >= revision

  private def discardRevisions(revisions: Seq[LayerRevision]) = Try(
      for {
        rev  <- revisions
        path = rev.path
      } Files.delete(path)
  )

  private def revisions: Seq[LayerRevision] = {
    def parseRevision(path: Path) = path.getFileName.toString match {
      case r"""${rev: String}@(\d+).bak""" => Some(rev.toLong)
      case _                               => None
    }

    val revisions = for {
      file <- Files.list(directory).iterator().asScala.toList
      rev  <- parseRevision(file)
    } yield LayerRevision(rev, file)

    revisions.sortWith(_.revision > _.revision)
  }

  private case class LayerRevision(revision: Long, path: Path) {
    def layer: Outcome[Layer] = fury.io.Path(path.toString).read[Layer]
  }
}

case object NoPreviousRevision extends FuryException
