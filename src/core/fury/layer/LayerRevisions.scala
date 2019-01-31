package fury.layer

import fury._
import fury.error._
import fury.io._
import kaleidoscope._

import scala.util.{Failure, Try}

final class LayerRevisions(directory: Path, retained: Int) {

  def store(layer: Layer): Outcome[Unit] = {
    val revision = layer.revision
    val path     = directory / s"$revision.bak"

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
      revisions.foreach(_.path.delete())
  )

  private def revisions: Seq[LayerRevision] = {
    def parseRevision(path: String) = path match {
      case r"""${rev: String}@(\d+).bak""" => Some(rev.toLong)
      case _                               => None
    }

    val revisions = for {
      file <- directory.children
      rev  <- parseRevision(file)
    } yield LayerRevision(rev, directory / file)

    revisions.sortWith(_.revision > _.revision)
  }

  private case class LayerRevision(revision: Long, path: Path) {
    def layer: Outcome[Layer] = path.read[Layer]
  }
}

case object NoPreviousRevision extends FuryException
