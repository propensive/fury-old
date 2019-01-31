package fury.layer

import fury._
import fury.error._
import fury.io._
import kaleidoscope._

import scala.annotation.tailrec
import scala.util._

final class LayerRevisions(directory: Path, retained: Int) {

  def store(layer: Layer): Outcome[Unit] = {
    val revision = previousRevision match {
      case None           => 0
      case Some(previous) => previous.revision + 1
    }

    val path = directory / s"$revision.bak"

    for {
      _ <- path.write(layer)
      _ <- discardStaleRevisions()
    } yield Unit
  }

  private def discardStaleRevisions(): Outcome[Unit] = {
    @tailrec
    def discard(revisions: Seq[LayerRevision]): Outcome[Unit] = revisions match {
      case Nil => Success(Unit)
      case revision :: remaining =>
        revision.discard match {
          case Success(_) => discard(remaining)
          case failure    => failure
        }
    }

    val staleRevisions = revisions.drop(retained)
    discard(staleRevisions)
  }

  def discardPrevious(): Outcome[Unit] = previousRevision match {
    case None           => Success(Unit)
    case Some(previous) => previous.discard
  }

  def previous: Outcome[Layer] = previousRevision match {
    case None           => Failure(NoPreviousRevision)
    case Some(previous) => previous.layer
  }

  private def revisions: Seq[LayerRevision] = {
    def parseRevision(path: String) = path match {
      case r"""${rev: String}@(\d+).bak""" => Some(rev.toLong)
      case _                               => None
    }

    val revisions = for {
      file <- directory.children
      rev  <- parseRevision(file)
    } yield new LayerRevision(rev, directory / file)

    revisions.sortWith(_.revision > _.revision)
  }

  private def previousRevision = revisions.headOption

  private class LayerRevision(val revision: Long, path: Path) {
    def layer: Outcome[Layer]  = path.read[Layer]
    def discard: Outcome[Unit] = path.delete()
  }
}

case object NoPreviousRevision extends FuryException
