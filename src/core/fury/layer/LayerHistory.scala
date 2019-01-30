package fury.layer

import fury._
import fury.error.`package`.Outcome
import fury.io.Path

import scala.util.Success

final class LayerHistory(revisions: LayerRevisions, currentVersion: Path) {

  def undo(): Outcome[Unit] = currentLayer match {
    case None => Success(Unit)
    case Some(layer) =>
      for {
        previousLayer <- revisions.fetchPrevious(layer.revision)
        _             <- currentVersion.write(previousLayer)
      } yield Unit
  }

  def update(layer: Layer): Outcome[Unit] = {
    val updatedLayer = revisions.lastRevision match {
      case None               => layer
      case Some(lastRevision) => layer.copy(revision = lastRevision + 1)
    }

    for {
      _ <- revisions.store(updatedLayer)
      _ <- currentVersion.write(updatedLayer)
    } yield Unit
  }

  private def currentLayer: Option[Layer] =
    if (currentVersion.exists()) currentVersion.read[Layer].toOption
    else None
}

object LayerHistory {

  def apply(layout: Layout): LayerHistory =
    new LayerHistory(new LayerRevisions(), layout.furyConfig)
}
