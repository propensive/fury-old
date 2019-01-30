package fury.layer

import fury._
import fury.error.`package`.Outcome
import fury.io.Path

final class LayerHistory(revisions: LayerRevisions, currentVersion: Path) {

  def restorePrevious(): Outcome[Unit] = {
    val currentRevision: Long = currentLayer.map(_.revision).getOrElse(0)

    for {
      previousLayer <- revisions.fetchPrevious(currentRevision)
      _             <- currentVersion.write(previousLayer)
      _             <- revisions.discardNewerThan(previousLayer.revision)
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
    new LayerHistory(new LayerRevisions(layout.historyDir.javaPath), layout.furyConfig)
}
