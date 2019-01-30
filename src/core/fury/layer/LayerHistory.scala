package fury.layer

import fury._
import fury.error.`package`.Outcome
import fury.io.Path

final class LayerHistory(revisions: LayerRevisions, currentVersion: Path) {

  def restorePrevious(): Outcome[Unit] =
    for {
      previous <- revisions.lastRevision
      _        <- currentVersion.write(previous)
      _        <- revisions.discardNewerThan(previous.revision)
    } yield Unit

  def update(layer: Layer): Outcome[Unit] = currentLayer match {
    case None => currentVersion.write(layer)
    case Some(currentLayer) =>
      val updatedLayer = layer.copy(revision = currentLayer.revision + 1)
      for {
        _ <- revisions.store(currentLayer)
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
