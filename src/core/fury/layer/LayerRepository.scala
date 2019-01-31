package fury.layer

import fury._
import fury.error.`package`.Outcome
import fury.io.Path

final class LayerRepository(revisions: LayerRevisions, current: Path) {

  def restorePrevious(): Outcome[Unit] =
    for {
      previous <- revisions.previous
      _        <- current.write(previous)
      _        <- revisions.discardNewerThan(previous.revision)
    } yield Unit

  def update(layer: Layer): Outcome[Unit] = currentLayer match {
    case None => current.write(layer)
    case Some(currentLayer) =>
      val updatedLayer = layer.copy(revision = currentLayer.revision + 1)
      for {
        _ <- revisions.store(currentLayer)
        _ <- current.write(updatedLayer)
      } yield Unit
  }

  private def currentLayer: Option[Layer] =
    if (current.exists()) current.read[Layer].toOption
    else None
}

object LayerRepository {
  // TODO make configurable
  private val retainedRevisions = 16

  def apply(layout: Layout): LayerRepository = {
    val revisionsDirectory = layout.historyDir.javaPath
    val revisions          = new LayerRevisions(revisionsDirectory, retainedRevisions)
    new LayerRepository(revisions, layout.furyConfig)
  }
}
