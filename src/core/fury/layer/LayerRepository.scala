package fury

import scala.util._

final class LayerRepository(revisions: LayerRevisions, current: Path) {

  def restorePrevious(): Try[Unit] =
    for {
      previous <- revisions.previous
      _        <- current.write(previous)
      _        <- revisions.discardPrevious()
    } yield Unit

  def update(layer: Layer): Try[Unit] = currentLayer match {
    case None => current.write(layer)
    case Some(currentLayer) =>
      for {
        _ <- revisions.store(currentLayer)
        _ <- current.write(layer)
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
    val revisions = new LayerRevisions(layout.historyDir, retainedRevisions)
    new LayerRepository(revisions, layout.furyConfig)
  }
}
