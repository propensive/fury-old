package fury

import scala.util._

final class LayerRepository(revisions: LayerRevisions, current: Path) {

  def restorePrevious(io: Io, layout: Layout): Try[Unit] =
    for {
      previous <- revisions.previous(io, layout)
      _        <- current.write(previous)
      _        <- revisions.discardPrevious()
    } yield Unit

  def update(io: Io, layer: Layer, layout: Layout): Try[Unit] = currentLayer(io, layout) match {
    case None => current.write(layer)
    case Some(currentLayer) =>
      for {
        _ <- revisions.store(currentLayer)
        _ <- current.write(layer)
      } yield Unit
  }

  private def currentLayer(io: Io, layout: Layout): Option[Layer] =
    if (current.exists) Layer.read(io, current, layout).toOption
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
