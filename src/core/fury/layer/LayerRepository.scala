package fury.core

import fury.io._, fury.ogdl._

import scala.util._

final class LayerRepository(revisions: LayerRevisions, current: Path) {

  def restorePrevious(io: Io, layout: Layout): Try[Unit] =
    for {
      previous <- revisions.previous(io, layout)
      _        <- Ogdl.write(previous, current)
      _        <- revisions.discardPrevious()
    } yield Unit

  def update(io: Io, layer: Layer, layout: Layout): Try[Unit] = currentLayer(io, layout) match {
    case None => Ogdl.write(layer, current)
    case Some(currentLayer) =>
      for {
        _ <- revisions.store(currentLayer)
        _ <- Ogdl.write(layer, current)
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
    new LayerRepository(revisions, layout.layerFile)
  }
}
