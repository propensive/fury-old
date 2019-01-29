package fury.layer

import fury._
import fury.error.`package`.Outcome

final class LayerHistory(previousRevisions: LayerRevisions, currentVersion: LayerRepository) {

  def undo(): Outcome[Unit] =
    for {
      currentLayer    <- currentVersion.fetch()
      currentRevision = currentLayer.revision
      previousLayer   <- previousRevisions.fetchPrevious(currentRevision)
      _               <- currentVersion.store(previousLayer)
    } yield Unit

  def update(layer: Layer): Outcome[Unit] =
    for {
      currentLayer    <- currentVersion.fetch()
      _               <- previousRevisions.store(currentLayer)
      currentRevision = currentLayer.revision
      updatedLayer    = layer.copy(revision = currentRevision + 1)
      _               <- currentVersion.store(updatedLayer)
    } yield Unit

}

object LayerHistory {

  def apply(layout: Layout): LayerHistory =
    new LayerHistory(new LayerRevisions(), LayerRepository.inMemory())
}
