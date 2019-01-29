package fury.layer

import fury._
import fury.error.`package`.Outcome

final class LayerHistory(previousVersion: LayerRepository, currentVersion: LayerRepository) {
  def undo(): Outcome[Unit] = previousVersion.fetch().flatMap(currentVersion.store)

  def update(layer: Layer): Outcome[Layer] =
    for {
      _ <- storeCurrentLayer()
    _ <- currentVersion.store(layer)
    } yield layer

  private def storeCurrentLayer() = currentVersion.fetch().flatMap(previousVersion.store)
}

object LayerHistory {
  def apply(layout: Layout): LayerHistory = {
    new LayerHistory(LayerRepository.inMemory(), LayerRepository.inMemory())
  }
}
