package fury.layer

import fury._
import fury.error.`package`.Outcome

class LayerHistory(updateCurrentConfig: Layer => Outcome[Layer]) {
  def update(layer: Layer): Outcome[Layer] =
    for {
      _ <- updateCurrentConfig(layer)
    } yield layer
}

object LayerHistory {
  def apply(layout: Layout): LayerHistory = {
    val furyConfig = layout.furyConfig
    new LayerHistory(furyConfig.write[Layer](_))
  }
}
