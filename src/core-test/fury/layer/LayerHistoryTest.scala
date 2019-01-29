package fury.layer

import fury._
import probably._

import scala.util.Success

object LayerHistoryTest extends TestApp {
  private var currentLayer: Layer = _

  private def update(layer: Layer) = {
    currentLayer = layer
    Success(currentLayer)
  }

  override def tests(): Unit = {
    test("modifies current layer on update") {
      val history = newHistory()

      history.update(Layer(version = 1))

      currentLayer
    }.assert(_ == Layer(version = 1))
  }

  private def newHistory() = new LayerHistory(update)
}
