package fury.layer
import fury._
import probably._

object LayerHistoryTest extends TestApp {
  private val initialLayer = Layer(version = 0)

  private var currentLayer: LayerRepository = _
  private var history: LayerHistory         = _

  override def tests(): Unit = {
    test("modifies current layer on update") {
      init()

      history.update(Layer(version = 1))

      currentLayer.fetch()
    }.assert(_.get == Layer(version = 1))

    test("does not change current layer if history is empty") {
      init()

      history.undo()

      currentLayer.fetch()
    }.assert(_.get == initialLayer)

    test("restores previous layer on undo") {
      init()

      history.update(Layer(version = 1))
      history.update(Layer(version = 2))
      history.undo()

      currentLayer.fetch()
    }.assert(_.get == Layer(version = 1))
  }

  private def init(): Unit = {
    currentLayer = LayerRepository.inMemory()
    currentLayer.store(initialLayer)
    history = new LayerHistory(LayerRepository.inMemory(), currentLayer)
  }
}
