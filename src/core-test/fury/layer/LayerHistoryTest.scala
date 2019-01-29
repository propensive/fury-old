package fury.layer
import fury._
import probably._

object LayerHistoryTest extends TestApp {
  private var currentLayer: LayerRepository = _
  private var history: LayerHistory         = _

  override def tests(): Unit = {
    test("modifies current layer on update") {
      init()

      history.update(Layer())

      revisionOf(currentLayer)
    }.assert(revision => revision == 1)

    test("does not change current layer if history is empty") {
      init()
      currentLayer.store(Layer(revision = 10))

      history.undo()

      revisionOf(currentLayer)
    }.assert(revision => revision == 10)

    test("restores previous layer on undo") {
      init()

      history.update(Layer())
      history.update(Layer())
      history.undo()

      revisionOf(currentLayer)
    }.assert(revision => revision == 1)

    test("allows undoing more than one revision") {
      init()

      history.update(Layer())
      history.update(Layer())
      history.update(Layer())
      history.undo()
      history.undo()
      history.undo()

      revisionOf(currentLayer)
    }.assert(revision => revision == 0)
  }

  private def revisionOf(currentLayer: LayerRepository) = currentLayer.fetch().map(_.revision).get

  private def init() = {
    currentLayer = LayerRepository.inMemory()
    currentLayer.store(Layer())
    history = new LayerHistory(new LayerRevisions(), currentLayer)
  }
}
