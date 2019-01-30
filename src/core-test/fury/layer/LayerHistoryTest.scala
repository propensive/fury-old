package fury.layer
import java.nio.file.Files

import fury._
import fury.io.Path
import probably._

object LayerHistoryTest extends TestApp {
  private var currentLayer: Path    = _
  private var history: LayerHistory = _

  override def tests(): Unit = {
    test("modifies current layer on update") {
      init()

      history.update(Layer())

      revisionOf(currentLayer)
    }.assert(revision => revision == 0)

    test("does not change current layer if history is empty") {
      init()
      currentLayer.write(Layer(revision = 10))

      history.undo()

      revisionOf(currentLayer)
    }.assert(revision => revision == 10)

    test("restores previous layer on undo") {
      init()

      history.update(Layer())
      history.update(Layer())
      history.undo()

      revisionOf(currentLayer)
    }.assert(revision => revision == 0)

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

    test("revisions are monotonically increasing") {
      init()

      history.update(Layer())
      history.undo()
      history.update(Layer())

      revisionOf(currentLayer)
    }.assert(revision => revision == 1)
  }

  private def revisionOf(currentLayer: Path) = currentLayer.read[Layer].map(_.revision).get

  private def init() = {
    currentLayer = Path(Files.createTempFile("layer", "fury").toString)
    history = new LayerHistory(new LayerRevisions(), currentLayer)
  }
}
