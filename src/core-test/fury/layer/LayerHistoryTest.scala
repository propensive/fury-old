package fury.layer
import java.nio.file.Files

import fury._
import fury.io.Path
import probably._

object LayerHistoryTest extends TestApp {
  private var currentLayer: Path               = _
  private var layerRepository: LayerRepository = _

  override def tests(): Unit = {
    test("modifies current layer on update") {
      init()

      layerRepository.update(Layer())

      revisionOf(currentLayer)
    }.assert(revision => revision == 1)

    test("does not change current layer if history is empty") {
      init()
      currentLayer.write(Layer(revision = 10))

      layerRepository.restorePrevious()

      revisionOf(currentLayer)
    }.assert(revision => revision == 10)

    test("restores previous layer on undo") {
      init()

      layerRepository.update(Layer())
      layerRepository.update(Layer())
      layerRepository.restorePrevious()

      revisionOf(currentLayer)
    }.assert(revision => revision == 1)

    test("allows undoing more than one revision") {
      init()

      layerRepository.update(Layer())
      layerRepository.update(Layer())
      layerRepository.update(Layer())
      layerRepository.restorePrevious()
      layerRepository.restorePrevious()
      layerRepository.restorePrevious()

      revisionOf(currentLayer)
    }.assert(revision => revision == 0)

    test("discard newer revisions when restoring") {
      init()

      layerRepository.update(Layer(revision = 1))
      layerRepository.update(Layer(revision = 2))
      layerRepository.restorePrevious()
      layerRepository.update(Layer(revision = 3))
      layerRepository.restorePrevious()

      revisionOf(currentLayer)
    }.assert(revision => revision == 1)

    test("revisions are monotonically increasing") {
      init()

      layerRepository.update(Layer())
      layerRepository.restorePrevious()
      layerRepository.update(Layer())

      revisionOf(currentLayer)
    }.assert(revision => revision == 1)
  }

  private def revisionOf(currentLayer: Path) = currentLayer.read[Layer].map(_.revision).get

  private def init() = {
    val revisions = new LayerRevisions(Files.createTempDirectory("layer-repo"))
    currentLayer = Path(Files.createTempFile("layer", "fury").toString)
    layerRepository = new LayerRepository(revisions, currentLayer)
  }
}
