package fury
import java.nio.file.Files

import probably._

object LayerRepositoryTest extends TestApp {
  private var currentLayer: Path               = _
  private var layerRepository: LayerRepository = _

  override def tests(): Unit = {
    test("modifies current layer on update") {
      init()

      layerRepository.update(Layer(version = 1))

      versionOf(currentLayer)
    }.assert(version => version == 1)

    test("does not change current layer if history is empty") {
      init()

      currentLayer.write(Layer(version = 1))
      layerRepository.restorePrevious()

      versionOf(currentLayer)
    }.assert(version => version == 1)

    test("restores previous layer on undo") {
      init()

      layerRepository.update(Layer(version = 1))
      layerRepository.update(Layer(version = 2))
      layerRepository.restorePrevious()

      versionOf(currentLayer)
    }.assert(version => version == 1)

    test("allows undoing more than one revision") {
      init()

      layerRepository.update(Layer(version = 1))
      layerRepository.update(Layer(version = 2))
      layerRepository.update(Layer(version = 3))
      layerRepository.restorePrevious()
      layerRepository.restorePrevious()

      versionOf(currentLayer)
    }.assert(version => version == 1)

    test("discard newer revisions when restoring") {
      init()

      layerRepository.update(Layer(version = 1))
      layerRepository.update(Layer(version = 2))
      layerRepository.restorePrevious()
      layerRepository.update(Layer(version = 3))
      layerRepository.restorePrevious()

      versionOf(currentLayer)
    }.assert(version => version == 1)

    test("cannot restore more revisions than are retained") {
      init(retainedRevisions = 1)

      layerRepository.update(Layer(version = 1))
      layerRepository.update(Layer(version = 2))
      layerRepository.update(Layer(version = 3))
      layerRepository.restorePrevious()
      layerRepository.restorePrevious()
      layerRepository.restorePrevious()

      versionOf(currentLayer)
    }.assert(version => version == 2)
  }

  private def versionOf(currentLayer: Path) = currentLayer.read[Layer].map(_.version).get

  private def init(retainedRevisions: Int = Int.MaxValue) = {
    val revisionsDir = Path(Files.createTempDirectory("layer-repo").toString)
    val revisions    = new LayerRevisions(revisionsDir, retainedRevisions)

    currentLayer = Path(Files.createTempFile("layer", "fury").toString)
    layerRepository = new LayerRepository(revisions, currentLayer)
  }
}
