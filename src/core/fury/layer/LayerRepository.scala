package fury.layer

import java.nio.file.{Files, Path}
import java.util.Objects

import fury._
import fury.error._

import scala.util.{Failure, Success}

trait LayerRepository {
  def store(layer: Layer): Outcome[Unit]
  def fetch(): Outcome[Layer]
}

object LayerRepository {
  def inMemory() = new InMemoryLayerRepository
}

final class InMemoryLayerRepository extends LayerRepository {
  private var layer: Layer = _

  override def store(layer: Layer): Outcome[Unit] = Success(this.layer = layer)
  override def fetch(): Outcome[Layer]            = Success(layer).filter(Objects.nonNull)
}

final class LayerRevisions(directory: Path = Files.createTempDirectory("layer-repo")) {
  import kaleidoscope._

  import collection.JavaConverters._

  def store(layer: Layer): Outcome[Unit] = {
    val revision = layer.revision
    val path     = fury.io.Path(directory.resolve(revision + ".bak").toString)
    path.write(layer)
  }

  def fetchPrevious(revision: Long): Outcome[Layer] =
    revisions
      .filter(_.revision < revision)
      .sortWith(_.revision < _.revision)
      .lastOption
      .map(_.layer)
      .getOrElse(Failure(new NoSuchElementException("Repository is empty")))

  private def revisions: Seq[LayerRevision] = {
    def parseRevision(path: Path): Option[Long] = path.getFileName.toString match {
      case r"""${rev: String}@(\d+).bak""" => Some(rev.toLong)
      case _                               => None
    }

    for {
      file <- Files.list(directory).iterator().asScala.toList
      rev  <- parseRevision(file)
    } yield new LayerRevision(rev, file)
  }

  private class LayerRevision(val revision: Long, path: Path) {
    def layer: Outcome[Layer] = fury.io.Path(path.toString).read[Layer]
  }

}
