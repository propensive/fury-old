package fury.layer

import java.util.Objects

import fury._
import fury.error._

import scala.util.Success

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
