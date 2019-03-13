package fury

import scala.collection.mutable._

abstract class Pool[K, T]() {

  private[this] val pool: Map[K, Set[T]] = new HashMap().withDefaultValue(Set())

  def borrow[S](io: Io, key: K)(action: T => S): S = {
    val element = synchronized {
      pool(key).headOption.fold(create(key)) { e =>
        pool(key) = pool(key) - e
        e
      }
    }

    val result: S = action(element)
    synchronized { pool(key) = pool(key) + element }
    result
  }

  def create(key: K): T
}
