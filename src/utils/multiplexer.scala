/*

    Fury, version 0.15.1. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.utils

/** a streaming multiplexer optimized for concurrent writes */
final class Multiplexer[K, V](keys: Set[K]) {
  private[this] var show: Boolean = false
  private[this] val state: Array[List[V]]  = Array.fill(keys.size)(Nil)
  private[this] val refs: Map[K, Int]      = keys.zipWithIndex.toMap
  private[this] val closed: Array[Boolean] = Array.fill(keys.size)(false)

  def finished: Boolean = closed.forall(identity)

  def start(): Unit = show = true

  def stream(interval: Int, tick: Option[V] = None): Iterator[V] = {
    def stream(lastSnapshot: List[List[V]]): Iterator[V] = {
      val t0       = System.currentTimeMillis
      val snapshot = state.to[List]
      // FIXME: This could be written more efficiently with a builder
      val changes = snapshot.zip(lastSnapshot).flatMap {
        case (current, last) =>
          current.take(current.length - last.length).reverse
      }
      if(finished && changes.isEmpty) {
        tick.to[Iterator]
      } else {
        val time = System.currentTimeMillis - t0
        if(time < interval) Thread.sleep(interval - time)
        changes.to[Iterator] ++ (if(show) tick.to[Iterator] else Iterator()) ++ stream(snapshot)
      }
    }
    stream(state.to[List])
  }

  def contains(key: K): Boolean = keys.contains(key)

  /** This method should only ever be called from one thread for any given reference, to
    *  guarantee safe concurrent access. */
  def fire(key: K, value: V): Unit = state(refs(key)) = value :: state(refs(key))

  /** This method should only ever be called from one thread for any given reference, to
    *  guarantee safe concurrent access. */
  def updateAll(value: V): Unit = keys.foreach(k => fire(k, value))

  /** This method should only ever be called from one thread for any given reference, to
    *  guarantee safe concurrent access. */
  def close(key: K): Unit = closed(refs(key)) = true

  def closeAll(): Unit = keys.foreach { k =>
    closed(refs(k)) = true
  }
}
