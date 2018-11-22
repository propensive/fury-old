package fury

import scala.collection.immutable.Stream

/** a streaming multiplexer optimized for concurrent writes */
final class Multiplexer[K, V](keys: List[K]) {
  private[this] val state: Array[List[V]] = Array.fill(keys.size)(Nil)
  private[this] val refs: Map[K, Int] = keys.zipWithIndex.toMap
  private[this] var lastSnapshot: List[List[V]] = state.to[List]
  private[this] val closed: Array[Boolean] = Array.fill(keys.size)(false)
  
  private[this] def finished: Boolean = closed.forall(identity)


  // FIXME: See if it is possible to write this as a tail-recursive method, without the
  // `lastSnapshot` var.
  def stream(interval: Int, tick: Option[V] = None): Stream[V] = {
    val t0 = System.currentTimeMillis
    val snapshot = state.clone().to[List]
    // FIXME: This could be written more efficiently with a builder
    val changes = snapshot.zip(lastSnapshot).flatMap { case (a, b) =>
      a.take(a.length - b.length).reverse
    }
    lastSnapshot = snapshot
    
    val time = System.currentTimeMillis - t0
    if(changes.size > 0) {
      if(time < interval) Thread.sleep(interval - time)
      changes.to[Stream] #::: tick.to[Stream] #::: stream(interval, tick)
    } else if(finished) tick.to[Stream]
    else {
      if(time < interval) Thread.sleep(interval - time)
      tick.to[Stream] #::: stream(interval, tick)
    }
  }

  /** This method should only ever be called from one thread for any given reference, to
   *  guarantee safe concurrent access. */
  def update(key: K, value: V): Unit = state(refs(key)) = value :: state(refs(key))

  /** This method should only ever be called from one thread for any given reference, to
   *  guarantee safe concurrent access. */
  def close(key: K): Unit = closed(refs(key)) = true
}
