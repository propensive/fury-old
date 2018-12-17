/*
  Fury, version 0.1.2. Copyright 2018 Jon Pretty, Propensive Ltd.

  The primary distribution site is: https://propensive.com/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required  by applicable  law or  agreed to  in writing,  software  distributed  under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
  express  or  implied.  See  the  License for  the specific  language  governing  permissions and
  limitations under the License.
                                                                                                  */
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
