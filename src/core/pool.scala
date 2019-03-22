/*
  Fury, version 0.4.0. Copyright 2018-19 Jon Pretty, Propensive Ltd.

  The primary distribution site is: https://propensive.com/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required  by applicable  law or  agreed to  in writing,  software  distributed  under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
  express  or  implied.  See  the  License for  the specific  language  governing  permissions and
  limitations under the License.
 */
package fury.core

import scala.collection.mutable._
import scala.concurrent._, duration._
import scala.util._
import scala.annotation._

abstract class Pool[K, T](max: Int, timeout: Long)(implicit ec: ExecutionContext) {

  def create(key: K): T
  def destroy(value: T): Unit

  object Entry { implicit val ord: Ordering[Entry] = Ordering[Long].on(_.timestamp) }
  case class Entry(timestamp: Long, key: K, value: T)

  private[this] var count: Int = 0
  private[this] var pool: SortedSet[Entry] = TreeSet()

  @tailrec
  private[this] def createOrRecycle(key: K): T = {
    val result = pool.find(_.key == key) match {
      case None =>
        if(count >= max) {
          pool.headOption match {
            case None =>
              Thread.sleep(timeout)
              None
            case Some(entry) =>
              destroy(entry.value)
              count -= 1
              None
          }
        } else Try(Await.result(Future(blocking(create(key))), timeout.milliseconds)).map { value =>
          count += 1
          pool += Entry(System.currentTimeMillis, key, value)
          Some(value)
        }.toOption.getOrElse(None)
      case Some(entry) =>
        pool -= entry
        Some(entry.value)
    }
    if(result.isEmpty) createOrRecycle(key) else result.get
  }

  def borrow[S](io: Io, key: K)(action: T => S): S = {
    val value: T = synchronized {
      pool.headOption.fold(createOrRecycle(key)) { e =>
        pool -= e
        e.value
      }
    }

    val result: S = action(value)
    synchronized { pool += Entry(System.currentTimeMillis, key, value) }
    result
  }
}
