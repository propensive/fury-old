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

abstract class Pool[K, T](timeout: Long)(implicit ec: ExecutionContext) {

  def create(key: K): T
  def destroy(value: T): Unit
  
  def isBad(value: T): Boolean

  case class Entry(key: K, value: T)

  private[this] val pool: Map[K, T] = scala.collection.concurrent.TrieMap()
  
  def size: Int = pool.size

  @tailrec
  private[this] def createOrRecycle(key: K): T = {
    val result = pool.get(key) match {
      case None =>
        Try(Await.result(Future(blocking(create(key))), timeout.milliseconds)).map { value =>
          pool(key) = value
          Some(value)
        }.toOption.getOrElse(None)
      case Some(value) =>
        pool -= key
        if(isBad(value)){
          destroy(value)
          None
        }
        else Some(value)
    }
    if(result.isEmpty) createOrRecycle(key) else result.get
  }

  def borrow[S](key: K)(action: T => S): S = {
    val value: T = synchronized {
      pool.get(key).fold(createOrRecycle(key)) { value =>
        pool -= key
        value
      }
    }

    val result: S = action(value)
    synchronized {
      pool(key) = value
    }
    result
  }
}
