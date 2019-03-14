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
