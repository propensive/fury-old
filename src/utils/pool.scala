/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.6.7. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
   ║                                                                                                           ║
   ║ The primary distribution site is: https://propensive.com/                                                 ║
   ║                                                                                                           ║
   ║ Licensed under  the Apache License,  Version 2.0 (the  "License"); you  may not use  this file  except in ║
   ║ compliance with the License. You may obtain a copy of the License at                                      ║
   ║                                                                                                           ║
   ║     http://www.apache.org/licenses/LICENSE-2.0                                                            ║
   ║                                                                                                           ║
   ║ Unless required  by applicable law  or agreed to in  writing, software  distributed under the  License is ║
   ║ distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. ║
   ║ See the License for the specific language governing permissions and limitations under the License.        ║
   ╚═══════════════════════════════════════════════════════════════════════════════════════════════════════════╝
*/
package fury.utils

import scala.collection.mutable.Map
import scala.collection.Set
import scala.concurrent._
import scala.util._

abstract class Pool[K, T <: AnyRef](val timeout: Long)(implicit ec: ExecutionContext) {

  def create(key: K): T
  def destroy(value: T): Unit
  def isBad(value: T): Boolean

  val pool: Map[K, Future[T]] = scala.collection.concurrent.TrieMap()
  
  def keySet: Set[K] = pool.keySet

  private[this] final def createOrRecycle(key: K): Future[T] = {
    val result = pool.get(key) match {
      case None =>
        val res = Future(blocking(create(key)))
        pool(key) = res
        res
      case Some(value) =>
        value.filter{v =>
          val bad = isBad(v)
          if(bad) { destroy(v) }
          !bad
        }.andThen{
          case Failure(e) => pool -= key
        }
    }
    result.recoverWith{ case _ => createOrRecycle(key) }
  }

  def borrow[S](key: K)(action: T => S): Future[S] = {
    val released = Promise[T]
    val lock: AnyRef = pool.get(key).getOrElse(pool)
    
    val claimed: Future[T] = lock.synchronized {
      createOrRecycle(key).map { v =>
        pool(key) = released.future
        v
      }
    }

    claimed.map { v =>
      try action(v)
      finally {
        if(isBad(v)) {
          destroy(v)
          released.failure(new Exception("Resource got stale"))
        } else released.success(v)
      }
    }
  }

}
