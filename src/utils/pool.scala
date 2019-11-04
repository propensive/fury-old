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

abstract class Pool[K, T <: AnyRef](private implicit val ec: ExecutionContext) {

  def create(key: K): T
  def destroy(value: T): Unit
  def isBad(value: T): Boolean

  private[this] val pool: Map[K, Future[T]] = scala.collection.concurrent.TrieMap()
  
  def keySet: Set[K] = pool.keySet

  protected def get(key: K): Future[T] = pool(key)

  protected def remove(key: K): Unit = {
    pool -= key
  }

  protected def add(key: K, value: Future[T]): Unit = {
    pool(key) = value
  }

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
    val lock: AnyRef = pool
    
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
        }
        else released.success(v)
      }
    }
  }

}

sealed trait PoolCleaner[K, T <: AnyRef] { this: Pool[K, T] =>

  def isIdle(value: T): Boolean

  val cleaningInterval: Long

  def clean()(implicit cleaner: ExecutionContext): Future[Unit] = {
    Future.traverse(keySet) { key =>
      val foo = get(key)
      foo.synchronized{
        foo.map { conn =>
          remove(key)
          if(isIdle(conn)) {
            destroy(conn)
            //add(key, Future.failed[T](new Exception("Resource has been destroyed")))
          } else add(key, Future.successful(conn))
        }
      }
    }.map(_ => ())
  }

  protected def keepCleaning()(implicit cleaner: ExecutionContext): Future[Unit] = {
    clean().andThen { case _ =>
      Thread.sleep(cleaningInterval)
      keepCleaning()
    }
  }

}

abstract class SelfCleaningPool[K, T <: AnyRef](val cleaningInterval: Long)(implicit ec: ExecutionContext)
  extends Pool[K, T]()(ec) with PoolCleaner[K, T] {

  keepCleaning()(Threads.singleThread("pool-cleaner", daemon = true))

}
