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
package fury

import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.ExecutionContext.Implicits.global
import fury.utils._
import probably._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.Random

object PoolTest extends TestApp {

  class Resource(val tag: String)

  class DummyPool extends Pool[String, Resource](10L) {
      override def create(key: String): Resource = new Resource(key)
      override def destroy(value: Resource): Unit = ()
      override def isBad(value: Resource): Boolean = false

      def syncBorrow[S](key: String)(action: Resource => S): S = {
        Await.result(borrow(key)(action), Duration.Inf)
      }
  }

  override def tests(): Unit = {
    test("reuse existing entries") {
      val dummyPool = new DummyPool
      dummyPool.syncBorrow("a/b/c")(void)
      dummyPool.syncBorrow("a/b/x")(void)
      dummyPool.syncBorrow("a/b/c")(void)
      dummyPool.size
    }.assert(_ == 2)

    test("support concurrent requests") {
      val resourcesCreated = new AtomicInteger(0)
      val dummyPool = new DummyPool{
        override def create(key: String): Resource = {
          resourcesCreated.incrementAndGet
          super.create(key)
        }
      }
      val keys = Stream.from(0).take(6).map(i => s"key $i")
      val tasks = Future.traverse(Random.shuffle((1 to 20).flatMap(_ => keys))){
        key => dummyPool.borrow(key){ _ => Thread.sleep(100) }
      }
      Await.result(tasks, 1 minute)
      resourcesCreated.get
    }.assert(_ == 6)

    test("Return correct values") {
      val dummyPool = new DummyPool
      var result1: Resource = null
      var result2: Resource = null
      var result3: Resource = null
      dummyPool.syncBorrow("a/b/c"){ result1 = _ }
      dummyPool.syncBorrow("a/b/x"){ result2 = _ }
      dummyPool.syncBorrow("a/b/c"){ result3 = _ }
      (result1.tag, result2.tag, result3.tag)
    }.assert(_ == ("a/b/c", "a/b/x", "a/b/c"))
  }
  
  private def void: Any => Unit = _ => ()

}
