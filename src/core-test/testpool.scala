/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.6.6. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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

import java.util.concurrent.CountDownLatch

import scala.concurrent.ExecutionContext.Implicits.global
import fury.utils._
import probably._
import scala.concurrent.Future
import scala.util.Random

object PoolTest extends TestApp {

  class Resource(val tag: String)

  class DummyPool extends Pool[String, Resource](10L) {
      override def create(key: String): Resource = new Resource(key)
      override def destroy(value: Resource): Unit = ()
      override def isBad(value: Resource): Boolean = false
  }

  override def tests(): Unit = {
    test("reuse existing entries") {
      val dummyPool = new DummyPool
      dummyPool.borrow("a/b/c"){void}
      dummyPool.borrow("a/b/x"){void}
      dummyPool.borrow("a/b/c"){void}
      dummyPool.size
    }.assert(_ == 2)

    test("support concurrent requests") {
      val dummyPool = new DummyPool
      val allFinished = new CountDownLatch(20)
      val keys = ('k' to 'p').map(i => s"key $i")
      (1 to 20).map { _ => Future {
        Random.shuffle(keys).foreach{ key =>
          dummyPool.borrow(key){_ => Thread.sleep(100)}
        }
        allFinished.countDown()
      }}
      allFinished.await()
      dummyPool
    }.assert(_.size == 6)

    test("Return correct values") {
      val dummyPool = new DummyPool
      var result1: Resource = null
      var result2: Resource = null
      var result3: Resource = null
      dummyPool.borrow("a/b/c"){result1 = _}
      dummyPool.borrow("a/b/x"){result2 = _}
      dummyPool.borrow("a/b/c"){result3 = _}
      (result1.tag, result2.tag, result3.tag)
    }.assert(_ == ("a/b/c", "a/b/x", "a/b/c"))
  }
  
  private def void: Any => Unit = _ => ()

}
