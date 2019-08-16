/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.6.1. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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

import scala.concurrent.ExecutionContext.Implicits.global

import fury.external._
import probably._

object PoolTest extends TestApp {
  
  private val dummyPool: Pool[String, Symbol] = new Pool[String, Symbol](10L) {
    override def create(key: String): Symbol = Symbol(key)
    override def destroy(value: Symbol): Unit = ()
    override def isBad(value: Symbol): Boolean = false
  }

  override def tests(): Unit = {
    test("reuse existing entries") {
      dummyPool.borrow("a/b/c"){void}
      dummyPool.borrow("a/b/x"){void}
      dummyPool.borrow("a/b/c"){void}
      dummyPool.size
    }.assert(_ == 2)

    test("Return correct values") {
      var result1: Symbol = null
      var result2: Symbol = null
      var result3: Symbol = null
      dummyPool.borrow("a/b/c"){result1 = _}
      dummyPool.borrow("a/b/x"){result2 = _}
      dummyPool.borrow("a/b/c"){result3 = _}
      (result1, result2, result3)
    }.assert(_ == (Symbol("a/b/c"), Symbol("a/b/x"), Symbol("a/b/c")))
  }
  
  private def void: Any => Unit = _ => ()

}
