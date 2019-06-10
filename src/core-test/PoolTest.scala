package fury

import scala.concurrent.ExecutionContext.Implicits.global

import fury.core.Pool
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
