package fury

import fury.core.{Pool, RetryingPool}
import probably._

import scala.concurrent.ExecutionContext.Implicits.global

object RetryingPoolTest extends TestApp {
  
  private val ok: collection.mutable.Map[Symbol, Unit] = collection.concurrent.TrieMap()
  
  private val dummyPool: Pool[String, Symbol] = new RetryingPool[String, Symbol, DummyException](10L) {
    override def create(key: String): Symbol = Symbol(key)
    override def destroy(value: Symbol): Unit = ()
    override def isBad(value: Symbol): Boolean = !ok.contains(value)
  }

  override def tests(): Unit = {
    test("reuse existing entries") {
      dummyPool.borrow("a/b/c"){mayThrow}
      dummyPool.borrow("a/b/x"){mayThrow}
      dummyPool.borrow("a/b/c"){mayThrow}
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
  
  private def mayThrow: Symbol => Unit = sym => {
    if(ok.contains(sym)) () else {
      ok(sym) = ()
      throw DummyException(sym)
    }
  }
  
  private case class DummyException(target: Symbol) extends Exception

}
