import probably._

object DummyTest extends Suite() {
  def run(test: Runner): Unit = {
    test("the sum of two identical integers is divisible by two") {
      val x: Int = 7
      x + x
    }.assert(_%2 == 0)
  }
}


