package fury.cli

import fury.cli.Parameter._
import probably._

object ParametersTest extends TestApp {
  private val a  = flag("a", "a", "")
  private val aa = flag("aa", "aa", "")
  private val b  = flag("b", "b", "")

  private val parameters = a :+ aa :+ b

  def tests(): Unit = {
    test("filters parameters by prefix") {
      parameters.matching("--a")
    }.assert(matched => matched.contains(a) && matched.contains(aa))

    test("finds parameters by specific name") {
      parameters.apply("--a")
    }.assert(matched => matched.contains(a))
  }
}
