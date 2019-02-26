package fury

import fury.cli._
import probably._

object Tests {
  private val testSuites = List[TestApp](
      ParametersTest,
      ArgumentsTest
  )

  def main(args: Array[String]): Unit = testSuites.foreach(_.main(args))
}
