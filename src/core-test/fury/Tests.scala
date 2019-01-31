package fury

import fury.ogdl.OgdlParserTest
import probably.TestApp

object Tests {
  private val testSuites = List[TestApp]()

  def main(args: Array[String]): Unit = testSuites.foreach(_.main(args))
}
