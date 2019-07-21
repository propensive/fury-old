package fury

import fury.ogdl.{OgdlParserTest, OgdlSerializerTest}
import probably.TestApp

object Tests {
  private val testSuites = List[TestApp](
      OgdlParserTest,
      OgdlSerializerTest
  )

  def main(args: Array[String]): Unit = testSuites.foreach(_.execute())
}
