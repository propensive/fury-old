package fury

import fury.layer.LayerRepositoryTest
import fury.layer.schema.FurySchemaTest
import probably.TestApp

object Tests {
  private val testSuites = List[TestApp](
      FurySchemaTest,
      LayerRepositoryTest
  )

  def main(args: Array[String]): Unit = testSuites.foreach(_.main(args))
}
