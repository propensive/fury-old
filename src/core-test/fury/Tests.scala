package fury

import fury.layer.LayerRepositoryTest
import probably.TestApp

object Tests {
  private val testSuites = List[TestApp](
      LayerRepositoryTest
  )

  def main(args: Array[String]): Unit = testSuites.foreach(_.main(args))
}
