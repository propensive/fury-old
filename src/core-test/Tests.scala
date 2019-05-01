package fury

import probably.TestApp

object Tests {
  private val testSuites = List[TestApp](
      DirectedGraphTest,
      LayerRepositoryTest,
      TablesTest
  )

  def main(args: Array[String]): Unit = testSuites.foreach(_.main(args))
}
