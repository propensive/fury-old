package print

import scala.io._

object Main extends App {
  println("Running print module")
  Source.fromFile("file1").getLines.foreach(println)
  Source.fromFile("subdir2/subdir/file2").getLines.foreach(println)
}
