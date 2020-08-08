package print

import java.io._

object Main extends App {
  println("Running print module")
  val dir = new File("classfiles")
  dir.listFiles().map(_.toString).sorted.foreach(println)
}
