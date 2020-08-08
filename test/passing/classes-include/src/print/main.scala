package print

import java.io._

object Main extends App {
  println("Running print module")
  val dir = new File("classfiles")
  dir.listFiles().foreach(println)
}
