package create

import java.io._

object Main extends App {
  println("Running create module")
  val writer = new FileWriter("file1")
  writer.write("File contents 1")
  writer.close()
  
  val dir = new File("subdir")
  dir.mkdirs()
  val writer2 = new FileWriter("subdir/file2")
  writer2.write("File contents 2")
  writer2.close()
}
