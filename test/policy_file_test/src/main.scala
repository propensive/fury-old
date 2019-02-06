package test

import java.io._

object WriteInsideSharedDir {

  def main(args: Array[String]) = {
    val sharedDir = System.getProperty("fury.sharedDir")
    val f         = new File(sharedDir ++ "/myfile")
    val bw        = new BufferedWriter(new FileWriter(f))
    bw.write("")
    bw.close()
  }
}

object WriteOutsideSharedDir {

  def main(args: Array[String]) = {
    val sharedDir = System.getProperty("fury.sharedDir")
    val f         = new File(sharedDir ++ "/../myfile")
    val bw        = new BufferedWriter(new FileWriter(f))
    bw.write("")
    bw.close()
  }
}
