package p

import p.in.B._
import p._

object A {
  def format(a: String)(implicit cv: String => Int): Int = cv(a)

  def main(args: Array[String]): Unit = {
    println(format("X"))
  }
}