package test

object HelloInternet {

  def main(args: Array[String]) = {
    val url = "https://fury.build"
    val result = scala.io.Source.fromURL(url).mkString
    Predef.assert(result.take(15) == "<!DOCTYPE html>")
  }

}
