import java.io._

object HelloWorld extends App {
  new PrintWriter(".content") {
    println("Hello World\n"); close()
  }
}
