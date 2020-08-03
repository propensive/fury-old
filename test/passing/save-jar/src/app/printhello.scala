object HelloWorld extends App {
  new java.io.PrintWriter(".content") {
    write(Constants.text)
    close()
  }
}