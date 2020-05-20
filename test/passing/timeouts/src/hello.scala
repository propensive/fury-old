object HelloWorld extends App {
  println("Waiting five seconds...")
  Thread.sleep(5000L)
  println("Timeout was not applied.")
}
