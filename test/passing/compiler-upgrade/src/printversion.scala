class Foo(private[this] var a: Int) {
   a = a + 1
   def foo = a
}

object CheckVersion extends App {
  val result = new Foo(0).foo
  // See https://github.com/scala/bug/issues/12003 for explanation
  val version = if(result == 1) "2.12.12 or newer" else "2.12.11 or older"
  println(s"Scala compiler is: $version")
}
