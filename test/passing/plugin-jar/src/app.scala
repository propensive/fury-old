package app

object Main {

  // Copied from https://github.com/typelevel/kind-projector/blob/master/src/test/scala/bounds.scala
  trait Leibniz[-L, +H >: L, A >: L <: H, B >: L <: H]

  object Test {
    trait Foo
    trait Bar extends Foo

    def outer[A >: Bar <: Foo] = {
      def test[F[_ >: Bar <: Foo]] = "Hello kind-projector!"
      test[λ[`b >: Bar <: Foo` => Leibniz[Bar, Foo, A, b]]]
    }
  }

  def main(args: Array[String]): Unit = {
    println(Test.outer)
  }
}

