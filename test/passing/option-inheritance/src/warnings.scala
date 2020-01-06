object Test {
  List(1, "two")     // -Ywarn-infer-any         ia
  def foo: Unit = () // -Ywarn-nullary-unit      nu
  Some(1, 2)         // -Ywarn-adapted-args      aa
}
