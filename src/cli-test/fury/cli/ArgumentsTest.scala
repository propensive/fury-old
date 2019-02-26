package fury.cli

import fury.cli.Arguments.bind
import fury.cli.Parameter._
import probably._

object ArgumentsTest extends TestApp {
  override def tests(): Unit = {
    test("not specified properties are ignored") {
      bind(Parameters.empty, Seq("-p", "v"))
    }.assert(args => args.options.isEmpty)

    test("value of not specified property is treated as a positional argument") {
      bind(Parameters.empty, Seq("-p", "value", "positional"))
    }.assert(args => args.positional.endsWith(Seq("value", "positional")))

    test("only one alias of specified property is mapped to the option value") {
      val parameter = Parameter.parameter("name", "n", "")(Some.apply)
      bind(Parameters.of(parameter), Seq("--name", "value"))
    }.assert(
        args => args.options.size == 1 && args.options("--name") == "value"
    )

    test("value after option is treated as a positional argument") {
      val f = flag("flag", "f", "")
      bind(Parameters.of(f), Seq("--flag", "value"))
    }.assert(
        args => args.options("--flag") == "" && args.positional == Seq("value")
    )

    test("two parameters in a row cause first one to have an empty values") {
      val p1 = parameter("param", "p", "")(Some.apply)
      val p2 = parameter("other", "o", "")(Some.apply)
      bind(p1 :+ p2, Seq("--param", "--other", "value"))
    }.assert(args => args.options("--param") == "" && args.options("--other") == "value")
  }
}
