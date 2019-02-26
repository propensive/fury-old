package fury.cli

import scala.annotation.tailrec
import scala.util.Try

final class Arguments(val options: Map[String, String], val positional: Seq[String]) {

  /**
    * Is not fully type safe (in contrast to [[fury.cli.Action.Bindings]]) to simplify specifying completions
    */
  def apply[A](param: Parameter[A]): Try[A] = param.apply(this)

  def valueOf(param: Parameter[_]): Option[String] =
    param.aliases.collectFirst {
      case name if options.contains(name) => options(name)
    }
}

private[cli] object Arguments {

  def bind(parameters: Parameters[_], arguments: Seq[String]): Arguments = {

    def withOption(bindings: Arguments)(option: String, value: String = "") =
      new Arguments(bindings.options + (option -> value), bindings.positional)

    def withPositional(bindings: Arguments)(positional: Seq[String]) =
      new Arguments(bindings.options, bindings.positional ++ positional)

    @tailrec
    def bind(arguments: Seq[String], bindings: Arguments): Arguments =
      arguments match {
        case Seq()              => bindings
        case "--" +: positional => withPositional(bindings)(positional)
        case name +: valueTail if name.startsWith("-") =>
          parameters(name) match {
            case None          => bind(valueTail, bindings) // ignoring unknown parameter
            case Some(_: Flag) => bind(valueTail, withOption(bindings)(name))
            case Some(_: Named[_]) =>
              valueTail match {
                case Nil => bindings // parameter not bound ("cmd --param")
                case value +: _ if value.startsWith("-") =>
                  bind(valueTail, withOption(bindings)(name)) // value not specified ("cmd --p1 --p2")
                case value +: tail =>
                  bind(tail, withOption(bindings)(name, value))
              }
          }
        case argument +: tail =>
          bind(tail, withPositional(bindings)(Seq(argument)))
      }

    bind(arguments, new Arguments(Map(), Nil))
  }
}
