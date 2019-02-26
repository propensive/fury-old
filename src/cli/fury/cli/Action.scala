package fury.cli

import fury.cli.Action.Bindings

import scala.util.Try

/**
  * @param parameters - specifies all of the parameters used by this command.
  *                   Helps during shell completion (allows to provide suggestions for every parameter from the list)
  */
class Action[+A <: Parameter[_], +B](val parameters: Parameters[A])(
  f: Bindings[A] => Try[B]
) {

  final def execute(arguments: Seq[String]): Try[B] = {
    val bindings = new Bindings(Arguments.bind(parameters, arguments))
    f(bindings)
  }
}

private[cli] object Action {

  def apply[A](value: A): Action[Nothing, A] = new Completed(value)

  /**
    * Provides type-safe access for [[fury.cli.Parameter]] values (a parameter can only be accessed
    * if it was specified as an action parameter - @see [[fury.cli.Action]]).
    */
  final class Bindings[+A](arguments: Arguments) {
    def apply[B](param: Parameter[B])(implicit ev: A <:< param.type): Try[B] =
      param(arguments)
    def positional: Seq[String] = arguments.positional
  }

  final class Completed[A](value: A)
      extends Action[Nothing, A](Parameters.empty)(_ => Try(value))
}
