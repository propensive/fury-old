package fury.cli

final class Parameters[+A <: Parameter[_]](parameters: Set[Parameter[_]]) {

  def :+[B <: Parameter[_]](p: B): Parameters[A with p.type] =
    new Parameters(parameters + p)

  def all: Set[Parameter[_]]                      = parameters
  def apply(name: String): Option[Parameter[_]]   = parameters.find(named(name))
  def matching(prefix: String): Set[Parameter[_]] = parameters.filter(prefixedWith(prefix))

  private def named(name: String)(p: Parameter[_]) = p.aliases.contains(name)
  private def prefixedWith(prefix: String)(p: Parameter[_]) =
    p.aliases.exists(_.startsWith(prefix))
}

object Parameters {
  val empty: Parameters[Nothing] = new Parameters(Set())

  def of(parameter: Parameter[_]): Parameters[Nothing with parameter.type] =
    empty :+ parameter
}
