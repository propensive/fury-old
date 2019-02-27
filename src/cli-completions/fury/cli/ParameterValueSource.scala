package fury.cli

import scala.util._

trait ParameterValueSources {
  def sourceFor(parameter: Parameter[_]): Option[ParameterValueSource]
}

trait ParameterValueSource {
  final def values(prefix: String)(arguments: Arguments): Seq[ParameterValue] =
    matchingValues(prefix)(arguments) match {
      case Success(values) => values.filter(matches(prefix))
      case Failure(_)      => Nil // TODO log the cause
    }

  protected def matchingValues(prefix: String)(arguments: Arguments): Try[Seq[ParameterValue]]
  private def matches(prefix: String): ParameterValue => Boolean = _.value.startsWith(prefix)
}

case class ParameterValue(value: String, description: Option[String] = None)
