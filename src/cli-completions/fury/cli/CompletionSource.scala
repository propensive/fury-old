package fury.cli

final private[cli] class CompletionSource(parameterValues: ParameterValueSources) {
  import ShellCompletion._

  def menuCompletions(menu: Menu[_], prefix: String): Seq[ShellCompletion] = {
    val matching = menu.commands.collect {
      case (name, command) if name.startsWith(prefix) => command
    }
    MenuCompletion(menu, matching.toSeq) :: Nil
  }

  def commandCompletions(
      parameters: Parameters[_],
      flatArgs: Seq[String],
      offset: Int
    ): Seq[ShellCompletion] = {
    def isParameter(value: String): Boolean = value.startsWith("-")
    def isFlag(value: String): Boolean      = parameters(value).exists(Parameter.isFlag)

    val argumentAt = flatArgs.lift
    val prefix     = argumentAt(offset).getOrElse("")

    val arguments = Arguments.bind(parameters, flatArgs)
    if (isParameter(prefix)) {
      parameterCompletions(parameters, arguments, prefix, valuePrefix = "")(NameCompletion)
    } else
      argumentAt(offset - 1) match {
        case Some(parameter) if isFlag(parameter) =>
          parameterCompletions(parameters, arguments, namePrefix = prefix, "")(NameCompletion)
        case Some(parameter) if isParameter(parameter) =>
          parameterCompletions(parameters, arguments, parameter, prefix)(ValueCompletion)

        case None =>
          parameterCompletions(parameters, arguments, prefix, valuePrefix = "")(NameCompletion)
        case _ => Nil // completing neither parameter name nor its value
      }
  }

  private def parameterCompletions(
      parameters: Parameters[_],
      arguments: Arguments,
      namePrefix: String,
      valuePrefix: String
    )(mode: ParameterCompletionMode
    ): Seq[ShellCompletion] =
    for {
      (name, parameter) <- matchingParameters(parameters, namePrefix)
      values            = matchingValues(parameter, valuePrefix)(arguments)
    } yield
      mode match {
        case NameCompletion  => ParameterNameCompletion(name, parameter, values)
        case ValueCompletion => ParameterValueCompletion(name, parameter, values)
      }

  private def matchingParameters(parameters: Parameters[_], namePrefix: String) =
    for {
      parameter <- parameters.all.toSeq
      alias     <- parameter.aliases
      if alias.startsWith(namePrefix)
    } yield (alias, parameter)

  private def matchingValues(parameter: Parameter[_], valuePrefix: String)(arguments: Arguments) =
    if (Parameter.isFlag(parameter)) Nil
    else
      parameterValues.sourceFor(parameter) match {
        case None         => Nil
        case Some(source) => source.values(valuePrefix)(arguments).sortBy(_.value)
      }

  sealed trait ParameterCompletionMode
  object NameCompletion  extends ParameterCompletionMode
  object ValueCompletion extends ParameterCompletionMode
}


