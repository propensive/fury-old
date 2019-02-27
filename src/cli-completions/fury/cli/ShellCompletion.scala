package fury.cli

sealed private[cli] trait ShellCompletion

private[cli] object ShellCompletion {
  case class MenuCompletion(menu: Menu[_], commands: Seq[CLI[_]]) extends ShellCompletion

  case class ParameterNameCompletion(
      name: String,
      parameter: Parameter[_],
      values: Seq[ParameterValue])
      extends ShellCompletion

  case class ParameterValueCompletion(
      name: String,
      parameter: Parameter[_],
      values: Seq[ParameterValue])
      extends ShellCompletion
}
