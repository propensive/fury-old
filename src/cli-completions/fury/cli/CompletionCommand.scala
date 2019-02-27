package fury.cli

import fury.cli.CompletionDispatcher.complete
import fury.cli.CompletionParameters._
import fury.cli.Parameter.parameter
import fury.cli.Shell.Zsh
import fury.cli.CompletionFormatter._

final private[cli] class CompletionCommand(interface: CLI[_], sources: ParameterValueSources)
    extends Action(ShellParameter :+ OffsetParameter)(bindings =>
      for {
        shell  <- bindings(ShellParameter)
        offset <- bindings(OffsetParameter)

        argumentPosition = shell match {
          case Zsh => offset - 1
          case _   => offset
        }

        suggestions   <- complete(interface, argumentPosition, sources, bindings.positional)
      } yield format(suggestions)(shell, offset))

private[cli] object CompletionParameters {
  val ShellParameter  = parameter("shell", "s", "shell (e.g. bash, zsh)")(Shell.parse)
  val OffsetParameter = parameter("offset", "o", "offset of a command")(s => Some(s.toInt))
}
