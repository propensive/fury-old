package fury.cli

import fury.cli.CLI.{CommandUnclear, InvalidArgument}

import scala.util._

private[cli] object CompletionDispatcher {

  def complete[A](
      cli: CLI[A],
      offset: Int,
      sources: ParameterValueSources,
      arguments: Seq[String]
    ): Try[Seq[ShellCompletion]] = {
    val dispatcher      = new CompletionDispatcher[A](offset, new CompletionSource(sources))
    val actualArguments = arguments.drop(1) // first argument is the application name
    dispatcher.visit(cli, actualArguments)
  }
}

final private[cli] class CompletionDispatcher[A](offset: Int, completions: CompletionSource)
    extends CLI.Visitor[A, Try[Seq[ShellCompletion]]] {
  override def onMenu(menu: Menu[A], arguments: Seq[String]): Try[Seq[ShellCompletion]] =
    arguments match {
      case "--" +: _ => menu.traverse(arguments, this) // ignore '--' during completion
      case _ =>
        if (offset == 1) {
          val prefix = arguments.headOption.getOrElse("")
          Try(completions.menuCompletions(menu, prefix))
        } else {
          menu.traverse(arguments, new CompletionDispatcher(offset - 1, completions))
        }
    }

  override def onCommand(command: Command[A], arguments: Seq[String]): Try[Seq[ShellCompletion]] =
    Try(completions.commandCompletions(command.action.parameters, arguments, offset - 1))

  override def onUnclearCommand(menu: Menu[A]): Try[Seq[ShellCompletion]] =
    Failure(CommandUnclear(menu))

  override def onUnknownArgument(menu: Menu[A], argument: String): Try[Seq[ShellCompletion]] =
    Failure(InvalidArgument(menu, argument))
}
