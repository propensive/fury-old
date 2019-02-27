package fury.cli

import java.io.PrintStream

import fury.cli.CLI._
import fury.{Abort, Done, ExitStatus}

import scala.util.Try

final class CommandCompleter private (interface: CLI[Seq[String]]) {

  def run(args: Seq[String])(out: PrintStream, err: PrintStream): Try[ExitStatus] =
    interface.execute(args) match {
      case Success(completions) => Try(completions.foreach(out.println)).map(_ => Done)
      case Failure(cause)       => Try(err.println(cause)).map(_ => Abort)
      case CommandUnclear(cmd)  => Try(HelpPrinter.print(cmd).foreach(out.println)).map(_ => Done)
    }
}

object CommandCompleter {

  def apply(completedInterface: CLI[_], sources: ParameterValueSources): CommandCompleter = {
    val cli =
      menu("Completion", "complete")(
          command("complete",
                  "complete a command",
                  new CompletionCommand(completedInterface, sources)))

    new CommandCompleter(cli)
  }
}
