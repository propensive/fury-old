package fury

import com.facebook.nailgun.NGContext
import fury.cli._

import scala.util.{Failure, Success}

object FuryCompletionApp {
  def main(args: Array[String]): Unit = execute(args)(ExecutionContext())
  def nailMain(ctx: NGContext): Unit  = execute(ctx.getArgs)(ExecutionContext(ctx))

  private def execute(arguments: Seq[String])(ctx: ExecutionContext): Unit = {
    val completer   = CommandCompleter(FuryCLI.interface, new FuryParameterSources(ctx))
    val completions = completer.run(arguments)(ctx.out, ctx.err)

    completions match {
      case Success(status) => System.exit(status.code)
      case Failure(cause) =>
        ctx.err.println(cause)
        System.exit(1)
    }
  }
}

final class FuryParameterSources(ctx: ExecutionContext) extends ParameterValueSources {
  override def sourceFor(parameter: cli.Parameter[_]): Option[ParameterValueSource] =
    sources.get(parameter)

  private val sources: Map[cli.Parameter[_], ParameterValueSource] = Map(
      FuryCLIParameters.Theme      -> ThemeCompleter,
      FuryCLIParameters.Schema     -> new SchemaCompleter(ctx),
      FuryCLIParameters.Project    -> new ProjectCompleter(ctx),
      FuryCLIParameters.Module     -> new ModuleCompleter(ctx),
      FuryCLIParameters.Link -> new DependencyCompleter(ctx)
  )
}
