package fury

import java.io.PrintStream

import com.facebook.nailgun.NGContext
import guillotine.Environment

import scala.util.Try

final class ExecutionContext(val out: PrintStream, val err: PrintStream, val env: Environment) {
  lazy val config: Config = layout.flatMap(Config.read()(env, _)).getOrElse(Config())
  lazy val io: Io         = new Io(out, config)

  def layout: Try[Layout] =
    for {
      workingDir <- env.workDir.map(Path.apply).ascribe(FileNotFound(Path("/")))
      homeDir    = Path(env.variables("HOME"))
    } yield Layout(homeDir, workingDir, env)

  def layer: Try[Layer] =
    for {
      layout <- layout
      layer  <- Layer.read(layout)(io)
    } yield layer
}

object ExecutionContext {
  import scala.collection.JavaConverters._

  def apply(): ExecutionContext = {
    val env = Environment(System.getenv.asScala.toMap, Option(System.getenv("PWD")))
    new ExecutionContext(System.out, System.err, env)
  }

  def apply(ctx: NGContext): ExecutionContext = {
    val ngEnv = ctx.getEnv
    val props = ctx.getEnv.stringPropertyNames.asScala.map(k => k -> ngEnv.getProperty(k))
    val env   = Environment(props.toMap, Option(ctx.getWorkingDirectory))
    new ExecutionContext(ctx.out, ctx.err, env)
  }
}
