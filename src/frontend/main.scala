/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.8.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.                                         ║
   ║                                                                                                           ║
   ║ The primary distribution site is: https://propensive.com/                                                 ║
   ║                                                                                                           ║
   ║ Licensed under  the Apache License,  Version 2.0 (the  "License"); you  may not use  this file  except in ║
   ║ compliance with the License. You may obtain a copy of the License at                                      ║
   ║                                                                                                           ║
   ║     http://www.apache.org/licenses/LICENSE-2.0                                                            ║
   ║                                                                                                           ║
   ║ Unless required  by applicable law  or agreed to in  writing, software  distributed under the  License is ║
   ║ distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. ║
   ║ See the License for the specific language governing permissions and limitations under the License.        ║
   ╚═══════════════════════════════════════════════════════════════════════════════════════════════════════════╝
*/
package fury

import fury.core._, fury.strings._, fury.model._

import com.facebook.nailgun.NGContext
import exoskeleton._
import guillotine._
import scala.collection.JavaConverters._
import scala.collection.mutable.HashSet

import annotation.tailrec
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent._, duration._

import scala.util._

object Main {

  def invoke(cli: Cli[CliParam[_]])(implicit log: Log): ExitStatus = try {

    val layer = for {
      layout <- cli.layout
      layer  <- Layer.read(layout)
    } yield layer

    val actions = layer.toOption
      .to[List]
      .flatMap { ws =>
        ws.aliases
      }
      .map { alias =>
        def action(cli: Cli[CliParam[_]]) =
          AliasCli.context(cli).flatMap(BuildCli.compile(alias.schema, Some(alias.module)))
        Action(
            Symbol(alias.cmd.key),
            msg"${alias.description}",
            (cli: Cli[CliParam[_]]) => action(cli))
      }

    Recovery.recover(cli)(FuryMenu.menu(actions)(log)(cli, cli))
  } catch {
    case e: Throwable =>
      log.fail("Terminal error when running Fury")
      log.fail(msg"    ${e.toString}")
      e.getStackTrace.foreach { ln => log.fail(msg"        at $ln") }
      Abort
  }

  def main(args: Array[String]): Unit = run(
      System.in,
      System.out,
      System.err,
      args,
      System.exit(_),
      Environment(System.getenv.asScala.toMap, Option(System.getenv("PWD")))
  )

  def nailMain(ctx: NGContext): Unit = run(
      ctx.in,
      ctx.out,
      ctx.err,
      ctx.getArgs,
      ctx.exit(_),
      Environment(ctx.getEnv.stringPropertyNames.asScala.map { k =>
        (k, ctx.getEnv.getProperty(k))
      }.toMap, Option(ctx.getWorkingDirectory))
  )

  def run(
      in: java.io.InputStream,
      out: java.io.PrintStream,
      err: java.io.PrintStream,
      args: Seq[String],
      exit: Int => Unit,
      env: Environment
    ) =
    exit {
      val pid = Pid(args.head.toInt)
      implicit val log: Log = Log.log(pid)
      val cli = Cli(new java.io.PrintWriter(out), ParamMap(args.tail: _*), command = None, optCompletions = Nil, env, pid)
      Lifecycle.trackThread(cli, args.lift(1).exists(Set("about", "help")(_))) {
        val end = invoke(cli).code
        out.flush()
        err.flush()
        end
      }
    }

}
