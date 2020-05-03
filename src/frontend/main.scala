/*

    Fury, version 0.15.1. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

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

object FuryServer {

  def invoke(cli: Cli)(implicit log: Log): ExitStatus = {

    val layer = for {
      layout <- cli.layout
      conf   <- Layer.readFuryConf(layout)
      layer  <- Layer.retrieve(conf)
    } yield layer

    val actions = layer.toOption.to[List].flatMap(_.aliases).map { alias =>
        def action(cli: Cli) = BuildCli(cli).compile(Some(alias.module), alias.args)

        Action(Symbol(alias.id.key), msg"${alias.description}", (cli: Cli) => action(cli))
      }

    Recovery.recover(cli)(FuryMenu.menu(actions)(log)(cli, cli))
  }

  def main(args: Array[String]): Unit = {
    def exit(code: Int) = {
      Lifecycle.shutdown()
      System.exit(code)
    }
    
    run(System.in, System.out, System.err, args, exit, Environment(System.getenv.asScala.toMap, Option(
        System.getenv("PWD"))))
  }

  def nailMain(ctx: NGContext): Unit =
    run(ctx.in, ctx.out, ctx.err, ctx.getArgs, ctx.exit(_), Environment(
        ctx.getEnv.stringPropertyNames.asScala.map { k => (k, ctx.getEnv.getProperty(k)) }.toMap, Option(
        ctx.getWorkingDirectory))
  )

  def run(in: java.io.InputStream,
          out: java.io.PrintStream,
          err: java.io.PrintStream,
          args: Seq[String],
          exit: Int => Unit,
          env: Environment)
         : Unit =
    exit {
      val pid = Pid(args.head.toInt)
      implicit val log: Log = Log.log(pid)
      
      val cli = Cli(new java.io.PrintWriter(out), ParamMap(args.tail: _*), command = None, optCompletions = Nil,
          env, pid)
      
      Lifecycle.trackThread(cli, args.lift(1).exists(Set("about", "help")(_))) {
        val exitStatus = invoke(cli).code
        out.flush()
        err.flush()
        exitStatus
      }
    }

}
