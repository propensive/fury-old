/*

    Fury, version 0.33.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury

import io._, text._, core._

import guillotine._
import exoskeleton._

import com.facebook.nailgun.NGContext

import scala.collection.JavaConverters._
import scala.util._
import scala.concurrent._, duration._

object FuryServer {
  
  Runtime.getRuntime.addShutdownHook {
    new Thread(() => Bus.enqueue(msg"JVM is exiting; shutting down", _.shutdown(), level = Halt))
  }

  val logThread = new Thread({ () => Bus.listen(200)(_ => true)(LogWriter(_)) })

  logThread.setDaemon(true)
  logThread.start()

  def main(args: Array[String]): Unit = {
    def exit(exitStatus: ExitStatus): Unit = System.exit(exitStatus.code)
    val environment = Environment(System.getenv.asScala.toMap, Option(System.getenv("PWD")))
    val pid = Pid(args.head.toInt)
    val session = Session(System.in, System.out, System.err, environment, args.to[List].tail, exit)

    run(pid, session)
  }

  def nailMain(ctx: NGContext): Unit = {
    def exit(exitStatus: ExitStatus): Unit = ctx.exit(exitStatus.code)
    val env = ctx.getEnv.stringPropertyNames.asScala.map { k => (k, ctx.getEnv.getProperty(k)) }.toMap
    val environment = Environment(env, Option(ctx.getWorkingDirectory))
    val pid = Pid(ctx.getArgs.head.toInt)
    val session = Session(ctx.in, ctx.out, ctx.err, environment, ctx.getArgs.to[List].tail, exit)
    
    run(pid, session)
  }

  def run(pid: Pid, session: Session): Unit = {
    Bus.enqueue(msg"Opening new session $session", _(pid) = session)
    import Bus.execCtx
    // FIXME: The job should move deeper inside the application, probably.
    val future = Future { blocking { try invoke(session) finally Bus.terminate(session) } }
    Logger(false).streamTo(session)
    log.info(msg"Stopped logging the session")
    session.exit(Bus.await(session))
  }
  
  def invoke(session: Session): ExitStatus = {
    val cli = Cli(ParamMap(session.args: _*), command = None, optCompletions = Nil, session)
    Recovery.recover(cli) {
      //Bus.put(StartJob(session, job))
      /*if(cli.args.args.length > 0 && cli.args.args(0).endsWith(".scala")) BuildCli(cli).script()
      else */{
        val layer: Try[Layer] = for {
            layout <- cli.layout
            conf   <- Layer.readFuryConf(layout)
            layer  <- Layer.retrieve(conf)
        } yield layer

        val actions = layer.toOption.to[List].flatMap(_.aliases).map { alias =>
          def action(cli: Cli) = BuildCli(cli).compile(Some(alias.module), alias.args)

          Action(Symbol(alias.id.key), alias.description, (cli: Cli) => action(cli))
        }

        FuryMenu.menu(actions)(cli, cli)
      }
    }
  }
}