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

import fury._, core._, text._, model._, io._, utils._

import com.facebook.nailgun.NGContext
import exoskeleton._
import guillotine._
import jovian._
import scala.collection.JavaConverters._
import scala.collection.mutable.HashSet

import annotation.tailrec
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent._, duration._

import scala.util._

import java.io._
import java.util.Date
import java.text._

object FuryServer {

  private final val dateFormat: DateFormat = new SimpleDateFormat("yyyy-MM-dd")
  private def logFile() = Installation.logsDir.extant() / str"${dateFormat.format(new Date()).toString}.log"
  
  private def printWriter(): PrintWriter =
    new PrintWriter(new BufferedWriter(new FileWriter(logFile().javaFile, true)))

  val logThread = new Thread {
    override def run(): Unit = Bus.listen(500) { stream =>
      
      @tailrec
      def writeLogs(lastWriter: PrintWriter, stream: Stream[Tick], lastDay: Int = new Date().getDay): Unit =
        if(!stream.isEmpty) {
          val day = new Date().getDay
          // When the day changes or the file gets deleted, create a new printWriter
          val writer = if(day != lastDay || !logFile().exists) printWriter() else lastWriter
          val tick = stream.head
          tick.events.foreach {
            case LogMessage(msg) =>
              writer.write(msg.string(Theme.Full))
              writer.write('\n')
            case TaskProgress(_, _) => ()
            case other =>
              writer.write(other.toString)
              writer.write('\n')
          }
          
          writer.write(msg"state: ${tick.state.toString}".string(Theme.Full))
          writer.write("\n")
          writer.flush()

          writeLogs(writer, stream.tail, day)
        }
        writeLogs(printWriter(), stream)
      }
  }

  logThread.setDaemon(true)
  logThread.start()

  def invoke(cli: Cli): ExitStatus = {
    Recovery.recover(cli) {
      if(cli.args.args.length > 0 && cli.args.args(0).endsWith(".scala")) BuildCli(cli).script()
      else {
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

  def run(in: InputStream,
          out: PrintStream,
          err: PrintStream,
          args: Seq[String],
          exit: Int => Unit,
          env: Environment)
         : Unit =
    exit {
      val pid = Pid(args.head.toInt)
      val job = Job(msg"${args.join(" ")}", pid)
      val cli = Cli(new PrintWriter(out), ParamMap(args.tail: _*), command = None, optCompletions = Nil, env, job)
      
      Lifecycle.trackThread(cli, args.lift(1).exists(Set("about", "help")(_))) {
        val exitStatus = invoke(cli).code
        out.flush()
        err.flush()
        exitStatus
      }
    }
}
