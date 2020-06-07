/*

    Fury, version 0.16.1. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury

import fury.core._, fury.text._

import scala.util._

import java.{text => jt}

case class AboutCli(cli: Cli)(implicit log: Log) {

  def version: Try[ExitStatus] = for {
    call <- cli.call()
    _    <- ~log.raw(str"fury version ${FuryVersion.current}")
  } yield log.await()

  private def resourcesString: String = {
    val runtime = Runtime.getRuntime
    val df: jt.DecimalFormat = new jt.DecimalFormat("0.0")

    def magnitude(value: Double, scale: List[String] = List("", "k", "M", "G", "T")): String =
      if(value < 1024) s"${df.format(value)}${scale.head}"
      else magnitude(value/1024, scale.tail)

    val free = magnitude(runtime.freeMemory)
    val total = magnitude(runtime.totalMemory)
    val used = magnitude(runtime.totalMemory - runtime.freeMemory)
    val max = magnitude(runtime.maxMemory)

    (str"""    CPUs: ${runtime.availableProcessors}
          |  Memory: ${used}B used, ${free}B free, ${total}B total, ${max}B max
          |""").stripMargin
  }

  private val formatter: java.text.DecimalFormat = new java.text.DecimalFormat("0.00")
  private def since(start: Long): String = str"${formatter.format((System.currentTimeMillis - start)/1000.0)}s"

  private def tasksString: String = Lifecycle.sessions.map { session =>
    str"[${session.pid}] started ${since(session.started)} ago: ${session.cli.args.args.mkString(" ")}"
  }.join("\n")

  private def connectionsString: String = BloopServer.workDirectories.map { dir => str"$dir" }.join("\n")

  private def withTemplate(content: String): String = {
    str"""|     _____
          |    / ___/__ __ ____ __ __
          |   / __/ / // // ._// // /
          |  /_/    \_._//_/  _\_. /
          |                   \___/
          |
          |Fury build tool, version ${FuryVersion.current}, built ${FuryVersion.built}
          |This software is provided under the Apache 2.0 License.
          |Fury depends on Bloop, Coursier, Git and Nailgun.
          |© Copyright 2018-20 Jon Pretty, Propensive OÜ.
          |
          |See the Fury website at https://fury.build/, or follow @propensive on Twitter
          |for more information.
          |
          |${content}
          |
          |For help on using Fury, run: fury help
          |""".stripMargin
  }

  def resources: Try[ExitStatus] = cli.call().map { _ =>
    log.raw(withTemplate(resourcesString))
    log.await()
  }

  def tasks: Try[ExitStatus] = cli.call().map{ _ =>
    log.raw(withTemplate(tasksString))
    log.await()
  }

  def connections: Try[ExitStatus] = cli.call().map{ _ =>
    log.raw(withTemplate(connectionsString))
    log.await()
  }

}