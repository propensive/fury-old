/*
  Fury, version 0.4.0. Copyright 2018-19 Jon Pretty, Propensive Ltd.

  The primary distribution site is: https://propensive.com/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required  by applicable  law or  agreed to  in writing,  software  distributed  under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
  express  or  implied.  See  the  License for  the specific  language  governing  permissions and
  limitations under the License.
 */
package fury

import fury.core._, fury.strings._

import com.facebook.nailgun.NGContext
import exoskeleton._
import guillotine._
import scala.collection.JavaConverters._
import scala.collection.mutable.HashSet

import annotation.tailrec
import java.util.concurrent.atomic.AtomicBoolean

import scala.util.Try

object Main {

  private[this] val terminating: AtomicBoolean = new AtomicBoolean(false)
  private[this] val running: HashSet[Thread]   = new HashSet()
  private[this] def busy(): Option[Int] =
    running.synchronized(if (running.size > 1) Some(running.size - 1) else None)

  private[this] def trackThread(action: => Int): Int =
    if (!terminating.get) {
      running.synchronized(running += Thread.currentThread)
      try action
      finally {
        running.synchronized(running -= Thread.currentThread)
      }
    } else {
      println("New tasks cannot be started while Fury is shutting down.")
      1
    }

  def invoke(cli: Cli[CliParam[_]]): ExitStatus = {

    val layer = for {
      layout <- cli.layout
      config <- Config.read()(cli.env, layout)
      layer  <- Layer.read(Io.silent(config), layout.layerFile, layout)
    } yield layer

    val actions = layer.toOption
      .to[List]
      .flatMap { ws =>
        ws.aliases
      }
      .map { alias =>
        def action(cli: Cli[CliParam[_]]) =
          AliasCli.mkContext(cli).flatMap(BuildCli.compile(alias.schema, Some(alias.module)))
        Action(
            Symbol(alias.cmd.key),
            msg"${alias.description}",
            (cli: Cli[CliParam[_]]) => action(cli))
      }

    Recovery.recover(cli)(FuryMenu.menu(actions)(cli, cli))
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
      trackThread {
        val cli = Cli(out, ParamMap(args: _*), None, Nil, env)
        val end = invoke(cli).code
        out.flush()
        err.flush()
        end
      }
    }

  @tailrec
  def shutdown(previous: Int = -1)(cli: Cli[CliParam[_]]): Try[ExitStatus] = {
    terminating.set(true)
    busy() match {
      case None => util.Success(Done)
      case Some(count) =>
        if (previous != count) {
          val plural = if (count > 1) "s" else ""
          println(s"Waiting for $count active task$plural to complete...")
        }
        Thread.sleep(10)
        shutdown(count)(cli)
    }
  }
}
