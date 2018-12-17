/*
  Fury, version 0.1.2. Copyright 2018 Jon Pretty, Propensive Ltd.

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

import exoskeleton._
import guillotine._

import scala.collection.JavaConverters._

import com.martiansoftware.nailgun.NGContext

object Main {

  def invoke(cli: Cli[CliParam[_]]): ExitStatus = {
    
    val layer = for {
      layout    <- cli.layout
      config    <- fury.Config.read()(cli.env, layout)
      layer     <- Layer.read(layout.furyConfig)(layout)
    } yield layer
    
    val actions = layer.opt.to[List].flatMap { ws => ws.aliases }.map { alias =>
      def action(cli: Cli[CliParam[_]]) = AliasCli.context(cli).flatMap(BuildCli.compile(alias.schema, Some(alias.module)))
      Action(Symbol(alias.cmd.key), msg"${alias.description}", (cli: Cli[CliParam[_]]) => action(cli))
    }
    
    Recovery.recover(cli)(FuryMenu.menu(actions)(cli, cli))
  }

  def main(args: Array[String]): Unit = run(
    System.in, System.out, System.err, args, System.exit(_),
    Environment(System.getenv.asScala.toMap, Option(System.getenv("PWD")))
  )
  
  def nailMain(ctx: NGContext): Unit = run(
    ctx.in, ctx.out, ctx.err, ctx.getArgs, ctx.exit(_),
    Environment(ctx.getEnv.stringPropertyNames.asScala.map { k =>
      (k, ctx.getEnv.getProperty(k))
    }.toMap, Option(ctx.getWorkingDirectory))
  )

  def run(in: java.io.InputStream, out: java.io.PrintStream, err: java.io.PrintStream, args: Seq[String], exit: Int => Unit, env: Environment) =
    exit {
      val cli = Cli(out, ParamMap(args: _*), None, Nil, env)
      val end = invoke(cli).code
      out.flush()
      err.flush()
      end
    }
}
