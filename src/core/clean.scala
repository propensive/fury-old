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
package fury.core

import fury.model._

import scala.util._

object CleanCli {
<<<<<<< HEAD
<<<<<<< HEAD
  def cleanAll(cli: Cli): Try[ExitStatus] = for {
    _ <- cleanBloop(cli)
    _ <- cleanClasses(cli)
    _ <- cleanLogs(cli)
    _ <- cleanRepos(cli)
    _ <- cleanSources(cli)
  } yield Done

  def cleanBloop(cli: Cli): Try[ExitStatus] = for {
    layout <- cli.layout
    _      <- layout.bloopDir.delete()
    _      <- layout.analysisDir.delete()
  } yield Done
=======
  case class Context(cli: Cli[CliParam[_]], layout: Layout)
  def context(cli: Cli[CliParam[_]]) = cli.layout.map(Context(cli, _))
=======
  case class Context(cli: Cli[CliParam], layout: Layout)
  def context(cli: Cli[CliParam]) = cli.layout.map(Context(cli, _))
>>>>>>> parent of 1036380... More verbosity before cleanup
  
  def cleanAll(ctx: Context): Try[ExitStatus] =
    for {
      _ <- cleanBloop(ctx)
      _ <- cleanClasses(ctx)
      _ <- cleanLogs(ctx)
      _ <- cleanRepos(ctx)
      _ <- cleanSources(ctx)
    } yield Done
<<<<<<< HEAD
>>>>>>> parent of 0d77017... Changed `CliParam[T]` to `CliParam { type Type = T }` everywhere (#973)
=======
>>>>>>> parent of 1036380... More verbosity before cleanup

  def cleanBloop(ctx: Context): Try[ExitStatus] =
    for {
      _ <- ctx.layout.bloopDir.delete()
      _ <- ctx.layout.analysisDir.delete()
    } yield Done

  def cleanClasses(ctx: Context): Try[ExitStatus] = ctx.layout.classesDir.delete().map(Done.waive)
  def cleanLogs(ctx: Context): Try[ExitStatus] = ctx.layout.logsDir.delete().map(Done.waive)
  def cleanRepos(ctx: Context): Try[ExitStatus] = Installation.reposDir.delete().map(Done.waive)
  def cleanSources(ctx: Context): Try[ExitStatus] = Installation.srcsDir.delete().map(Done.waive)
}
