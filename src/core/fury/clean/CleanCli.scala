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
package fury.clean

import fury._

object CleanCli {
  case class Context(cli: Cli[CliParam[_]], layout: Layout)

  def context(cli: Cli[CliParam[_]]) =
    for {
      layout <- cli.layout
    } yield Context(cli, layout)

  def cleanAll(ctx: Context) =
    for {
      _ <- cleanBloop(ctx)
      _ <- cleanClasses(ctx)
      _ <- cleanRepos(ctx)
      _ <- cleanSources(ctx)
    } yield Done

  def cleanBloop(ctx: Context) =
    for {
      _ <- ctx.layout.bloopDir.delete()
      _ <- ctx.layout.analysisDir.delete()
    } yield Done

  def cleanClasses(ctx: Context) =
    for {
      _ <- ctx.layout.classesDir.delete()
    } yield Done

  def cleanRepos(ctx: Context) =
    for {
      _ <- ctx.layout.reposDir.delete()
    } yield Done

  def cleanSources(ctx: Context) =
    for {
      _ <- ctx.layout.srcsDir.delete()
    } yield Done
}
