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
