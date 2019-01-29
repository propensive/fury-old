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

import fury.Args._
import fury.io._
import fury.error._
import guillotine._

import scala.concurrent._
import scala.util._

object ConfigCli {

  case class Context(cli: Cli[CliParam[_]], layout: Layout, config: Config)

  def context(cli: Cli[CliParam[_]]) =
    for {
      layout <- cli.layout
      config <- ~Config.read()(cli.env, layout).toOption.getOrElse(Config())
    } yield new Context(cli, layout, config)

  def set(ctx: Context) = {
    import ctx._
    for {
      cli      <- cli.hint(ThemeArg, Theme.all)
      invoc    <- cli.read()
      io       <- invoc.io()
      newTheme <- ~invoc(ThemeArg).toOption
      config <- ~newTheme.map { th =>
                 config.copy(theme = th)
               }.getOrElse(config)
      _ <- ~io.save(config, layout.userConfig)
    } yield io.await()
  }
}

object AliasCli {

  def context(cli: Cli[CliParam[_]]) =
    for {
      layout <- cli.layout
      config <- Config.read()(cli.env, layout)
      layer  <- Layer.read(Io.silent(config), layout.furyConfig, layout)
    } yield new MenuContext(cli, layout, config, layer)

  def list(ctx: MenuContext) = {
    import ctx._
    for {
      cli   <- cli.hint(RawArg)
      invoc <- cli.read()
      io    <- invoc.io()
      raw   <- ~invoc(RawArg).isSuccess
      cols  <- Success(Terminal.columns.getOrElse(100))
      rows  <- ~layer.aliases.to[List]
      table <- ~Tables(config).show(Tables(config).aliases, cols, rows, raw)(identity(_))
      _     <- ~(if (!raw) io.println(Tables(config).contextString(layout.pwd, true)))
      _ <- ~io.println(UserMsg { theme =>
            table.mkString("\n")
          })
    } yield io.await()
  }

  def remove(ctx: MenuContext) = {
    import ctx._
    for {
      cli        <- cli.hint(AliasArg, layer.aliases.map(_.cmd))
      invoc      <- cli.read()
      io         <- invoc.io()
      aliasArg   <- invoc(AliasArg)
      aliasToDel <- ~layer.aliases.find(_.cmd == aliasArg)
      layer <- Lenses.updateSchemas(None, layer, true) { s =>
                Lenses.layer.aliases
              }(_(_) --= aliasToDel)
      _ <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }

  def add(ctx: MenuContext) = {
    import ctx._
    for {
      cli          <- cli.hint(SchemaArg, layer.schemas)
      optSchemaArg <- ~cli.peek(SchemaArg)
      cli          <- cli.hint(AliasArg)
      cli          <- cli.hint(DescriptionArg)
      optDefaultSchema <- ~optSchemaArg
                           .flatMap(layer.schemas.findBy(_).toOption)
                           .orElse(layer.mainSchema.toOption)
      cli          <- cli.hint(ProjectArg, optDefaultSchema.map(_.projects).getOrElse(Nil))
      optProjectId <- ~cli.peek(ProjectArg)
      optProject <- ~optProjectId
                     .orElse(optDefaultSchema.flatMap(_.main))
                     .flatMap(id => optDefaultSchema.flatMap(_.projects.findBy(id).toOption))
                     .to[List]
                     .headOption
      cli         <- cli.hint(ModuleArg, optProject.map(_.modules).getOrElse(Nil))
      invoc       <- cli.read()
      io          <- invoc.io()
      moduleArg   <- invoc(ModuleArg)
      project     <- optProject.ascribe(UnspecifiedProject())
      module      <- project.modules.findBy(moduleArg)
      moduleRef   <- ~module.ref(project)
      aliasArg    <- invoc(AliasArg)
      description <- invoc(DescriptionArg)
      alias       <- ~Alias(aliasArg, description, optSchemaArg, moduleRef)
      layer <- Lenses.updateSchemas(None, layer, true) { s =>
                Lenses.layer.aliases
              }(_(_) += alias)
      _ <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }
}

object BuildCli {

  def context(cli: Cli[CliParam[_]]) =
    for {
      layout <- cli.layout
      config <- Config.read()(cli.env, layout)
      layer  <- Layer.read(Io.silent(config), layout.furyConfig, layout)
    } yield new MenuContext(cli, layout, config, layer)

  def notImplemented(cli: Cli[CliParam[_]]): Outcome[ExitStatus] = Success(Abort)

  def about(cli: Cli[CliParam[_]]): Outcome[ExitStatus] =
    for {
      invoc <- cli.read()
      io    <- invoc.io()
      _     <- ~io.println(str"""|     _____ 
                             |    / ___/__ __ ____ __ __
                             |   / __/ / // // ._// // /
                             |  /_/    \_._//_/  _\_. /
                             |                   \___/
                             |
                             |Fury build tool for Scala, version ${Version.current}.
                             |This software is provided under the Apache 2.0 License.
                             |Fury depends on Bloop, Coursier, Git and Nailgun.
                             |© Copyright 2018 Jon Pretty, Propensive Ltd.
                             |
                             |See the Fury website at https://fury.build/, or follow @propensive on Twitter
                             |for more information.
                             |
                             |For help on using Fury, run: fury help
                             |""".stripMargin)
    } yield io.await()

  def undo(cli: Cli[CliParam[_]]): Outcome[ExitStatus] = {
    import cli._
    for {
      layout <- layout
      path   <- ~layout.furyConfig
      invoc  <- cli.read()
      io     <- invoc.io()
      bak <- ~path.rename { f =>
              s".$f.bak"
            }
      _ <- if (bak.exists) Success(bak.copyTo(path)) else Failure(FileNotFound(bak))
    } yield io.await()
  }

  def compile(optSchema: Option[SchemaId], moduleRef: Option[ModuleRef])(ctx: MenuContext) = {
    import ctx._
    for {
      cli          <- cli.hint(SchemaArg, layer.schemas)
      schemaArg    <- ~cli.peek(SchemaArg).orElse(optSchema).getOrElse(layer.main)
      schema       <- layer.schemas.findBy(schemaArg)
      cli          <- cli.hint(ProjectArg, schema.projects)
      optProjectId <- ~cli.peek(ProjectArg).orElse(moduleRef.map(_.projectId)).orElse(schema.main)
      optProject   <- ~optProjectId.flatMap(schema.projects.findBy(_).toOption)
      cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
      cli          <- cli.hint(WatchArg)
      cli <- cli
              .hint(DebugArg, optProject.to[List].flatMap(_.modules).filter(_.kind == Application))
      invoc   <- cli.read()
      io      <- invoc.io()
      t0      <- Success(System.currentTimeMillis)
      project <- optProject.ascribe(UnspecifiedProject())
      optModuleId <- ~invoc(ModuleArg).toOption
                      .orElse(moduleRef.map(_.moduleId))
                      .orElse(project.main)
      optModule   <- ~optModuleId.flatMap(project.modules.findBy(_).toOption)
      module      <- optModule.ascribe(UnspecifiedModule())
      hierarchy   <- schema.hierarchy(io, layout.pwd, layout)
      universe    <- hierarchy.universe
      artifact    <- universe.artifact(io, module.ref(project), layout)
      artifacts   <- universe.transitiveDependencies(io, module.ref(project), layout)
      _           <- Bloop.server(layout.shell, io)
      compilation <- universe.compilation(io, module.ref(project), layout)
      _           <- ~compilation.checkoutAll(io, layout)
      _           <- compilation.generateFiles(io, layout)
      debugStr    <- ~invoc(DebugArg).toOption
      multiplexer <- ~(new Multiplexer[ModuleRef, CompileEvent](
                        module.ref(project) :: artifacts.map(_.ref).to[List]))
      future <- ~compilation
                 .compile(io, module.ref(project), multiplexer, Map(), layout)
                 .apply(module.ref(project))
      _ <- ~Graph.live(
              changed = false,
              io,
              compilation.graph.mapValues(_.to[Set]),
              multiplexer.stream(50, Some(Tick)),
              Map())(config.theme)
      t1 <- Success(System.currentTimeMillis - t0)
      _  <- ~io.println(s"Total time: ${if (t1 >= 10000) s"${t1 / 1000}s" else s"${t1}ms"}\n")
    } yield io.await(Await.result(future, duration.Duration.Inf).success)
  }

  def getPrompt(layer: Layer, theme: Theme): Outcome[String] =
    for {
      schemaId     <- ~layer.main
      schema       <- layer.schemas.findBy(schemaId)
      schemaPart   <- ~(if (layer.schemas.size == 1) "*" else schemaId.key)
      optProjectId <- ~schema.main
      optProject   <- ~optProjectId.flatMap(schema.projects.findBy(_).toOption)
      projectPart  <- ~optProjectId.map(_.key).getOrElse("-")
      optModuleId  <- ~optProject.flatMap(_.main)
      optModule <- ~optModuleId.flatMap { mId =>
                    optProject.flatMap(_.modules.findBy(mId).toOption)
                  }
      modulePart <- ~optModuleId.map(_.key).getOrElse("-")
    } yield Prompt.zsh(layer, schema, optProject, optModule)(theme)

  def prompt(cli: Cli[CliParam[_]]) =
    for {
      layout <- cli.layout
      config <- Config.read()(cli.env, layout)
      layer  <- ~Layer.read(Io.silent(config), layout.furyConfig, layout).toOption
      msg <- layer
              .map(getPrompt(_, config.theme))
              .getOrElse(Success(Prompt.empty(config)(config.theme)))
      invoc <- cli.read()
      io    <- invoc.io()
      _     <- ~io.println(msg)
    } yield io.await()

  def save(ctx: MenuContext) = {
    import ctx._
    for {
      cli          <- cli.hint(SchemaArg, layer.schemas)
      schemaArg    <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema       <- layer.schemas.findBy(schemaArg)
      cli          <- cli.hint(ProjectArg, schema.projects)
      optProjectId <- ~cli.peek(ProjectArg).orElse(schema.main)
      optProject   <- ~optProjectId.flatMap(schema.projects.findBy(_).toOption)
      cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
      cli          <- cli.hint(DirArg)
      invoc        <- cli.read()
      io           <- invoc.io()
      dir          <- invoc(DirArg)
      project      <- optProject.ascribe(UnspecifiedProject())
      optModuleId  <- ~invoc(ModuleArg).toOption.orElse(project.main)
      optModule    <- ~optModuleId.flatMap(project.modules.findBy(_).toOption)
      module       <- optModule.ascribe(UnspecifiedModule())
      hierarchy    <- schema.hierarchy(io, layout.pwd, layout)
      universe     <- hierarchy.universe
      compilation  <- universe.compilation(io, module.ref(project), layout)
      _            <- compilation.saveJars(io, module.ref(project), dir in layout.pwd, layout)
    } yield io.await()
  }

  def classpath(ctx: MenuContext) = {
    import ctx._
    for {
      cli          <- cli.hint(SchemaArg, layer.schemas)
      schemaArg    <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema       <- layer.schemas.findBy(schemaArg)
      cli          <- cli.hint(ProjectArg, schema.projects)
      optProjectId <- ~cli.peek(ProjectArg).orElse(schema.main)
      optProject   <- ~optProjectId.flatMap(schema.projects.findBy(_).toOption)
      cli          <- cli.hint(ModuleArg, optProject.map(_.modules).getOrElse(Nil))
      optModuleId  <- ~cli.peek(ModuleArg).orElse(optProject.flatMap(_.main))
      optModule <- ~optModuleId.flatMap { arg =>
                    optProject.flatMap(_.modules.findBy(arg).toOption)
                  }
      invoc       <- cli.read()
      io          <- invoc.io()
      module      <- optModule.ascribe(UnspecifiedModule())
      project     <- optProject.ascribe(UnspecifiedProject())
      hierarchy   <- schema.hierarchy(io, layout.pwd, layout)
      universe    <- hierarchy.universe
      compilation <- universe.compilation(io, module.ref(project), layout)
      classpath   <- ~compilation.classpath(module.ref(project), layout)
      _           <- ~io.println(classpath.map(_.value).join(":"))
    } yield io.await()
  }

  def describe(ctx: MenuContext) = {
    import ctx._
    for {
      cli          <- cli.hint(SchemaArg, layer.schemas)
      schemaArg    <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema       <- layer.schemas.findBy(schemaArg)
      cli          <- cli.hint(ProjectArg, schema.projects)
      optProjectId <- ~cli.peek(ProjectArg).orElse(schema.main)
      optProject   <- ~optProjectId.flatMap(schema.projects.findBy(_).toOption)
      cli          <- cli.hint(ModuleArg, optProject.map(_.modules).getOrElse(Nil))
      invoc        <- cli.read()
      io           <- invoc.io()
      optModuleId  <- ~invoc(ModuleArg).toOption.orElse(optProject.flatMap(_.main))
      optModule <- ~optModuleId.flatMap { arg =>
                    optProject.flatMap(_.modules.findBy(arg).toOption)
                  }
      module      <- optModule.ascribe(UnspecifiedModule())
      project     <- optProject.ascribe(UnspecifiedProject())
      hierarchy   <- schema.hierarchy(io, layout.pwd, layout)
      universe    <- hierarchy.universe
      artifact    <- universe.artifact(io, module.ref(project), layout)
      compilation <- universe.compilation(io, module.ref(project), layout)
      _ <- ~Graph
            .draw(compilation.graph.mapValues(_.to[Set]), true, Map())(config.theme)
            .foreach(io.println(_))
    } yield io.await()
  }
}

object LayerCli {

  case class LayerCtx(
      cli: Cli[CliParam[_]],
      layout: Layout,
      config: Config,
      layer: Layer,
      schema: Schema) {
    implicit def implicitLayout: Layout   = layout
    implicit def implicitEnv: Environment = cli.env
    implicit def implicitShell: Shell     = layout.shell
  }

  def context(cli: Cli[CliParam[_]]) =
    for {
      layout    <- cli.layout
      config    <- Config.read()(cli.env, layout)
      layer     <- Layer.read(Io.silent(config), layout.furyConfig, layout)
      cli       <- cli.hint(SchemaArg, layer.schemas)
      schemaArg <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaArg)
    } yield LayerCtx(cli, layout, config, layer, schema)

  def init(ctx: LayerCtx) = {
    import ctx._
    for {
      cli   <- cli.hint(ForceArg)
      invoc <- cli.read()
      io    <- invoc.io()
      force <- ~invoc(ForceArg).toOption.isDefined
      layer <- ~Layer()
      _     <- layout.furyConfig.mkParents()
      _     <- ~io.save(layer, layout.furyConfig)
      _     <- layout.shell.git.init(layout.pwd)
      _     <- layout.shell.git.add(layout.pwd, List(layout.furyConfig))
      _     <- layout.shell.git.commit(layout.pwd, "Initial commit")
      _     <- ~io.println("Initialized new git repo and committed layer.fury.")
    } yield io.await()
  }

  def projects(ctx: LayerCtx) = {
    import ctx._
    for {
      cols     <- Success(Terminal.columns.getOrElse(100))
      cli      <- cli.hint(RawArg)
      invoc    <- cli.read()
      io       <- invoc.io()
      raw      <- ~invoc(RawArg).isSuccess
      projects <- schema.allProjects(io, layout)
      table <- ~Tables(config).show(Tables(config).projects(None), cols, projects.distinct, raw)(
                  _.id)
      _ <- ~(if (!raw)
               io.println(Tables(config).contextString(layout.pwd, layer.showSchema, schema)))
      _ <- ~io.println(table.mkString("\n"))
    } yield io.await()
  }

  def publish(ctx: LayerCtx) = {
    import ctx._
    for {
      suggestedTags <- layout.shell.git.tags(layout.pwd)
      cli           <- cli.hint(TagArg, suggestedTags)
      invoc         <- cli.read()
      io            <- invoc.io()
      tag           <- invoc(TagArg)
      _             <- layout.shell.git.add(layout.pwd, List(layout.furyConfig))
      _             <- layout.shell.git.commit(layout.pwd, s"Tagged version $tag")
      _             <- layout.shell.git.tag(layout.pwd, tag)
      _             <- ~io.println(msg"Committed tag $tag.")
      _             <- layout.shell.git.push(layout.pwd, all = true)
      _             <- ~io.println(msg"Pushed git repository.")
    } yield io.await()
  }

}
