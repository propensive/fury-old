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

import Args._

import guillotine._
import mitigation._

import scala.concurrent._

object ConfigCli {

  case class Context(cli: Cli[CliParam[_]], layout: Layout, config: Config)

  def context(cli: Cli[CliParam[_]]) = for {
    layout    <- cli.layout
    config    <- ~Config.read()(cli.env, layout).opt.getOrElse(Config())
  } yield new Context(cli, layout, config)

  def set(ctx: Context) = {
    import ctx._
    for {
      cli           <- cli.hint(ThemeArg, Theme.all)
      io            <- cli.io()
      newTheme      <- ~io(ThemeArg).opt
      config        <- ~newTheme.map { th => config.copy(theme = th) }.getOrElse(config)
      _             <- ~io.save(config, layout.userConfig)
    } yield io.await()
  }
}

object AliasCli {

  def context(cli: Cli[CliParam[_]]) = for {
    layout    <- cli.layout
    config    <- Config.read()(cli.env, layout)
    layer     <- Layer.read(layout.furyConfig)(layout)
  } yield new MenuContext(cli, layout, config, layer)


  def list(ctx: MenuContext) = {
    import ctx._
    for {
      cli     <- cli.hint(RawArg)
      io      <- cli.io()
      raw     <- ~io(RawArg).successful
      cols    <- Answer(Terminal.columns.getOrElse(100))
      rows    <- ~layer.aliases.to[List]
      table   <- ~Tables(config).show(Tables(config).aliases, cols, rows, raw)(identity(_))
      _       <- ~(if(!raw) io.println(Tables(config).contextString(layout.pwd, true)))
      _       <- ~io.println(UserMsg { theme => table.mkString("\n") })
    } yield io.await()
  }

  def delete(ctx: MenuContext) = {
    import ctx._
    for {
      cli           <- cli.hint(AliasArg, layer.aliases.map(_.cmd))
      io            <- cli.io()
      aliasArg      <- io(AliasArg)
      aliasToDel    <- ~layer.aliases.find(_.cmd == aliasArg)
      layer         <- Lenses.updateSchemas(None, layer, true) { s =>
                           Lenses.layer.aliases } (_(_) --= aliasToDel)
      _             <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }

  def add(ctx: MenuContext) = {
    import ctx._
    for {
      cli           <- cli.hint(SchemaArg, layer.schemas)
      optSchemaArg  <- ~cli.peek(SchemaArg)
      cli           <- cli.hint(AliasArg)
      cli           <- cli.hint(DescriptionArg)
      optDefaultSchema <- ~optSchemaArg.flatMap(layer.schemas.findBy(_).opt).orElse(layer.mainSchema.opt)
      cli           <- cli.hint(ProjectArg, optDefaultSchema.map(_.projects).getOrElse(Nil))
      optProjectId  <- ~cli.peek(ProjectArg)
      optProject    <- ~optProjectId.orElse(optDefaultSchema.flatMap(_.main)).flatMap(id => optDefaultSchema.flatMap(_.projects.findBy(id).opt)).to[List].headOption
      cli           <- cli.hint(ModuleArg, optProject.map(_.modules).getOrElse(Nil))
      io            <- cli.io()
      moduleArg     <- io(ModuleArg)
      project       <- optProject.ascribe(UnspecifiedProject())
      module        <- project.modules.findBy(moduleArg)
      moduleRef     <- ~module.ref(project)
      aliasArg      <- io(AliasArg)
      description   <- io(DescriptionArg)
      alias         <- ~Alias(aliasArg, description, optSchemaArg, moduleRef)
      layer         <- Lenses.updateSchemas(None, layer, true) { s =>
                           Lenses.layer.aliases } (_(_) += alias)
      _             <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }
}

object BuildCli {
  def context(cli: Cli[CliParam[_]]) = for {
    layout    <- cli.layout
    config    <- Config.read()(cli.env, layout)
    layer     <- Layer.read(layout.furyConfig)(layout)
  } yield new MenuContext(cli, layout, config, layer)

  def notImplemented(cli: Cli[CliParam[_]]): Result[ExitStatus, ~] = Answer(Abort)

  def init(cli: Cli[CliParam[_]]) = {
    for {
      layout        <- cli.layout
      config        <- Config.read()(cli.env, layout)
      cli           <- cli.hint(ForceArg)
      io            <- cli.io()
      force         <- ~io(ForceArg).opt.isDefined
      layer         <- ~Layer.empty()
      _             <- ~io.println("Initializing new build directory: ./.fury")
      _             <- layout.furyConfig.mkParents()
      _             <- ~io.save(layer, layout.furyConfig)
      _             <- cli.shell.git.init(layout.pwd)
      _             <- ~io.println("Initialized new git repository")
      _             <- cli.shell.git.add(layout.pwd, List(layout.furyConfig))
      _             <- ~io.println("Added files to git repository")
      _             <- cli.shell.git.commit(layout.pwd, "Initial commit")
      _             <- ~io.println("Committed files")
    } yield io.await()
  }

  def about(cli: Cli[CliParam[_]]): Result[ExitStatus, ~ | EarlyCompletions] = for {
    io <- cli.io()
    _  <- ~io.println(
            str"""|     _____ 
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
                  |See the Fury website at https://fury.build/, or follow @propensive on Twtter
                  |for more information.
                  |
                  |For help on using Fury, run: fury help
                  |""".stripMargin)
  } yield io.await()

  def undo(cli: Cli[CliParam[_]]): Result[ExitStatus, ~ | FileNotFound | EarlyCompletions] = {
    import cli._
    for {
      layout <- layout
      path   <- ~layout.furyConfig
      io     <- cli.io()
      bak    <- ~path.rename { f => s".$f.bak" }
      _      <- if(bak.exists) Answer(bak.copyTo(path)) else Result.abort(FileNotFound(bak))
    } yield io.await()
  }

  def clean(ctx: MenuContext) = {
    import ctx._
    for {
      cli          <- cli.hint(SchemaArg, layer.schemas)
      schemaArg    <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema       <- layer.schemas.findBy(schemaArg)
      cli          <- cli.hint(ProjectArg, schema.projects)
      optProjectId <- ~cli.peek(ProjectArg).orElse(schema.main)
      optProject   <- ~optProjectId.flatMap(schema.projects.findBy(_).opt)
      cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
      io           <- cli.io()
      project      <- optProject.ascribe(UnspecifiedProject())
      optModuleId  <- ~io(ModuleArg).opt.orElse(project.main)
      optModule    <- ~optModuleId.flatMap(project.modules.findBy(_).opt)
      module       <- optModule.ascribe(UnspecifiedModule())
      schemaTree   <- schema.schemaTree()
      universe     <- schemaTree.universe
      artifact     <- universe.artifact(module.ref(project))
      _            <- Bloop.server(cli)(io)
      _            <- ~universe.clean(module.ref(project))
    } yield io.await()
  }
  
  def compile(optSchema: Option[SchemaId], moduleRef: Option[ModuleRef])(ctx: MenuContext) = {
    import ctx._
    for {
      cli            <- cli.hint(SchemaArg, layer.schemas)
      schemaArg      <- ~cli.peek(SchemaArg).orElse(optSchema).getOrElse(layer.main)
      schema         <- layer.schemas.findBy(schemaArg)
      cli            <- cli.hint(ProjectArg, schema.projects)
      optProjectId   <- ~cli.peek(ProjectArg).orElse(moduleRef.map(_.projectId)).orElse(schema.main)
      optProject     <- ~optProjectId.flatMap(schema.projects.findBy(_).opt)
      cli            <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
      cli            <- cli.hint(WatchArg)
      cli            <- cli.hint(DebugArg, optProject.to[List].flatMap(_.modules).filter(_.kind == Application))
      io             <- cli.io()
      t0             <- Answer(System.currentTimeMillis)
      project        <- optProject.ascribe(UnspecifiedProject())
      optModuleId    <- ~io(ModuleArg).opt.orElse(moduleRef.map(_.moduleId)).orElse(project.main)
      optModule      <- ~optModuleId.flatMap(project.modules.findBy(_).opt)
      module         <- optModule.ascribe(UnspecifiedModule())
      schemaTree     <- schema.schemaTree()
      universe       <- schemaTree.universe
      artifact       <- universe.artifact(module.ref(project))
      artifacts      <- universe.transitiveDependencies(module.ref(project))(cli.shell)
      _              <- Bloop.server(cli)(io)
      compilation    <- universe.compilation(module.ref(project))(cli.shell, layout)
      _              <- ~compilation.checkoutAll()
      _              <- compilation.generateFiles(universe)(layout, cli.env, cli.shell)
      debugStr       <- ~io(DebugArg).opt
      multiplexer    <- ~(new Multiplexer[ModuleRef, CompileEvent](module.ref(project) :: artifacts.map(_.ref).to[List]))
      future         <- ~universe.compile(artifact, multiplexer).apply(module.ref(project))
      _              <- ~Graph.live(cli)(io, compilation.graph.mapValues(_.to[Set]), multiplexer.stream(50, Some(Tick)), Map())(config.theme)
      t1             <- Answer(System.currentTimeMillis - t0)
      _              <- ~io.println(s"Total time: ${if(t1 >= 10000) s"${t1/1000}s" else s"${t1}ms"}\n")
      _              <- ~Thread.sleep(200)
    } yield io.await(Await.result(future, duration.Duration.Inf).success)
  }
 
  def getPrompt(layer: Layer, theme: Theme): Result[String, ~ | ItemNotFound] = for {
    schemaId        <- ~layer.main
    schema          <- layer.schemas.findBy(schemaId)
    schemaPart      <- ~(if(layer.schemas.size == 1) "*" else schemaId.key)
    optProjectId    <- ~schema.main
    optProject      <- ~optProjectId.flatMap(schema.projects.findBy(_).opt)
    projectPart     <- ~optProjectId.map(_.key).getOrElse("-")
    optModuleId     <- ~optProject.flatMap(_.main)
    optModule       <- ~optModuleId.flatMap { mId => optProject.flatMap(_.modules.findBy(mId).opt) }
    modulePart      <- ~optModuleId.map(_.key).getOrElse("-")
  } yield Prompt.zsh(layer, schema, optProject, optModule)(theme)

  def prompt(cli: Cli[CliParam[_]]) = for {
    layout    <- cli.layout
    config    <- Config.read()(cli.env, layout)
    layer     <- ~Layer.read(layout.furyConfig)(layout).opt
    msg       <- layer.map(getPrompt(_, config.theme)).getOrElse(Answer(Prompt.empty(config)(config.theme)))
    io        <- cli.io()
    _         <- ~io.println(msg)
  } yield io.await()
 
  def save(ctx: MenuContext) = {
    import ctx._
    for {
      cli          <- cli.hint(SchemaArg, layer.schemas)
      schemaArg    <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema       <- layer.schemas.findBy(schemaArg)
      cli          <- cli.hint(ProjectArg, schema.projects)
      optProjectId <- ~cli.peek(ProjectArg).orElse(schema.main)
      optProject   <- ~optProjectId.flatMap(schema.projects.findBy(_).opt)
      cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
      cli          <- cli.hint(DirArg)
      io           <- cli.io()
      dir          <- io(DirArg)
      project      <- optProject.ascribe(UnspecifiedProject())
      optModuleId  <- ~io(ModuleArg).opt.orElse(project.main)
      optModule    <- ~optModuleId.flatMap(project.modules.findBy(_).opt)
      module       <- optModule.ascribe(UnspecifiedModule())
      schemaTree   <- schema.schemaTree()
      universe     <- schemaTree.universe
      artifact     <- universe.artifact(module.ref(project))
      _            <- universe.saveJars(cli)(io, module.ref(project), dir in layout.pwd)
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
      optProject   <- ~optProjectId.flatMap(schema.projects.findBy(_).opt)
      cli          <- cli.hint(ModuleArg, optProject.map(_.modules).getOrElse(Nil))
      optModuleId  <- ~cli.peek(ModuleArg).orElse(optProject.flatMap(_.main))
      optModule    <- ~optModuleId.flatMap { arg =>
                        optProject.flatMap(_.modules.findBy(arg).opt)
                      }
      io           <- cli.io()
      module       <- optModule.ascribe(UnspecifiedModule())
      project      <- optProject.ascribe(UnspecifiedProject())
      schemaTree   <- schema.schemaTree()
      universe     <- schemaTree.universe
      artifact     <- universe.artifact(module.ref(project))
      classpath    <- universe.classpath(module.ref(project))
      _            <- ~io.println(classpath.map(_.value).join(":"))
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
      optProject   <- ~optProjectId.flatMap(schema.projects.findBy(_).opt)
      cli          <- cli.hint(ModuleArg, optProject.map(_.modules).getOrElse(Nil))
      io           <- cli.io()
      optModuleId  <- ~io(ModuleArg).opt.orElse(optProject.flatMap(_.main))
      optModule    <- ~optModuleId.flatMap { arg =>
                        optProject.flatMap(_.modules.findBy(arg).opt)
                      }
      module       <- optModule.ascribe(UnspecifiedModule())
      project      <- optProject.ascribe(UnspecifiedProject())
      schemaTree   <- schema.schemaTree()
      universe     <- schemaTree.universe
      artifact     <- universe.artifact(module.ref(project))
      compilation  <- universe.compilation(module.ref(project))
      _            <- ~io.println(compilation.toString)
      _            <- ~Graph.draw(compilation.graph.mapValues(_.to[Set]), true, Map())(config.theme).foreach(io.println(_))
    } yield io.await()
  }
}

object LayerCli {
  
  case class LayerCtx(cli: Cli[CliParam[_]], layout: Layout, config: Config, layer: Layer,
      schema: Schema) {
    implicit def implicitLayout: Layout = layout
    implicit def implicitEnv: Environment = cli.env
    implicit def implicitShell: Shell = cli.shell
  }
  
  def context(cli: Cli[CliParam[_]]) = for {
    layout    <- cli.layout
    config    <- Config.read()(cli.env, layout)
    layer     <- Layer.read(layout.furyConfig)(layout)
    cli       <- cli.hint(SchemaArg, layer.schemas)
    schemaArg <- ~cli.peek(SchemaArg).getOrElse(layer.main)
    schema    <- layer.schemas.findBy(schemaArg)
  } yield LayerCtx(cli, layout, config, layer, schema)

  def projects(ctx: LayerCtx) = {
    import ctx._
    for {
      cols     <- Answer(Terminal.columns.getOrElse(100))
      cli      <- cli.hint(RawArg)
      io       <- cli.io()
      raw      <- ~io(RawArg).successful
      projects <- schema.allProjects
      table    <- ~Tables(config).show(Tables(config).projects(None), cols, projects.distinct, raw)(_.id)
      _        <- ~(if(!raw) io.println(Tables(config).contextString(layout.pwd, layer.showSchema, schema)))
      _        <- ~io.println(table.mkString("\n"))
    } yield io.await()
  }

  def publish(ctx: LayerCtx) = {
    import ctx._
    for {
      suggestedTags <- cli.shell.git.tags(layout.pwd)
      cli           <- cli.hint(TagArg)
      keys          <- cli.shell.gpg.keys()
      cli           <- cli.hint(KeyArg, keys)
      io            <- cli.io()
      tag           <- io(TagArg)
      key           <- io(KeyArg)
      _             <- cli.shell.git.add(layout.pwd, List(layout.furyConfig))
      _             <- cli.shell.git.commit(layout.pwd, s"Tagged version $tag")
      _             <- cli.shell.git.tag(layout.pwd, tag)
      _             <- ~io.println(msg"Comitted tag $tag.")
      _             <- cli.shell.git.push(layout.pwd, all = true)
      _             <- ~io.println(msg"Pushed git repository.")
    } yield io.await()
  }

}

