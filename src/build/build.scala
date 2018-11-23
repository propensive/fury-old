/*
  Fury, version 0.1.0. Copyright 2018 Jon Pretty, Propensive Ltd.

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

import guillotine._
import mitigation._

import scala.concurrent._

import Args._

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
      io            <- ~io.save(config, layout.userConfig)
    } yield io.await()
  }
}

object AliasCli {

  def context(cli: Cli[CliParam[_]]) = for {
    layout    <- cli.layout
    config    <- Config.read()(cli.env, layout)
    workspace <- Workspace.read(layout.furyConfig)(layout)
  } yield new MenuContext(cli, layout, config, workspace)


  def list(ctx: MenuContext) = {
    import ctx._
    for {
      cli     <- cli.hint(RawArg)
      io      <- cli.io()
      raw     <- ~io(RawArg).successful
      cols    <- Answer(Terminal.columns.getOrElse(100))
      rows    <- ~workspace.aliases.to[List]
      table   <- ~Tables(config).show(Tables(config).aliases, cols, rows, raw)(identity(_))
      io      <- ~(if(!raw) io.println(Tables(config).contextString(layout.pwd, true)) else io)
      io      <- ~io.println(UserMsg { theme => table.mkString("\n") })
    } yield io.await()
  }

  def delete(ctx: MenuContext) = {
    import ctx._
    for {
      cli           <- cli.hint(AliasArg, workspace.aliases.map(_.cmd))
      io            <- cli.io()
      aliasArg      <- io(AliasArg)
      aliasToDel    <- ~workspace.aliases.find(_.cmd == aliasArg)
      workspace     <- Lenses.updateSchemas(None, workspace, true) { s =>
                           Lenses.workspace.aliases } (_(_) --= aliasToDel)
      io            <- ~io.save(workspace, layout.furyConfig)
    } yield io.await()
  }

  def add(ctx: MenuContext) = {
    import ctx._
    for {
      cli           <- cli.hint(SchemaArg, workspace.schemas)
      optSchemaArg  <- ~cli.peek(SchemaArg)
      cli           <- cli.hint(AliasArg)
      cli           <- cli.hint(DescriptionArg)
      optDefaultSchema <- ~optSchemaArg.flatMap(workspace.schemas.findBy(_).opt).orElse(workspace.mainSchema.opt)
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
      workspace     <- Lenses.updateSchemas(None, workspace, true) { s =>
                           Lenses.workspace.aliases } (_(_) += alias)
      io            <- ~io.save(workspace, layout.furyConfig)
    } yield io.await()
  }
}

object BuildCli {
  def context(cli: Cli[CliParam[_]]) = for {
    layout    <- cli.layout
    config    <- Config.read()(cli.env, layout)
    workspace <- Workspace.read(layout.furyConfig)(layout)
  } yield new MenuContext(cli, layout, config, workspace)

  def notImplemented(cli: Cli[CliParam[_]]): Result[ExitStatus, ~] = Answer(Abort)

  def init(cli: Cli[CliParam[_]]) = {
    for {
      layout        <- cli.layout
      config        <- Config.read()(cli.env, layout)
      cli           <- cli.hint(ForceArg)
      io            <- cli.io()
      force         <- ~io(ForceArg).opt.isDefined
      workspace     <- ~Workspace.empty()
      io            <- ~io.println("Initializing new build directory: ./.fury")
      _             <- layout.furyConfig.mkParents()
      io            <- ~io.save(workspace, layout.furyConfig)
      _             <- cli.shell.git.init(layout.pwd)
      io            <- ~io.println("Initialized new git repository")
      _             <- cli.shell.git.add(layout.pwd, List(layout.furyConfig))
      io            <- ~io.println("Added files to git repository")
      _             <- cli.shell.git.commit(layout.pwd, "Initial commit")
      io            <- ~io.println("Committed files")
    } yield io.await()
  }

  def about(cli: Cli[CliParam[_]]): Result[ExitStatus, ~ | EarlyCompletions] = {
    cli.io().map(_.println(
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
            |""".stripMargin).await())
  }

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
      cli          <- cli.hint(SchemaArg, workspace.schemas)
      schemaArg    <- ~cli.peek(SchemaArg).getOrElse(workspace.main)
      schema       <- workspace.schemas.findBy(schemaArg)
      cli          <- cli.hint(ProjectArg, schema.projects)
      optProjectId <- ~cli.peek(ProjectArg).orElse(schema.main)
      optProject   <- ~optProjectId.flatMap(schema.projects.findBy(_).opt)
      cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
      cli          <- cli.hint(RecursiveArg)
      io           <- cli.io()
      project      <- optProject.ascribe(UnspecifiedProject())
      optModuleId  <- ~io(ModuleArg).opt.orElse(project.main)
      optModule    <- ~optModuleId.flatMap(project.modules.findBy(_).opt)
      module       <- optModule.ascribe(UnspecifiedModule())
      artifact     <- schema.artifact(workspace, module.ref(project))
      io           <- Bloop.server(cli)(io)
      recursive    <- ~io(RecursiveArg).opt.isDefined
      io           <- ~io.effect(artifact.clean(recursive))
    } yield io.await()
  }
  
  def compile(optSchema: Option[SchemaId], moduleRef: Option[ModuleRef])(ctx: MenuContext) = {
    import ctx._
    for {
      cli            <- cli.hint(SchemaArg, workspace.schemas)
      schemaArg      <- ~cli.peek(SchemaArg).orElse(optSchema).getOrElse(workspace.main)
      schema         <- workspace.schemas.findBy(schemaArg)
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
      artifact       <- schema.artifact(workspace, module.ref(project))
      artifacts      <- artifact.transitiveDependencies(layout, cli.shell)
      io             <- Bloop.server(cli)(io)
      files          <- ~Bloop.generateFiles(artifacts)(layout, cli.env, cli.shell)
      graph          <- artifact.dependencyGraph(layout, cli.shell)
      debugStr       <- ~io(DebugArg).opt
      io             <- ~io.println(Tables(config).contextString(layout.pwd, workspace.showSchema, schema, project, module))
      allDeps        <- artifact.transitiveDependencies
      multiplexer    <- ~(new Multiplexer[ModuleRef, CompileEvent](allDeps.map(_.ref).to[List]))
      future         <- ~artifact.compile(multiplexer).apply(module.ref(project))
      io             <- ~Graph.live(cli)(io, graph, multiplexer.stream(50, Some(Tick)), Map())(config.theme)
      t1             <- Answer(System.currentTimeMillis - t0)
      io             <- ~io.println(s"Total time: ${if(t1 >= 10000) s"${t1/1000}s" else s"${t1}ms"}\n")
      io             <- ~io.effect(Thread.sleep(150))
    } yield io.await(Await.result(future, duration.Duration.Inf).success)
  }
 
  def getPrompt(workspace: Workspace, theme: Theme): Result[String, ~ | ItemNotFound] = for {
    schemaId        <- ~workspace.main
    schema          <- workspace.schemas.findBy(schemaId)
    schemaPart      <- ~(if(workspace.schemas.size == 1) "*" else schemaId.key)
    optProjectId    <- ~schema.main
    optProject      <- ~optProjectId.flatMap(schema.projects.findBy(_).opt)
    projectPart     <- ~optProjectId.map(_.key).getOrElse("-")
    optModuleId     <- ~optProject.flatMap(_.main)
    optModule       <- ~optModuleId.flatMap { mId => optProject.flatMap(_.modules.findBy(mId).opt) }
    modulePart      <- ~optModuleId.map(_.key).getOrElse("-")
  } yield Prompt.zsh(workspace, schema, optProject, optModule)(theme)

  def prompt(cli: Cli[CliParam[_]]) = for {
    layout    <- cli.layout
    config    <- Config.read()(cli.env, layout)
    workspace <- ~Workspace.read(layout.furyConfig)(layout).opt
    msg       <- workspace.map(getPrompt(_, config.theme)).getOrElse(Answer(Prompt.empty(config)(config.theme)))
    io        <- cli.io()
    io        <- ~io.println(msg)
  } yield io.await()
 
  def save(ctx: MenuContext) = {
    import ctx._
    for {
      cli          <- cli.hint(SchemaArg, workspace.schemas)
      schemaArg    <- ~cli.peek(SchemaArg).getOrElse(workspace.main)
      schema       <- workspace.schemas.findBy(schemaArg)
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
      artifact     <- schema.artifact(workspace, module.ref(project))
      io           <- artifact.saveJars(cli)(io, dir in layout.pwd)
    } yield io.await()
  }
  
  def classpath(ctx: MenuContext) = {
    import ctx._
    for {
      cli          <- cli.hint(SchemaArg, workspace.schemas)
      schemaArg    <- ~cli.peek(SchemaArg).getOrElse(workspace.main)
      schema       <- workspace.schemas.findBy(schemaArg)
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
      resolved     <- schema.resolve(project.id)(layout, cli.shell).ascribe(ItemNotFound(project.id))
      artifact     <- resolved.artifact(workspace, module.id)
      classpath    <- artifact.classpath
      io           <- ~io.println(classpath.map(_.value).join(":"))
    } yield io.await()
  }
  
  def describe(ctx: MenuContext) = {
    import ctx._
    for {
      cli          <- cli.hint(SchemaArg, workspace.schemas)
      schemaArg    <- ~cli.peek(SchemaArg).getOrElse(workspace.main)
      schema       <- workspace.schemas.findBy(schemaArg)
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
      resolved     <- schema.resolve(project.id)(layout, cli.shell).ascribe(ItemNotFound(project.id))
      artifact     <- resolved.artifact(workspace, module.id)
      graph        <- artifact.dependencyGraph(layout, cli.shell)
      io           <- ~Graph.draw(graph, true, Map())(config.theme).foldLeft(io)(_.println(_))
    } yield io.await()
  }
}

object WorkspaceCli {
  
  case class WorkspaceCtx(cli: Cli[CliParam[_]], layout: Layout, config: Config, workspace: Workspace,
      schema: Schema) {
    implicit def implicitLayout: Layout = layout
    implicit def implicitEnv: Environment = cli.env
    implicit def implicitShell: Shell = cli.shell
  }
  
  def context(cli: Cli[CliParam[_]]) = for {
    layout    <- cli.layout
    config    <- Config.read()(cli.env, layout)
    workspace <- Workspace.read(layout.furyConfig)(layout)
    cli       <- cli.hint(SchemaArg, workspace.schemas)
    schemaArg <- ~cli.peek(SchemaArg).getOrElse(workspace.main)
    schema    <- workspace.schemas.findBy(schemaArg)
  } yield WorkspaceCtx(cli, layout, config, workspace, schema)

  def projects(ctx: WorkspaceCtx) = {
    import ctx._
    for {
      cols     <- Answer(Terminal.columns.getOrElse(100))
      cli      <- cli.hint(RawArg)
      io       <- cli.io()
      raw      <- ~io(RawArg).successful
      projects <- schema.allProjects
      table    <- ~Tables(config).show(Tables(config).projects(None), cols, projects.distinct, raw)(_.id)
      io       <- ~(if(!raw) io.println(Tables(config).contextString(layout.pwd, workspace.showSchema, schema)) else io)
      io       <- ~io.println(table.mkString("\n"))
    } yield io.await()
  }

  def publish(ctx: WorkspaceCtx) = {
    import ctx._
    for {
      suggestedTags <- cli.shell.git.tags(layout.pwd)
      cli           <- cli.hint(TagArg, GitTag.suggested(suggestedTags))
      keys          <- cli.shell.gpg.keys()
      cli           <- cli.hint(KeyArg, keys)
      io            <- cli.io()
      tag           <- io(TagArg)
      key           <- io(KeyArg)
      _             <- layout.signedConfig.delete()
      _             <- cli.shell.gpg.sign(layout.furyConfig, layout.signedConfig, key)
      _             <- cli.shell.git.add(layout.pwd,
                           List(layout.signedConfig, layout.furyConfig))
      _             <- cli.shell.git.commit(layout.pwd, s"Tagged version $tag")
      _             <- cli.shell.git.tag(layout.pwd, tag)
      io            <- ~io.println(msg"Comitted tag $tag.")
      _             <- cli.shell.git.push(layout.pwd, all = true)
      io            <- ~io.println(msg"Pushed git repository.")
    } yield io.await()
  }

}

