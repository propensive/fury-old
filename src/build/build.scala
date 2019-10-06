/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.6.7. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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
package fury

import fury.strings._, fury.core._, fury.ogdl._, fury.model._, fury.io._

import Args._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.util._
import java.text.DecimalFormat

import fury.utils.Multiplexer

import language.higherKinds

object ConfigCli {
  case class Context(cli: Cli[CliParam[_]], config: Config)

  def context(cli: Cli[CliParam[_]]): Try[Context] = Try(new Context(cli, cli.config))

  def set(ctx: Context): Try[ExitStatus] = {
    import ctx._
    for {
      cli      <- cli.hint(ThemeArg, Theme.all)
      cli      <- cli.hint(TimestampsArg, List("on", "off"))
      cli      <- cli.hint(PipeliningArg, List("on", "off"))
      invoc    <- cli.read()
      io       <- invoc.io()
      newTheme <- ~invoc(ThemeArg).toOption
      timestamps <- ~invoc(TimestampsArg).toOption
      pipelining <- ~invoc(PipeliningArg).toOption
      config   <- ~newTheme.map { th => config.copy(theme = th) }.getOrElse(config)
      config   <- ~timestamps.map { ts => config.copy(timestamps = ts) }.getOrElse(config)
      config   <- ~pipelining.map { p => config.copy(pipelining = p) }.getOrElse(config)
      _        <- ~Ogdl.write(config, cli.globalLayout.userConfig)
    } yield io.await()
  }
}

object AliasCli {
  def context(cli: Cli[CliParam[_]]) =
    for {
      layout <- cli.layout
      config <- ~cli.config
      layer  <- Layer.read(Io.silent(config), layout, cli.globalLayout)
    } yield new MenuContext(cli, layout, config, layer)

  def list(ctx: MenuContext): Try[ExitStatus] = {
    import ctx._
    for {
      cli   <- cli.hint(RawArg)
      invoc <- cli.read()
      io    <- invoc.io()
      raw   <- ~invoc(RawArg).isSuccess
      rows  <- ~layer.aliases.to[List]
      table <- ~Tables(config).show(Tables(config).aliases, cli.cols, rows, raw)(identity(_))
      _     <- ~(if(!raw) io.println(Tables(config).contextString(layout.base, true), noTime = true))
      _     <- ~io.println(UserMsg { theme => table.mkString("\n") })
    } yield io.await()
  }

  def remove(ctx: MenuContext): Try[ExitStatus] = {
    import ctx._
    for {
      cli        <- cli.hint(AliasArg, layer.aliases.map(_.cmd))
      invoc      <- cli.read()
      io         <- invoc.io()
      aliasArg   <- invoc(AliasArg)
      aliasToDel <- ~layer.aliases.find(_.cmd == aliasArg)
      layer      <- Lenses.updateSchemas(None, layer, true) { s => Lenses.layer.aliases } (_(_) --= aliasToDel)
      _          <- ~Layer.save(io, layer, layout, cli.globalLayout)
    } yield io.await()
  }

  def add(ctx: MenuContext): Try[ExitStatus] = {
    import ctx._
    for {
      cli              <- cli.hint(SchemaArg, layer.schemas)
      optSchemaArg     <- ~cli.peek(SchemaArg)
      cli              <- cli.hint(AliasArg)
      cli              <- cli.hint(DescriptionArg)

      optDefaultSchema <- ~optSchemaArg.flatMap(layer.schemas.findBy(_).toOption).orElse(
                            layer.mainSchema.toOption)
      
      cli              <- cli.hint(ProjectArg, optDefaultSchema.map(_.projects).getOrElse(Nil))
      optProjectId     <- ~cli.peek(ProjectArg)
      
      optProject       <- ~optProjectId.orElse(optDefaultSchema.flatMap(_.main)).flatMap { id =>
                              optDefaultSchema.flatMap(_.projects.findBy(id).toOption) }.to[List].headOption
      
      cli              <- cli.hint(ModuleArg, optProject.map(_.modules).getOrElse(Nil))
      invoc            <- cli.read()
      io               <- invoc.io()
      moduleArg        <- invoc(ModuleArg)
      project          <- optProject.ascribe(UnspecifiedProject())
      module           <- project.modules.findBy(moduleArg)
      moduleRef        <- ~module.ref(project)
      aliasArg         <- invoc(AliasArg)
      description      <- invoc(DescriptionArg)
      alias            <- ~Alias(aliasArg, description, optSchemaArg, moduleRef)
      layer            <- Lenses.updateSchemas(None, layer, true) { s => Lenses.layer.aliases } (_(_) += alias)
      _                <- ~Layer.save(io, layer, layout, cli.globalLayout)
    } yield io.await()
  }
}

object BuildCli {

  def context(cli: Cli[CliParam[_]]): Try[MenuContext] = for {
    layout <- cli.layout
    config <- ~cli.config
    layer  <- Layer.read(Io.silent(config), layout, cli.globalLayout)
  } yield new MenuContext(cli, layout, config, layer)

  def notImplemented(cli: Cli[CliParam[_]]): Try[ExitStatus] = Success(Abort)

  def status(busyCount: Int): String = {
    val runtime = Runtime.getRuntime
    val df: DecimalFormat = new DecimalFormat("0.0")

    def magnitude(value: Double, scale: List[String] = List("", "k", "M", "G", "T")): String =
      if(value < 1024) s"${df.format(value)}${scale.head}"
      else magnitude(value/1024, scale.tail)

    val free = magnitude(runtime.freeMemory)
    val total = magnitude(runtime.totalMemory)
    val used = magnitude(runtime.totalMemory - runtime.freeMemory)
    val max = magnitude(runtime.maxMemory)

    str"""    CPUs: ${runtime.availableProcessors}
         |  Memory: ${used}B used, ${free}B free, ${total}B total, ${max}B max
         |     BSP: ${Compilation.bspPool.size} connections
         |  Builds: ${busyCount} active""".stripMargin
  }

  def about(busyCount: Int)(cli: Cli[CliParam[_]]): Try[ExitStatus] =
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
                                 |© Copyright 2018-19 Jon Pretty, Propensive OÜ.
                                 |
                                 |See the Fury website at https://fury.build/, or follow @propensive on Twitter
                                 |for more information.
                                 |
                                 |${status(busyCount)}
                                 |
                                 |For help on using Fury, run: fury help
                                 |""".stripMargin, noTime = true)
    } yield io.await()

  /*def undo(cli: Cli[CliParam[_]]): Try[ExitStatus] = {
    import cli._
    for {
      layout          <- layout
      layerRepository  = LayerRepository(layout)
      invoc           <- cli.read()
      io              <- invoc.io()
      _               <- layerRepository.restorePrevious(io, layout)
    } yield Done
  }*/

  def compile(optSchema: Option[SchemaId], moduleRef: Option[ModuleRef])
             (ctx: MenuContext)
             : Try[ExitStatus] = {
    import ctx._
    for {
      cli          <- cli.hint(SchemaArg, layer.schemas)
      cli          <- cli.hint(HttpsArg)
      schemaArg    <- ~cli.peek(SchemaArg).orElse(optSchema).getOrElse(layer.main)
      schema       <- layer.schemas.findBy(schemaArg)
      cli          <- cli.hint(ProjectArg, schema.projects)
      cli          <- cli.hint(PipeliningArg, List("on", "off"))
      optProjectId <- ~cli.peek(ProjectArg).orElse(moduleRef.map(_.projectId)).orElse(schema.main)
      optProject   <- ~optProjectId.flatMap(schema.projects.findBy(_).toOption)
      cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
      cli          <- cli.hint(WatchArg)
      cli          <- cli.hint(ReporterArg, Reporter.all)
      cli          <- cli.hint(DebugArg, optProject.to[List].flatMap(_.modules).filter(_.kind == Application))
      invoc        <- cli.read()
      io           <- invoc.io()
      project      <- optProject.ascribe(UnspecifiedProject())
      optModuleId  <- ~invoc(ModuleArg).toOption.orElse(moduleRef.map(_.moduleId)).orElse(project.main)
      optModule    <- ~optModuleId.flatMap(project.modules.findBy(_).toOption)
      https        <- ~invoc(HttpsArg).isSuccess
      module       <- optModule.ascribe(UnspecifiedModule())
      pipelining   <- ~invoc(PipeliningArg).toOption
      globalPolicy <- Policy.read(io, cli.globalLayout)
      reporter     =  invoc(ReporterArg).toOption.getOrElse(GraphReporter)
      watch        =  invoc(WatchArg).isSuccess
      compilation  <- Compilation.syncCompilation(io, schema, module.ref(project), layout, cli.globalLayout, https)
      watcher      =  new SourceWatcher(compilation.allSources)
      //_            =  watcher.directories.map(_.toString).foreach(s => io.println(str"$s"))
      _            =  if(watch) watcher.start()
      future       <- new Repeater[Try[Future[CompileResult]]] {
        var cnt: Int = 0
        override def repeatCondition(): Boolean = watcher.hasChanges

        override def stopCondition(): Boolean = !watch

        override def action(): Try[Future[CompileResult]] = {
          //io.println(str"Rebuild $cnt")
          cnt = cnt + 1
          watcher.clear()
          compileOnce(io, compilation, schema, module.ref(project), layout,
            globalPolicy, invoc.suffix, pipelining.getOrElse(config.pipelining),reporter, config.theme, https)
        }
      }.start()
      
    } yield {
      val result = Await.result(future, duration.Duration.Inf)
      watcher.stop
      io.await(result.isSuccessful)
    }
  }

  def getPrompt(layer: Layer, theme: Theme): Try[String] = for {
    schemaId     <- ~layer.main
    schema       <- layer.schemas.findBy(schemaId)
    optProjectId <- ~schema.main
    optProject   <- ~optProjectId.flatMap(schema.projects.findBy(_).toOption)
    optModuleId  <- ~optProject.flatMap(_.main)
    optModule    <- ~optModuleId.flatMap { mId => optProject.flatMap(_.modules.findBy(mId).toOption) }
  } yield Prompt.zsh(layer, schema, optProject, optModule)(theme)

  def prompt(cli: Cli[CliParam[_]]): Try[ExitStatus] = for {
    layout <- cli.layout
    config <- ~cli.config
    layer  <- ~Layer.read(Io.silent(config), layout, cli.globalLayout).toOption
    msg    <- layer.fold(Try(Prompt.empty(config)(config.theme)))(getPrompt(_, config.theme))
    invoc  <- cli.read()
    io     <- invoc.io()
    _      <- ~io.println(msg)
  } yield io.await()

  def save(ctx: MenuContext): Try[ExitStatus] = {
    import ctx._
    for {
      cli            <- cli.hint(SchemaArg, layer.schemas)
      cli            <- cli.hint(HttpsArg)
      schemaArg      <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema         <- layer.schemas.findBy(schemaArg)
      cli            <- cli.hint(ProjectArg, schema.projects)
      optProjectId   <- ~cli.peek(ProjectArg).orElse(schema.main)
      cli            <- cli.hint(PipeliningArg, List("on", "off"))
      optProject     <- ~optProjectId.flatMap(schema.projects.findBy(_).toOption)
      cli            <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
      cli            <- cli.hint(DirArg)
      cli            <- cli.hint(FatJarArg)
      cli            <- cli.hint(ReporterArg, Reporter.all)
      invoc          <- cli.read()
      io             <- invoc.io()
      dir            <- invoc(DirArg)
      https          <- ~invoc(HttpsArg).isSuccess
      project        <- optProject.ascribe(UnspecifiedProject())
      optModuleId    <- ~invoc(ModuleArg).toOption.orElse(project.main)
      optModule      <- ~optModuleId.flatMap(project.modules.findBy(_).toOption)
      module         <- optModule.ascribe(UnspecifiedModule())
      pipelining     <- ~invoc(PipeliningArg).toOption
      fatJar         =  invoc(FatJarArg).isSuccess
      globalPolicy   <- Policy.read(io, cli.globalLayout)
      reporter       <- ~invoc(ReporterArg).toOption.getOrElse(GraphReporter)
      watch          =  invoc(WatchArg).isSuccess
      compilation    <- Compilation.syncCompilation(io, schema, module.ref(project), layout, cli.globalLayout, https)
      watcher        =  new SourceWatcher(compilation.allSources)
      _              =  if(watch) watcher.start()
      future         <- new Repeater[Try[Future[CompileResult]]] {
        var cnt: Int = 0
        override def repeatCondition(): Boolean = watcher.hasChanges

        override def stopCondition(): Boolean = !watch

        override def action(): Try[Future[CompileResult]] = {
          //io.println(str"Rebuild $cnt")
          cnt = cnt + 1
          watcher.clear()
          for{
            task <- compileOnce(io, compilation, schema, module.ref(project), layout,
              globalPolicy, invoc.suffix, pipelining.getOrElse(config.pipelining), reporter, config.theme, https)
          } yield {
            task.transform{ completed =>
              for{
                compileResult  <- completed
                compileSuccess <- compileResult.asTry
                _              <- compilation.saveJars(io, module.ref(project), compileSuccess.classDirectories,
                  dir in layout.pwd, layout, fatJar)
              } yield compileSuccess
            }
          }
        }
      }.start()
    } yield {
      val result = Await.result(future, duration.Duration.Inf)
      watcher.stop
      io.await(result.isSuccessful)
    }
  }

  def native(ctx: MenuContext) = {
    import ctx._
    for {
      cli          <- cli.hint(SchemaArg, layer.schemas)
      cli          <- cli.hint(HttpsArg)
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
      https        <- ~invoc(HttpsArg).isSuccess
      project      <- optProject.ascribe(UnspecifiedProject())
      optModuleId  <- ~invoc(ModuleArg).toOption.orElse(project.main)
      optModule    <- ~optModuleId.flatMap(project.modules.findBy(_).toOption)
      module       <- optModule.ascribe(UnspecifiedModule())
      
      compilation  <- Compilation.syncCompilation(io, schema, module.ref(project), layout, cli.globalLayout,
                          https)
      
      _            <- if(module.kind == Application) Success(()) else Failure(InvalidKind(Application))
      main         <- module.main.ascribe(UnspecifiedMain(module.id))
      _            <- compilation.saveNative(io, module.ref(project), dir in layout.pwd, layout, main)
    } yield io.await()
  }

  def classpath(ctx: MenuContext): Try[ExitStatus] = {
    import ctx._
    for {
      cli          <- cli.hint(SchemaArg, layer.schemas)
      cli          <- cli.hint(HttpsArg)
      schemaArg    <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema       <- layer.schemas.findBy(schemaArg)
      cli          <- cli.hint(ProjectArg, schema.projects)
      optProjectId <- ~cli.peek(ProjectArg).orElse(schema.main)
      optProject   <- ~optProjectId.flatMap(schema.projects.findBy(_).toOption)
      cli          <- cli.hint(ModuleArg, optProject.map(_.modules).getOrElse(Nil))
      optModuleId  <- ~cli.peek(ModuleArg).orElse(optProject.flatMap(_.main))
      optModule    <- ~optModuleId.flatMap { arg => optProject.flatMap(_.modules.findBy(arg).toOption) }
      invoc        <- cli.read()
      io           <- invoc.io()
      https        <- ~invoc(HttpsArg).isSuccess
      project      <- optProject.ascribe(UnspecifiedProject())
      module       <- optModule.ascribe(UnspecifiedModule())
      
      compilation  <- Compilation.syncCompilation(io, schema, module.ref(project), layout, cli.globalLayout,
                          https)
      
      classpath    <- ~compilation.classpath(module.ref(project), layout)
      _            <- ~io.println(classpath.map(_.value).join(":"), noTime = true)
    } yield io.await()
  }

  def describe(ctx: MenuContext): Try[ExitStatus] = {
    import ctx._
    for {
      cli          <- cli.hint(SchemaArg, layer.schemas)
      cli          <- cli.hint(HttpsArg)
      schemaArg    <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema       <- layer.schemas.findBy(schemaArg)
      cli          <- cli.hint(ProjectArg, schema.projects)
      optProjectId <- ~cli.peek(ProjectArg).orElse(schema.main)
      optProject   <- ~optProjectId.flatMap(schema.projects.findBy(_).toOption)
      cli          <- cli.hint(ModuleArg, optProject.map(_.modules).getOrElse(Nil))
      invoc        <- cli.read()
      io           <- invoc.io()
      https        <- ~invoc(HttpsArg).isSuccess
      optModuleId  <- ~invoc(ModuleArg).toOption.orElse(optProject.flatMap(_.main))
      optModule    <- ~optModuleId.flatMap { arg => optProject.flatMap(_.modules.findBy(arg).toOption) }
      project      <- optProject.ascribe(UnspecifiedProject())
      module       <- optModule.ascribe(UnspecifiedModule())

      compilation  <- Compilation.syncCompilation(io, schema, module.ref(project), layout, cli.globalLayout,
                          https)
      
      _            <- ~Graph.draw(compilation.graph.map { case (k, v) => (k.ref, v.map(_.ref).to[Set]) }, true,
                          Map())(config.theme).foreach(io.println(_, noTime = true))

    } yield io.await()
  }

  private[this] def compileOnce(io: Io,
                  compilation: Compilation,
                  schema: Schema,
                  moduleRef: ModuleRef,
                  layout: Layout,
                  globalPolicy: Policy,
                  compileArgs: List[String],
                  pipelining: Boolean,
                  reporter: Reporter,
                  theme: Theme,
                  https: Boolean): Try[Future[CompileResult]] = {
    for {
      _            <- compilation.checkoutAll(io, layout, https)
      _            <- compilation.generateFiles(io, layout)
    } yield {
      val multiplexer = new Multiplexer[ModuleRef, CompileEvent](compilation.targets.map(_._1).to[List])
      val future = compilation.compile(io, moduleRef, multiplexer, Map(), layout,
        globalPolicy, compileArgs, pipelining).apply(TargetId(schema.id, moduleRef)).andThen {
        case compRes =>
          multiplexer.closeAll()
          compRes
      }
      reporter.report(io, compilation, theme, multiplexer)
      future
    }
  }

}

object LayerCli {
  def init(cli: Cli[CliParam[_]]): Try[ExitStatus] = for {
    layout <- cli.newLayout
    cli    <- cli.hint(ForceArg)
    invoc  <- cli.read()
    io     <- invoc.io()
    force  =  invoc(ForceArg).isSuccess
    _      <- if (layout.focusFile.exists && !force) Failure(AlreadyInitialized()) else ~()
    _      <- layout.focusFile.mkParents()
    _      <- Layer.create(io, Layer(), layout, cli.globalLayout)
    _      <- ~io.println(str"Created an empty layer with focus at ${layout.focusFile.value}")
  } yield io.await()

  def projects(cli: Cli[CliParam[_]]): Try[ExitStatus] = for {
    layout    <- cli.layout
    config    <- ~cli.config
    layer     <- Layer.read(Io.silent(config), layout, cli.globalLayout)
    cli       <- cli.hint(SchemaArg, layer.schemas)
    cli       <- cli.hint(HttpsArg)
    schemaArg <- ~cli.peek(SchemaArg).getOrElse(layer.main)
    schema    <- layer.schemas.findBy(schemaArg)
    cli       <- cli.hint(RawArg)
    invoc     <- cli.read()
    io        <- invoc.io()
    raw       <- ~invoc(RawArg).isSuccess
    https     <- ~invoc(HttpsArg).isSuccess
    projects  <- schema.allProjects(io, layout, cli.globalLayout, https)
    table     <- ~Tables(config).show(Tables(config).projects(None), cli.cols, projects.distinct, raw)(_.id)
    _         <- ~(if(!raw) io.println(Tables(config).contextString(layout.base, layer.showSchema, schema), noTime = true))
    _         <- ~io.println(table.mkString("\n"), noTime = true)
  } yield io.await()

  def select(cli: Cli[CliParam[_]]): Try[ExitStatus] = for {
    layout    <- cli.layout
    config    <- ~cli.config
    baseLayer <- Layer.base(Io.silent(config), layout, cli.globalLayout)
    schema    <- baseLayer.mainSchema
    cli       <- cli.hint(LayerArg, schema.importTree(Io.silent(config), layout, cli.globalLayout, true).getOrElse(Nil))
    invoc     <- cli.read()
    io        <- invoc.io()
    _         <- schema.importTree(io, layout, cli.globalLayout, true)
    newPath   <- invoc(LayerArg)
    focus     <- Layer.readFocus(io, layout)
    newFocus  <- ~focus.copy(path = newPath)
    _         <- Layer.saveFocus(io, newFocus, layout)
  } yield io.await()
  
  def clone(cli: Cli[CliParam[_]]): Try[ExitStatus] = for {
    cli           <- cli.hint(DirArg)
    cli           <- cli.hint(ImportArg, Layer.pathCompletions(Io.silent(cli.config), cli.config.service, cli.env, cli.globalLayout).getOrElse(Nil))
    invoc         <- cli.read()
    io            <- invoc.io()
    layerImport   <- invoc(ImportArg)
    followable    <- Try(Layer.follow(layerImport, cli.config).get)
    layerRef      <- Layer.resolve(io, followable, cli.env, cli.globalLayout)
    dir           <- invoc(DirArg)
    pwd           <- cli.pwd
    dir           <- ~Path(pwd).resolve(dir)
    _             <- ~dir.mkdir()
    _             <- Layer.saveFocus(io, Focus(layerRef, ImportPath.Root), dir / ".focus.fury")
  } yield io.await()

  def share(cli: Cli[CliParam[_]]): Try[ExitStatus] = for {
    layout        <- cli.layout
    layer         <- Layer.read(Io.silent(cli.config), layout, cli.globalLayout)
    invoc         <- cli.read()
    io            <- invoc.io()
    ref           <- Layer.share(io, layer, cli.env, cli.globalLayout)
    _             <- ~io.println(str"fury://${ref.key}")
  } yield io.await()

  def addImport(cli: Cli[CliParam[_]]): Try[ExitStatus] = {
    for {
      layout        <- cli.layout
      layer         <- Layer.read(Io.silent(cli.config), layout, cli.globalLayout)
      cli           <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      cli           <- cli.hint(ImportNameArg)
      schemaArg     <- ~cli.peek(SchemaArg)
      defaultSchema <- ~layer.schemas.findBy(schemaArg.getOrElse(layer.main)).toOption
     
      cli           <- cli.hint(ImportArg, Layer.pathCompletions(Io.silent(cli.config), cli.config.service, cli.env, cli.globalLayout).getOrElse(Nil))
      layerImport   <- ~cli.peek(ImportArg)
      followable    <- ~layerImport.flatMap(Layer.follow(_, cli.config))
      layerRef      <- ~followable.flatMap(Layer.resolve(Io.silent(cli.config), _, cli.env, cli.globalLayout).toOption)
      maybeLayer    <- ~layerRef.flatMap(Layer.read(Io.silent(cli.config), _, layout, cli.globalLayout).toOption)
      cli           <- cli.hint(ImportSchemaArg, maybeLayer.map(_.schemas.map(_.id)).getOrElse(Nil))

      invoc         <- cli.read()
      io            <- invoc.io()
      layerRef      <- ~followable.flatMap(Layer.resolve(io, _, cli.env, cli.globalLayout).toOption)
      maybeLayer    <- ~layerRef.flatMap(Layer.read(io, _, layout, cli.globalLayout).toOption)
      layerImport   <- invoc(ImportArg)
      nameArg       <- invoc(ImportNameArg)
      schemaId      <- invoc(ImportSchemaArg)
      layerRef      <- layerRef.ascribe(UnspecifiedLayer())
      schemaRef     <- ~SchemaRef(nameArg, layerRef, schemaId, followable)
      layer         <- Lenses.updateSchemas(schemaArg, layer, true)(Lenses.layer.imports(_))(_.modify(_)(_ +
                           schemaRef.copy(id = nameArg)))
      
      _             <- ~Layer.save(io, layer, layout, cli.globalLayout)
    } yield io.await()
  }

  def unimport(cli: Cli[CliParam[_]]): Try[ExitStatus] = {
    for {
      layout    <- cli.layout
      layer     <- Layer.read(Io.silent(cli.config), layout, cli.globalLayout)
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      schemaArg <- ~cli.peek(SchemaArg)
      dSchema   <- ~layer.schemas.findBy(schemaArg.getOrElse(layer.main)).toOption
      cli       <- cli.hint(ImportIdArg, dSchema.map(_.imports.map(_.id)).getOrElse(Nil))
      invoc     <- cli.read()
      io        <- invoc.io()
      schemaId  <- ~invoc(SchemaArg).toOption.getOrElse(layer.main)
      importArg <- invoc(ImportIdArg)
      schema    <- layer.schemas.findBy(schemaId)
      lens      <- ~Lenses.layer.imports(schema.id)
      layer     <- ~lens.modify(layer)(_.filterNot(_.id == importArg))
      _         <- ~Layer.save(io, layer, layout, cli.globalLayout)
    } yield io.await()
  }

  def list(cli: Cli[CliParam[_]]): Try[ExitStatus] = {
    for {
      layout    <- cli.layout
      layer     <- Layer.read(Io.silent(cli.config), layout, cli.globalLayout)
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      cli       <- cli.hint(HttpsArg)
      schemaArg <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaArg)
      cli       <- cli.hint(RawArg)
      invoc     <- cli.read()
      io        <- invoc.io()
      raw       <- ~invoc(RawArg).isSuccess
      https     <- ~invoc(HttpsArg).isSuccess
      rows      <- ~schema.imports.to[List].map { i => (i, schema.resolve(i, io, layout, cli.globalLayout, https)) }
      
      table     <- ~Tables(cli.config).show(Tables(cli.config).imports(Some(layer.main)), cli.cols, rows,
                       raw)(_._1.schema.key)
      
      _         <- ~(if(!raw) io.println(Tables(cli.config).contextString(layout.base, layer.showSchema, schema), noTime = true)
                       else io)
      
      _         <- ~io.println(UserMsg { theme => table.mkString("\n") }, noTime = true)
    } yield io.await()
  }
}
