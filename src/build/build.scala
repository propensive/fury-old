/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.7.13. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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
  case class Context(cli: Cli[CliParam[_]])

  def context(cli: Cli[CliParam[_]]): Try[Context] = Try(new Context(cli))

  def set(ctx: Context): Try[ExitStatus] = {
    import ctx._
    for {
      cli      <- cli.hint(ThemeArg, Theme.all)
      cli      <- cli.hint(TimestampsArg, List("on", "off"))
      cli      <- cli.hint(PipeliningArg, List("on", "off"))
      cli      <- cli.hint(ServiceArg, List("furore.dev"))
      invoc    <- cli.read()
      log      <- invoc.logger()
      newTheme <- ~invoc(ThemeArg).toOption
      timestamps <- ~invoc(TimestampsArg).toOption
      pipelining <- ~invoc(PipeliningArg).toOption
      service    <- ~invoc(ServiceArg).toOption
      config   <- ~Installation.config()
      config   <- ~newTheme.map { th => config.copy(theme = th) }.getOrElse(config)
      config   <- ~service.map { s => config.copy(service = s) }.getOrElse(config)
      config   <- ~timestamps.map { ts => config.copy(timestamps = ts) }.getOrElse(config)
      config   <- ~pipelining.map { p => config.copy(pipelining = p) }.getOrElse(config)
      _        <- ~Ogdl.write(config, Installation.userConfig)
    } yield log.await()
  }
}

object AliasCli {
  def context(cli: Cli[CliParam[_]]) =
    for {
      layout <- cli.layout
      config <- ~cli.config
      layer  <- Layer.read(Log.silent(config), layout)
    } yield new MenuContext(cli, layout, config, layer)

  def list(ctx: MenuContext): Try[ExitStatus] = {
    import ctx._
    for {
      cli   <- cli.hint(RawArg)
      invoc <- cli.read()
      log   <- invoc.logger()
      raw   <- ~invoc(RawArg).isSuccess
      rows  <- ~layer.aliases.to[List]
      table <- ~Tables(config).show(Tables(config).aliases, cli.cols, rows, raw)(identity(_))
      _     <- ~(if(!raw) log.println(Tables(config).contextString(layout.baseDir, true)))
      _     <- ~log.info(UserMsg { theme => table.mkString("\n") })
    } yield log.await()
  }

  def remove(ctx: MenuContext): Try[ExitStatus] = {
    import ctx._
    for {
      cli        <- cli.hint(AliasArg, layer.aliases.map(_.cmd))
      invoc      <- cli.read()
      log        <- invoc.logger()
      aliasArg   <- invoc(AliasArg)
      aliasToDel <- ~layer.aliases.find(_.cmd == aliasArg)
      layer      <- Lenses.updateSchemas(None, layer, true) { s => Lenses.layer.aliases } (_(_) --= aliasToDel)
      _          <- ~Layer.save(log, layer, layout)
    } yield log.await()
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
      log              <- invoc.logger()
      moduleArg        <- invoc(ModuleArg)
      project          <- optProject.ascribe(UnspecifiedProject())
      module           <- project.modules.findBy(moduleArg)
      moduleRef        <- ~module.ref(project)
      aliasArg         <- invoc(AliasArg)
      description      <- invoc(DescriptionArg)
      alias            <- ~Alias(aliasArg, description, optSchemaArg, moduleRef)
      layer            <- Lenses.updateSchemas(None, layer, true) { s => Lenses.layer.aliases } (_(_) += alias)
      _                <- ~Layer.save(log, layer, layout)
    } yield log.await()
  }
}

object BuildCli {

  def context(cli: Cli[CliParam[_]]): Try[MenuContext] = for {
    layout <- cli.layout
    config <- ~cli.config
    layer  <- Layer.read(Log.silent(config), layout)
  } yield new MenuContext(cli, layout, config, layer)

  def notImplemented(cli: Cli[CliParam[_]]): Try[ExitStatus] = Success(Abort)

  def status: String = {
    val runtime = Runtime.getRuntime
    val df: DecimalFormat = new DecimalFormat("0.0")

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
  def since(start: Long): String = str"${formatter.format((System.currentTimeMillis - start)/1000.0)}s"

  def builds: String = Lifecycle.sessions.map { session =>
    str"[${session.pid}] started ${since(session.started)} ago: ${session.cli.args.args.mkString(" ")}"
  }.mkString("\n")

  def about(cli: Cli[CliParam[_]]): Try[ExitStatus] =
    for {
      invoc <- cli.read()
      log   <- invoc.logger()
      _     <- ~log.println(str"""|     _____ 
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
                                 |${status}
                                 |
                                 |${builds}
                                 |
                                 |For help on using Fury, run: fury help
                                 |""".stripMargin)
    } yield log.await()

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
      log          <- invoc.logger()
      project      <- optProject.ascribe(UnspecifiedProject())
      optModuleId  <- ~invoc(ModuleArg).toOption.orElse(moduleRef.map(_.moduleId)).orElse(project.main)
      optModule    <- ~optModuleId.flatMap(project.modules.findBy(_).toOption)
      https        <- ~invoc(HttpsArg).isSuccess
      module       <- optModule.ascribe(UnspecifiedModule())
      pipelining   <- ~invoc(PipeliningArg).toOption
      globalPolicy <- Policy.read(log)
      reporter     =  invoc(ReporterArg).toOption.getOrElse(GraphReporter)
      watch        =  invoc(WatchArg).isSuccess
      compilation  <- Compilation.syncCompilation(log, schema, module.ref(project), layout, https)
      watcher      =  new SourceWatcher(compilation.allSources)
      //_            =  watcher.directories.map(_.toString).foreach(s => log.info(str"$s"))
      _            =  if(watch) watcher.start()
      future       <- new Repeater[Try[Future[CompileResult]]] {
        var cnt: Int = 0
        override def repeatCondition(): Boolean = watcher.hasChanges

        override def stopCondition(): Boolean = !watch

        override def action(): Try[Future[CompileResult]] = {
          //log.info(str"Rebuild $cnt")
          cnt = cnt + 1
          watcher.clear()
          compileOnce(log, compilation, schema, module.ref(project), layout,
            globalPolicy, invoc.suffix, pipelining.getOrElse(Installation.config().pipelining),reporter, Installation.config().theme, https)
        }
      }.start()
      
    } yield {
      val result = Await.result(future, duration.Duration.Inf)
      watcher.stop
      log.await(result.isSuccessful)
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

  def upgrade(cli: Cli[CliParam[_]]): Try[ExitStatus] = Installation.tmpFile { tmpFile => for {
    layout        <- cli.layout
    invoc         <- cli.read()
    log           <- invoc.logger()
    records       <- Dns.lookup(Log.silent, Installation.config().service)
    latestRef     <- records.filter(_.startsWith("fury.latest:")).headOption.map(_.drop(12)).map(IpfsRef(_)).ascribe(NoLatestVersion())
    tmpFile       <- Installation.layersPath.mkTempFile()
    file          <- Shell(cli.env).ipfs.get(latestRef, tmpFile)
    _             <- tmpFile.delete()
    _             <- TarGz.extract(log, file, Installation.upgradeDir)
  } yield log.await()

  def prompt(cli: Cli[CliParam[_]]): Try[ExitStatus] = for {
    layout <- cli.layout
    config <- ~cli.config
    layer  <- ~Layer.read(Log.silent(config), layout).toOption
    msg    <- layer.fold(Try(Prompt.empty(config)(config.theme)))(getPrompt(_, config.theme))
    invoc  <- cli.read()
    log    <- invoc.logger()
    _      <- ~log.info(msg)
  } yield log.await()

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
      log            <- invoc.logger()
      dir            <- invoc(DirArg)
      https          <- ~invoc(HttpsArg).isSuccess
      project        <- optProject.ascribe(UnspecifiedProject())
      optModuleId    <- ~invoc(ModuleArg).toOption.orElse(project.main)
      optModule      <- ~optModuleId.flatMap(project.modules.findBy(_).toOption)
      module         <- optModule.ascribe(UnspecifiedModule())
      pipelining     <- ~invoc(PipeliningArg).toOption
      fatJar         =  invoc(FatJarArg).isSuccess
      globalPolicy   <- Policy.read(log)
      reporter       <- ~invoc(ReporterArg).toOption.getOrElse(GraphReporter)
      watch          =  invoc(WatchArg).isSuccess
      compilation    <- Compilation.syncCompilation(log, schema, module.ref(project), layout, https)
      watcher        =  new SourceWatcher(compilation.allSources)
      _              =  if(watch) watcher.start()
      future         <- new Repeater[Try[Future[CompileResult]]] {
        var cnt: Int = 0
        override def repeatCondition(): Boolean = watcher.hasChanges

        override def stopCondition(): Boolean = !watch

        override def action(): Try[Future[CompileResult]] = {
          //log.info(str"Rebuild $cnt")
          cnt = cnt + 1
          watcher.clear()
          for {
            task <- compileOnce(log, compilation, schema, module.ref(project), layout,
              globalPolicy, invoc.suffix, pipelining.getOrElse(Installation.config().pipelining), reporter, Installation.config().theme, https)
          } yield {
            task.transform { completed =>
              for{
                compileResult  <- completed
                compileSuccess <- compileResult.asTry
                _              <- compilation.saveJars(log, module.ref(project), compileSuccess.classDirectories,
                  dir in layout.pwd, layout, fatJar)
              } yield compileSuccess
            }
          }
        }
      }.start()
    } yield {
      val result = Await.result(future, duration.Duration.Inf)
      watcher.stop
      log.await(result.isSuccessful)
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
      log          <- invoc.logger()
      dir          <- invoc(DirArg)
      https        <- ~invoc(HttpsArg).isSuccess
      project      <- optProject.ascribe(UnspecifiedProject())
      optModuleId  <- ~invoc(ModuleArg).toOption.orElse(project.main)
      optModule    <- ~optModuleId.flatMap(project.modules.findBy(_).toOption)
      module       <- optModule.ascribe(UnspecifiedModule())
      
      compilation  <- Compilation.syncCompilation(log, schema, module.ref(project), layout,
                          https)
      
      _            <- if(module.kind == Application) Success(()) else Failure(InvalidKind(Application))
      main         <- module.main.ascribe(UnspecifiedMain(module.id))
      _            <- compilation.saveNative(log, module.ref(project), dir in layout.pwd, layout, main)
    } yield log.await()
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
      log          <- invoc.logger()
      https        <- ~invoc(HttpsArg).isSuccess
      project      <- optProject.ascribe(UnspecifiedProject())
      module       <- optModule.ascribe(UnspecifiedModule())
      
      compilation  <- Compilation.syncCompilation(log, schema, module.ref(project), layout,
                          https)
      
      classpath    <- ~compilation.classpath(module.ref(project), layout)
      _            <- ~log.println(classpath.map(_.value).join(":"))
    } yield log.await()
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
      log          <- invoc.logger()
      https        <- ~invoc(HttpsArg).isSuccess
      optModuleId  <- ~invoc(ModuleArg).toOption.orElse(optProject.flatMap(_.main))
      optModule    <- ~optModuleId.flatMap { arg => optProject.flatMap(_.modules.findBy(arg).toOption) }
      project      <- optProject.ascribe(UnspecifiedProject())
      module       <- optModule.ascribe(UnspecifiedModule())

      compilation  <- Compilation.syncCompilation(log, schema, module.ref(project), layout,
                          https)
      
      _            <- ~Graph.draw(compilation.graph.map { case (k, v) => (k.ref, v.map(_.ref).to[Set]) }, true,
                          Map())(Installation.config().theme).foreach(log.println(_))

    } yield log.await()
  }

  private[this] def compileOnce(log: Log,
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
      _            <- compilation.checkoutAll(log, layout, https)
    } yield {
      val multiplexer = new Multiplexer[ModuleRef, CompileEvent](compilation.targets.map(_._1).to[List])
      val future = compilation.compile(log, moduleRef, multiplexer, Map(), layout,
        globalPolicy, compileArgs, pipelining).apply(TargetId(schema.id, moduleRef)).andThen {
        case compRes =>
          multiplexer.closeAll()
          compRes
      }
      reporter.report(log, compilation.graph, theme, multiplexer)
      future
    }
  }

}

object LayerCli {
  def init(cli: Cli[CliParam[_]]): Try[ExitStatus] = for {
    layout <- cli.newLayout
    cli    <- cli.hint(ForceArg)
    invoc  <- cli.read()
    log    <- invoc.logger()
    force  =  invoc(ForceArg).isSuccess
    _      <- if (layout.focusFile.exists && !force) Failure(AlreadyInitialized()) else ~()
    _      <- layout.focusFile.mkParents()
    _      <- Layer.create(log, Layer(), layout)
    _      <- ~log.info(str"Initialized an empty layer")
  } yield log.await()

  def projects(cli: Cli[CliParam[_]]): Try[ExitStatus] = for {
    layout    <- cli.layout
    config    <- ~cli.config
    layer     <- Layer.read(Log.silent(config), layout)
    cli       <- cli.hint(SchemaArg, layer.schemas)
    cli       <- cli.hint(HttpsArg)
    schemaArg <- ~cli.peek(SchemaArg).getOrElse(layer.main)
    schema    <- layer.schemas.findBy(schemaArg)
    cli       <- cli.hint(RawArg)
    invoc     <- cli.read()
    log       <- invoc.logger()
    raw       <- ~invoc(RawArg).isSuccess
    https     <- ~invoc(HttpsArg).isSuccess
    projects  <- schema.allProjects(log, layout, https)
    table     <- ~Tables(config).show(Tables(config).projects(None), cli.cols, projects.distinct, raw)(_.id)
    _         <- ~(if(!raw) log.println(Tables(config).contextString(layout.baseDir, layer.showSchema, schema)))
    _         <- ~log.println(table.mkString("\n"))
  } yield log.await()

  def select(cli: Cli[CliParam[_]]): Try[ExitStatus] = for {
    layout    <- cli.layout
    config    <- ~cli.config
    baseLayer <- Layer.base(Log.silent(config), layout)
    schema    <- baseLayer.mainSchema
    cli       <- cli.hint(LayerArg, schema.importTree(Log.silent(config), layout, true).getOrElse(Nil))
    invoc     <- cli.read()
    log       <- invoc.logger()
    _         <- schema.importTree(log, layout, true)
    newPath   <- invoc(LayerArg)
    focus     <- Layer.readFocus(log, layout)
    newPath   <- ~focus.path.dereference(relPath)
    newFocus  <- ~focus.copy(path = newPath)
    _         <- Layer.saveFocus(log, newFocus, layout)
  } yield log.await()
 
  def extract(cli: Cli[CliParam[_]]): Try[ExitStatus] = for {
    cli      <- cli.hint(DirArg)
    cli      <- cli.hint(FileArg)
    invoc    <- cli.read()
    log      <- invoc.logger()
    pwd      <- cli.pwd
    file     <- invoc(FileArg).map(pwd.resolve(_))
    dir      <- ~cli.peek(DirArg).map(pwd.resolve(_)).getOrElse(pwd)
    layout   <- cli.newLayout.map(_.copy(baseDir = dir))
    layerRef <- Layer.loadFile(log, file, layout, cli.env)
    _        <- Layer.saveFocus(log, Focus(layerRef), layout)
  } yield log.await()

  def clone(cli: Cli[CliParam[_]]): Try[ExitStatus] = for {
    cli           <- cli.hint(DirArg)
    cli           <- cli.hint(ImportArg, Layer.pathCompletions(Log.silent(cli.config), cli.config.service, cli.env).getOrElse(Nil))
    invoc         <- cli.read()
    log           <- invoc.logger()
    layerImport   <- invoc(ImportArg)
    followable    <- Try(Layer.follow(layerImport, cli.config).get)
    layerRef      <- Layer.resolve(log, followable, cli.env)
    dir           <- invoc(DirArg)
    pwd           <- cli.pwd
    dir           <- ~pwd.resolve(dir)
    _             <- ~dir.mkdir()
    _             <- Layer.saveFocus(log, Focus(layerRef, ImportPath.Root), dir / ".focus.fury")
  } yield log.await()

  def share(cli: Cli[CliParam[_]]): Try[ExitStatus] = for {
    layout        <- cli.layout
    layer         <- Layer.read(Log.silent(cli.config), layout)
    invoc         <- cli.read()
    log           <- invoc.logger()
    ref           <- Layer.share(log, layer, cli.env)
    _             <- ~log.info(str"fury://${ref.key}")
  } yield log.await()

  def export(cli: Cli[CliParam[_]]): Try[ExitStatus] = for {
    layout        <- cli.layout
    cli           <- cli.hint(FileArg)
    layer         <- Layer.read(Log.silent(cli.config), layout)
    invoc         <- cli.read()
    log           <- invoc.logger()
    pwd           <- cli.pwd
    destination   <- invoc(FileArg).map(pwd.resolve(_))
    _             <- Layer.export(log, layer, layout, destination)
    _             <- ~log.info(msg"Saved layer file ${destination}")
  } yield log.await()

  def addImport(cli: Cli[CliParam[_]]): Try[ExitStatus] = {
    for {
      layout        <- cli.layout
      layer         <- Layer.read(Log.silent(cli.config), layout)
      cli           <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      cli           <- cli.hint(ImportNameArg)
      cli           <- cli.hint(FileArg)
      schemaArg     <- ~cli.peek(SchemaArg)
      defaultSchema <- ~layer.schemas.findBy(schemaArg.getOrElse(layer.main)).toOption
     
      cli           <- cli.hint(ImportArg, Layer.pathCompletions(Log.silent(cli.config), cli.config.service, cli.env).getOrElse(Nil))
      layerImport   <- ~cli.peek(ImportArg)
      fileImport    <- ~cli.peek(FileArg)
      followable    <- ~((layerImport, fileImport) match {
                         case (Some(imp), None) => Layer.follow(imp)
                         case _ => None
                       })
      layerRef      <- ~((layerImport, fileImport) match {
                         case (Some(imp), None) => for {
                           followable <- Layer.follow(imp, cli.config)
                           layerRef <- Layer.resolve(Log.silent(cli.config), followable, cli.env).toOption
                         } yield layerRef
                         case (None, Some(path)) =>
                           Layer.loadFile(Log.silent(cli.config), path in layout.pwd, layout, cli.env).toOption
                         case _ =>
                           None
                       })
      maybeLayer    <- ~layerRef.flatMap(Layer.read(Log.silent(cli.config), _, layout).toOption)
      cli           <- cli.hint(ImportSchemaArg, maybeLayer.map(_.schemas.map(_.id)).getOrElse(Nil))

      invoc         <- cli.read()
      log           <- invoc.logger()
      layerRef      <- ~followable.flatMap(Layer.resolve(log, _, cli.env).toOption)
      maybeLayer    <- ~layerRef.flatMap(Layer.read(log, _, layout).toOption)
      nameArg       <- invoc(ImportNameArg)
      schemaId      <- invoc(ImportSchemaArg)
      layerRef      <- layerRef.ascribe(UnspecifiedLayer())
      schemaRef     <- ~SchemaRef(nameArg, layerRef, schemaId, followable)
      layer         <- Lenses.updateSchemas(schemaArg, layer, true)(Lenses.layer.imports(_))(_.modify(_)(_ +
                           schemaRef.copy(id = nameArg)))
      
      _             <- ~Layer.save(log, layer, layout)
    } yield log.await()
  }

  def unimport(cli: Cli[CliParam[_]]): Try[ExitStatus] = {
    for {
      layout    <- cli.layout
      layer     <- Layer.read(Log.silent(cli.config), layout)
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      schemaArg <- ~cli.peek(SchemaArg)
      dSchema   <- ~layer.schemas.findBy(schemaArg.getOrElse(layer.main)).toOption
      cli       <- cli.hint(ImportIdArg, dSchema.map(_.imports.map(_.id)).getOrElse(Nil))
      invoc     <- cli.read()
      log       <- invoc.logger()
      schemaId  <- ~invoc(SchemaArg).toOption.getOrElse(layer.main)
      importArg <- invoc(ImportIdArg)
      schema    <- layer.schemas.findBy(schemaId)
      lens      <- ~Lenses.layer.imports(schema.id)
      layer     <- ~lens.modify(layer)(_.filterNot(_.id == importArg))
      _         <- ~Layer.save(log, layer, layout)
    } yield log.await()
  }

  def list(cli: Cli[CliParam[_]]): Try[ExitStatus] = {
    for {
      layout    <- cli.layout
      layer     <- Layer.read(Log.silent(cli.config), layout)
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      cli       <- cli.hint(HttpsArg)
      schemaArg <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaArg)
      cli       <- cli.hint(RawArg)
      invoc     <- cli.read()
      log       <- invoc.logger()
      raw       <- ~invoc(RawArg).isSuccess
      https     <- ~invoc(HttpsArg).isSuccess
      rows      <- ~schema.imports.to[List].map { i => (i, schema.resolve(i, log, layout, https)) }
      
      table     <- ~Tables().show(Tables().imports(Some(layer.main)), cli.cols, rows,
                       raw)(_._1.schema.key)
      
      _         <- ~(if(!raw) log.println(Tables(cli.config).contextString(layout.baseDir, layer.showSchema, schema))
                       else log)
      
      _         <- ~log.println(UserMsg { theme => table.mkString("\n") })
    } yield log.await()
  }
}
