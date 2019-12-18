/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.7.14. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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

import fury.strings._, fury.core._, fury.ogdl._, fury.model._, fury.io._, fury.utils._

import exoskeleton._
import euphemism._

import Args._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._, duration._
import scala.util._
import java.text.DecimalFormat

import fury.utils.Multiplexer

import language.higherKinds
import scala.util.control.NonFatal

object ConfigCli {
  case class Context(cli: Cli[CliParam[_]])

  def context(cli: Cli[CliParam[_]])(implicit log: Log): Try[Context] = Try(new Context(cli))

  def set(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli      <- cli.hint(ThemeArg, Theme.all)
      cli      <- cli.hint(TimestampsArg, List("on", "off"))
      cli      <- cli.hint(PipeliningArg, List("on", "off"))
      cli      <- cli.hint(ServiceArg, List("furore.dev"))
      call     <- cli.call()
      newTheme <- ~call(ThemeArg).toOption
      timestamps <- ~call(TimestampsArg).toOption
      pipelining <- ~call(PipeliningArg).toOption
      service    <- ~call(ServiceArg).toOption
      config   <- ~ManagedConfig()
      config   <- ~newTheme.map { th => config.copy(theme = th) }.getOrElse(config)
      config   <- ~service.map { s => config.copy(service = s) }.getOrElse(config)
      config   <- ~timestamps.map { ts => config.copy(timestamps = ts) }.getOrElse(config)
      config   <- ~pipelining.map { p => config.copy(pipelining = p) }.getOrElse(config)
      _        <- ~ManagedConfig.write(config)
    } yield log.await()
  }

  def auth(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      call     <- cli.call()
      code     <- ~Rnd.token(18)
      // These futures should be managed in the session
      uri      <- ~Uri("https", str"${ManagedConfig().service}/await?code=$code")
      _        <- ~log.info(msg"Please visit https://${ManagedConfig().service}/auth?code=$code to log in.")
      future   <- ~Future(blocking(Http.get(uri, Map("code" -> code), Set())))
      _        <- ~Future(blocking(Shell(cli.env).tryXdgOpen(str"https://${ManagedConfig().service}/auth?code=$code")))
      response <- Await.result(future, Duration.Inf)
      json     <- ~Json.parse(new String(response, "UTF-8")).get
      token    <- ~json.token.as[String].get
      config   <- ~ManagedConfig().copy(token = token)
      _        <- ~ManagedConfig.write(config)
      _        <- ~log.info("You are now authenticated")
    } yield log.await()

  }
}

object AliasCli {
  def context(cli: Cli[CliParam[_]])(implicit log: Log) =
    for {
      layout <- cli.layout
      layer  <- Layer.read(layout)
    } yield new MenuContext(cli, layout, layer)

  def list(ctx: MenuContext)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli   <- cli.hint(RawArg)
      call  <- cli.call()
      raw   <- ~call(RawArg).isSuccess
      rows  <- ~layer.aliases.to[List]
      table <- ~Tables().show(Tables().aliases, cli.cols, rows, raw)(identity(_))
      _     <- ~(if(!raw) log.info(Tables().contextString(layer, true)))
      _     <- ~log.info(UserMsg { theme => table.mkString("\n") })
    } yield log.await()
  }

  def remove(ctx: MenuContext)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli        <- cli.hint(AliasArg, layer.aliases.map(_.cmd))
      call       <- cli.call()
      aliasArg   <- call(AliasArg)
      aliasToDel <- ~layer.aliases.find(_.cmd == aliasArg)
      layer      <- Lenses.updateSchemas(None, layer, true) { s => Lenses.layer.aliases } (_(_) --= aliasToDel)
      _          <- ~Layer.save(layer, layout)
    } yield log.await()
  }

  def add(ctx: MenuContext)(implicit log: Log): Try[ExitStatus] = {
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
      call             <- cli.call()
      moduleArg        <- call(ModuleArg)
      project          <- optProject.ascribe(UnspecifiedProject())
      module           <- project.modules.findBy(moduleArg)
      moduleRef        <- ~module.ref(project)
      aliasArg         <- call(AliasArg)
      description      <- call(DescriptionArg)
      alias            <- ~Alias(aliasArg, description, optSchemaArg, moduleRef)
      layer            <- Lenses.updateSchemas(None, layer, true) { s => Lenses.layer.aliases } (_(_) += alias)
      _                <- ~Layer.save(layer, layout)
    } yield log.await()
  }
}

object BuildCli {

  def context(cli: Cli[CliParam[_]])(implicit log: Log): Try[MenuContext] = for {
    layout <- cli.layout
    layer  <- Layer.read(layout)
  } yield new MenuContext(cli, layout, layer)

  def notImplemented(cli: Cli[CliParam[_]])(implicit log: Log): Try[ExitStatus] = Success(Abort)

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

  def about(cli: Cli[CliParam[_]])(implicit log: Log): Try[ExitStatus] =
    for {
      call  <- cli.call()
      _     <- ~log.raw(str"""|     _____ 
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
             (implicit log: Log): Try[ExitStatus] = {
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
      call         <- cli.call()
      project      <- optProject.ascribe(UnspecifiedProject())
      optModuleId  <- ~call(ModuleArg).toOption.orElse(moduleRef.map(_.moduleId)).orElse(project.main)
      optModule    <- ~optModuleId.flatMap(project.modules.findBy(_).toOption)
      https        <- ~call(HttpsArg).isSuccess
      module       <- optModule.ascribe(UnspecifiedModule())
      pipelining   <- ~call(PipeliningArg).toOption
      globalPolicy <- ~Policy.read(log)
      reporter     =  call(ReporterArg).toOption.getOrElse(GraphReporter)
      watch        =  call(WatchArg).isSuccess
      compilation  <- Compilation.syncCompilation(schema, module.ref(project), layout, https)
      r            =  repeater[Try[Future[CompileResult]]](compilation.allSources) { _: Unit =>
                         compileOnce(compilation, schema, module.ref(project), layout,
                           globalPolicy, call.suffix, pipelining.getOrElse(ManagedConfig().pipelining),reporter, ManagedConfig().theme, https)
                      }
      future       <- if(watch) Try(r.start()).flatten else r.action()
    } yield {
      val result = Await.result(future, duration.Duration.Inf)
      log.await(result.isSuccessful)
    }
  }

  def getPrompt(layer: Layer, theme: Theme)(implicit log: Log): Try[String] = for {
    schemaId     <- ~layer.main
    schema       <- layer.schemas.findBy(schemaId)
    optProjectId <- ~schema.main
    optProject   <- ~optProjectId.flatMap(schema.projects.findBy(_).toOption)
    optModuleId  <- ~optProject.flatMap(_.main)
    optModule    <- ~optModuleId.flatMap { mId => optProject.flatMap(_.modules.findBy(mId).toOption) }
  } yield Prompt.zsh(layer, schema, optProject, optModule)(theme)

  def upgrade(cli: Cli[CliParam[_]])(implicit log: Log): Try[ExitStatus] = Installation.tmpFile { tmpFile => for {
    layout        <- cli.layout
    call          <- cli.call()
    records       <- Dns.lookup(ManagedConfig().service)
    latestRef     <- records.filter(_.startsWith("fury.latest:")).headOption.map(_.drop(12)).map(IpfsRef(_)).ascribe(NoLatestVersion())
    file          <- Shell(cli.env).ipfs.get(latestRef, tmpFile)
    _             <- TarGz.extract(file, Installation.upgradeDir)
  } yield log.await() }

  def prompt(cli: Cli[CliParam[_]])(implicit log: Log): Try[ExitStatus] = for {
    layout <- cli.layout
    layer  <- ~Layer.read(layout).toOption
    msg    <- layer.fold(Try(Prompt.empty(ManagedConfig().theme)))(getPrompt(_, ManagedConfig().theme))
    call   <- cli.call()
    _      <- ~log.info(msg)
  } yield log.await()

  def save(ctx: MenuContext)(implicit log: Log): Try[ExitStatus] = {
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
      call           <- cli.call()
      dir            <- call(DirArg)
      https          <- ~call(HttpsArg).isSuccess
      project        <- optProject.ascribe(UnspecifiedProject())
      optModuleId    <- ~call(ModuleArg).toOption.orElse(project.main)
      optModule      <- ~optModuleId.flatMap(project.modules.findBy(_).toOption)
      module         <- optModule.ascribe(UnspecifiedModule())
      pipelining     <- ~call(PipeliningArg).toOption
      fatJar         =  call(FatJarArg).isSuccess
      globalPolicy   <- ~Policy.read(log)
      reporter       <- ~call(ReporterArg).toOption.getOrElse(GraphReporter)
      watch          =  call(WatchArg).isSuccess
      compilation    <- Compilation.syncCompilation(schema, module.ref(project), layout, https)
      r              =  repeater[Try[Future[CompileResult]]](compilation.allSources) { _: Unit =>
        for {
          task <- compileOnce(compilation, schema, module.ref(project), layout,
            globalPolicy, call.suffix, pipelining.getOrElse(ManagedConfig().pipelining), reporter, ManagedConfig().theme, https)
        } yield {
          task.transform { completed =>
            for{
              compileResult  <- completed
              compileSuccess <- compileResult.asTry
              _              <- compilation.saveJars(module.ref(project), compileSuccess.classDirectories,
                dir in layout.pwd, layout, fatJar)
            } yield compileSuccess
          }
        }
      }
      future        <- if(watch) Try(r.start()).flatten else r.action()
    } yield {
      val result = Await.result(future, duration.Duration.Inf)
      log.await(result.isSuccessful)
    }
  }

  private def repeater[T](sources: Set[Path])(f: Unit => T): Repeater[T] = new Repeater[T]{
    private val watcher = new SourceWatcher(sources)
    override def repeatCondition(): Boolean = watcher.hasChanges

    override def start(): T = {
      try{
        watcher.start()
        super.start()
      } catch {
        case NonFatal(e) => throw e
        case x: Throwable => throw new Exception("Fatal exception inside watcher", x)
      } finally {
        watcher.stop
      }
    }
    override def action(): T = {
      watcher.clear()
      f()
    }
  }

  def install(ctx: MenuContext)(implicit log: Log): Try[ExitStatus] = {
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
      cli          <- cli.hint(ExecNameArg)
      //cli          <- cli.hint(DirArg)
      call         <- cli.call()
      name         <- call(ExecNameArg)
      //dir          <- call(DirArg)
      https        <- ~call(HttpsArg).isSuccess
      project      <- optProject.ascribe(UnspecifiedProject())
      optModuleId  <- ~call(ModuleArg).toOption.orElse(project.main)
      optModule    <- ~optModuleId.flatMap(project.modules.findBy(_).toOption)
      module       <- optModule.ascribe(UnspecifiedModule())
      
      compilation  <- Compilation.syncCompilation(schema, module.ref(project), layout,
                          https)
      
      _            <- if(module.kind == Application) Success(()) else Failure(InvalidKind(Application))
      main         <- module.main.ascribe(UnspecifiedMain(module.id))
      _            <- ~log.info(msg"Building native image for $name")
      _            <- compilation.saveNative(module.ref(project), Path("/usr/local/bin"), layout, main)
      bin          <- ~Path(str"/usr/local/bin/${main.toLowerCase}")
      newBin       <- ~(bin.rename { _ => name })
      _            <- bin.moveTo(newBin)
      _            <- ~log.info(msg"Installed $name executable")
    } yield log.await()
  }

  def console(ctx: MenuContext)(implicit log: Log): Try[ExitStatus] = {
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
      cli          <- cli.hint(SingleColumnArg)
      call         <- cli.call()
      https        <- ~call(HttpsArg).isSuccess
      singleColumn <- ~call(SingleColumnArg).isSuccess
      project      <- optProject.ascribe(UnspecifiedProject())
      module       <- optModule.ascribe(UnspecifiedModule())
      
      compilation  <- Compilation.syncCompilation(schema, module.ref(project), layout,
                          https)
      
      classpath    <- ~compilation.classpath(module.ref(project), layout)
      bootCp       <- ~compilation.bootClasspath(module.ref(project), layout)
    } yield {
      val separator = if(singleColumn) "\n" else ":"
      val cp = classpath.map(_.value).join(separator)
      val bcp = bootCp.map(_.value).join(separator)
      cli.continuation(str"""java -Xmx256M -Xms32M -Xbootclasspath/a:$bcp -classpath $cp -Dscala.boot.class.path=$cp -Dscala.home=/opt/scala-2.12.8 -Dscala.usejavacp=true scala.tools.nsc.MainGenericRunner""")
    }
  }

  def classpath(ctx: MenuContext)(implicit log: Log): Try[ExitStatus] = {
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
      cli          <- cli.hint(SingleColumnArg)
      call         <- cli.call()
      https        <- ~call(HttpsArg).isSuccess
      singleColumn <- ~call(SingleColumnArg).isSuccess
      project      <- optProject.ascribe(UnspecifiedProject())
      module       <- optModule.ascribe(UnspecifiedModule())
      
      compilation  <- Compilation.syncCompilation(schema, module.ref(project), layout,
                          https)
      
      classpath    <- ~compilation.classpath(module.ref(project), layout)
    } yield {
      val separator = if(singleColumn) "\n" else ":"
      log.raw(classpath.map(_.value).join(separator))
      log.await()
    }
  }

  def describe(ctx: MenuContext)(implicit log: Log): Try[ExitStatus] = {
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
      call         <- cli.call()
      https        <- ~call(HttpsArg).isSuccess
      optModuleId  <- ~call(ModuleArg).toOption.orElse(optProject.flatMap(_.main))
      optModule    <- ~optModuleId.flatMap { arg => optProject.flatMap(_.modules.findBy(arg).toOption) }
      project      <- optProject.ascribe(UnspecifiedProject())
      module       <- optModule.ascribe(UnspecifiedModule())

      compilation  <- Compilation.syncCompilation(schema, module.ref(project), layout,
                          https)
      
      _            <- ~Graph.draw(compilation.graph.links, true,
                          Map())(ManagedConfig().theme).foreach(log.info(_))

    } yield log.await()
  }

  private[this] def compileOnce(compilation: Compilation,
                                schema: Schema,
                                moduleRef: ModuleRef,
                                layout: Layout,
                                globalPolicy: Policy,
                                compileArgs: List[String],
                                pipelining: Boolean,
                                reporter: Reporter,
                                theme: Theme,
                                https: Boolean)
                               (implicit log: Log): Try[Future[CompileResult]] = {
    for {
      _            <- compilation.checkoutAll(layout, https)
    } yield {
      val multiplexer = new Multiplexer[ModuleRef, CompileEvent](compilation.targets.map(_._1).to[List])
      val future = compilation.compile(moduleRef, multiplexer, Map(), layout,
        globalPolicy, compileArgs, pipelining).apply(TargetId(schema.id, moduleRef)).andThen {
        case compRes =>
          multiplexer.closeAll()
          compRes
      }
      reporter.report(compilation.graph, theme, multiplexer)
      future
    }
  }

}

object LayerCli {
  def init(cli: Cli[CliParam[_]])(implicit log: Log): Try[ExitStatus] = for {
    layout <- cli.newLayout
    cli    <- cli.hint(ForceArg)
    call   <- cli.call()
    force  =  call(ForceArg).isSuccess
    _      <- if (layout.focusFile.exists && !force) Failure(AlreadyInitialized()) else ~()
    _      <- layout.focusFile.mkParents()
    _      <- Layer.create(Layer(), layout)
    _      <- ~log.info(str"Initialized an empty layer")
  } yield log.await()

  def projects(cli: Cli[CliParam[_]])(implicit log: Log): Try[ExitStatus] = for {
    layout    <- cli.layout
    layer     <- Layer.read(layout)
    cli       <- cli.hint(SchemaArg, layer.schemas)
    cli       <- cli.hint(HttpsArg)
    schemaArg <- ~cli.peek(SchemaArg).getOrElse(layer.main)
    schema    <- layer.schemas.findBy(schemaArg)
    cli       <- cli.hint(RawArg)
    call      <- cli.call()
    raw       <- ~call(RawArg).isSuccess
    https     <- ~call(HttpsArg).isSuccess
    projects  <- schema.allProjects(layout, https)
    table     <- ~Tables().show(Tables().projects(None), cli.cols, projects.distinct, raw)(_.id)
    _         <- ~(if(!raw) log.info(Tables().contextString(layer, layer.showSchema, schema)))
    _         <- ~log.info(table.mkString("\n"))
  } yield log.await()

  def select(cli: Cli[CliParam[_]])(implicit log: Log): Try[ExitStatus] = for {
    layout    <- cli.layout
    baseLayer <- Layer.base(layout)
    schema    <- baseLayer.mainSchema
    cli       <- cli.hint(LayerArg,  schema.importTree(layout, true).getOrElse(Nil))
    call      <- cli.call()
    layers    <- schema.importTree(layout, true)
    relPath   <- call(LayerArg)
    focus     <- Layer.readFocus(layout)
    newPath   <- focus.path.dereference(relPath)
    _         <- verifyLayers(newPath, layers)
    newFocus  <- ~focus.copy(path = newPath)
    _         <- Layer.saveFocus(newFocus, layout)
  } yield log.await()


  def verifyLayers(path: ImportPath, list: List[ImportPath]): Try[Unit] =
    if (list.map(_.path).contains(path.path))
      Success()
    else
      Failure(LayersFailure(path))

  def extract(cli: Cli[CliParam[_]])(implicit log: Log): Try[ExitStatus] = for {
    cli      <- cli.hint(DirArg)
    cli      <- cli.hint(FileArg)
    call     <- cli.call()
    pwd      <- cli.pwd
    file     <- call(FileArg).map(pwd.resolve(_))
    dir      <- ~cli.peek(DirArg).map(pwd.resolve(_)).getOrElse(pwd)
    layout   <- cli.newLayout.map(_.copy(baseDir = dir))
    layerRef <- Layer.loadFile(file, layout, cli.env)
    _        <- Layer.saveFocus(Focus(layerRef), layout)
  } yield log.await()

  def clone(cli: Cli[CliParam[_]])(implicit log: Log): Try[ExitStatus] = for {
    cli           <- cli.hint(DirArg)
    cli           <- cli.hint(ImportArg, Layer.pathCompletions().getOrElse(Nil))
    call          <- cli.call()
    layout        <- cli.newLayout
    layerImport   <- call(ImportArg)
    layerRef      <- Layer.resolveLoad(layerImport, layout, cli.env)
    dir           <- call(DirArg)
    pwd           <- cli.pwd
    dir           <- ~pwd.resolve(dir)
    _             <- ~dir.mkdir()
    _             <- Layer.saveFocus(Focus(layerRef, ImportPath.Root), dir / ".focus.fury")
  } yield log.await()

  def publish(cli: Cli[CliParam[_]])(implicit log: Log): Try[ExitStatus] = for {
    layout        <- cli.layout
    cli           <- cli.hint(RemoteLayerArg)
    call          <- cli.call()
    layer         <- Layer.read(layout)
    path          <- call(RemoteLayerArg)
    ref           <- Layer.share(layer, layout, cli.env)
    pub           <- Service.publish(ref.key, cli.env, path)
    _             <- ~log.info(msg"Shared at ${ref.uri}")
    _             <- ~log.info(msg"Published to ${Uri("fury", str"${ManagedConfig().service}/${path}")}")
  } yield log.await()

  def share(cli: Cli[CliParam[_]])(implicit log: Log): Try[ExitStatus] = for {
    layout        <- cli.layout
    layer         <- Layer.read(layout)
    call          <- cli.call()
    ref           <- Layer.share(layer, layout, cli.env)
    _             <- ~log.info(msg"Shared at ${ref.uri}")
  } yield log.await()

  def export(cli: Cli[CliParam[_]])(implicit log: Log): Try[ExitStatus] = for {
    layout        <- cli.layout
    cli           <- cli.hint(FileArg)
    layer         <- Layer.read(layout)
    call          <- cli.call()
    pwd           <- cli.pwd
    destination   <- call(FileArg).map(pwd.resolve(_))
    _             <- Layer.export(layer, layout, destination)
    _             <- ~log.info(msg"Saved layer file ${destination}")
  } yield log.await()

  def addImport(cli: Cli[CliParam[_]])(implicit log: Log): Try[ExitStatus] = {
    for {
      layout        <- cli.layout
      layer         <- Layer.read(layout)
      cli           <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      cli           <- cli.hint(ImportNameArg)
      schemaArg     <- ~cli.peek(SchemaArg)
      defaultSchema <- ~layer.schemas.findBy(schemaArg.getOrElse(layer.main)).toOption
     
      cli           <- cli.hint(ImportArg, Layer.pathCompletions().getOrElse(Nil))
      layerImport   <- ~cli.peek(ImportArg)
      layerRef      <- ~layerImport.flatMap(Layer.resolveLoad(_, layout, cli.env).toOption)
      maybeLayer    <- ~layerRef.flatMap(Layer.read(_, layout).toOption)
      cli           <- cli.hint(ImportSchemaArg, maybeLayer.map(_.schemas.map(_.id)).getOrElse(Nil))
      call          <- cli.call()
      layerImport   <- call(ImportArg)
      layerInput    <- Layer.resolve(layerImport, layout).ascribe(InvalidLayer(layerImport))
      nameArg       <- cli.peek(ImportNameArg).orElse(layerInput.suggestedName).ascribe(MissingArg("name"))
      schemaId      <- cli.peek(ImportSchemaArg).orElse(maybeLayer.map(_.main)).ascribe(MissingArg("schema"))
      layerRef      <- Layer.resolveLoad(layerImport, layout, cli.env)
      schemaRef     <- ~SchemaRef(nameArg, layerRef, schemaId)
      layer         <- Lenses.updateSchemas(schemaArg, layer, true)(Lenses.layer.imports(_))(_.modify(_)(_ +
                           schemaRef.copy(id = nameArg)))
      
      _             <- ~Layer.save(layer, layout)
    } yield log.await()
  }

  def unimport(cli: Cli[CliParam[_]])(implicit log: Log): Try[ExitStatus] = {
    for {
      layout    <- cli.layout
      layer     <- Layer.read(layout)
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      schemaArg <- ~cli.peek(SchemaArg)
      dSchema   <- ~layer.schemas.findBy(schemaArg.getOrElse(layer.main)).toOption
      cli       <- cli.hint(ImportIdArg, dSchema.map(_.imports.map(_.id)).getOrElse(Nil))
      call      <- cli.call()
      schemaId  <- ~call(SchemaArg).toOption.getOrElse(layer.main)
      importArg <- call(ImportIdArg)
      schema    <- layer.schemas.findBy(schemaId)
      lens      <- ~Lenses.layer.imports(schema.id)
      layer     <- ~lens.modify(layer)(_.filterNot(_.id == importArg))
      _         <- ~Layer.save(layer, layout)
    } yield log.await()
  }

  def list(cli: Cli[CliParam[_]])(implicit log: Log): Try[ExitStatus] = {
    for {
      layout    <- cli.layout
      layer     <- Layer.read(layout)
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      cli       <- cli.hint(HttpsArg)
      schemaArg <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaArg)
      cli       <- cli.hint(RawArg)
      call      <- cli.call()
      raw       <- ~call(RawArg).isSuccess
      https     <- ~call(HttpsArg).isSuccess
      rows      <- ~schema.imports.to[List].map { i => (i, schema.resolve(i, layout, https)) }
      
      table     <- ~Tables().show(Tables().imports(Some(layer.main)), cli.cols, rows,
                       raw)(_._1.schema.key)
      
      _         <- ~(if(!raw) log.info(Tables().contextString(layer, layer.showSchema, schema))
                       else log)
      
      _         <- ~log.info(UserMsg { theme => table.mkString("\n") })
    } yield log.await()
  }
}
