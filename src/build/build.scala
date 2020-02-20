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
package fury

import fury.strings._, fury.core._, fury.model._, fury.io._

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

case class ConfigCli(cli: Cli)(implicit log: Log) {
  def set: Try[ExitStatus] = for {
    cli      <- cli.hint(ThemeArg, Theme.all)
    cli      <- cli.hint(TimestampsArg, List("on", "off"))
    cli      <- cli.hint(PipeliningArg, List("on", "off"))
    cli      <- cli.hint(TraceArg, List("on", "off"))
    cli      <- cli.hint(ServiceArg, List("furore.dev"))
    call     <- cli.call()
    newTheme <- ~call(ThemeArg).toOption
    timestamps <- ~call(TimestampsArg).toOption
    pipelining <- ~call(PipeliningArg).toOption
    trace    <- ~call(TraceArg).toOption
    service  <- ~call(ServiceArg).toOption
    config   <- ~ManagedConfig()
    config   <- ~newTheme.map { th => config.copy(theme = th) }.getOrElse(config)
    config   <- ~service.map { s => config.copy(service = s) }.getOrElse(config)
    config   <- ~timestamps.map { ts => config.copy(timestamps = ts) }.getOrElse(config)
    config   <- ~pipelining.map { p => config.copy(pipelining = p) }.getOrElse(config)
    config   <- ~trace.map { t => config.copy(trace = t) }.getOrElse(config)
    _        <- ~ManagedConfig.write(config)
  } yield log.await()

  def auth: Try[ExitStatus] = for {
    call     <- cli.call()
    code     <- ~Rnd.token(18)
    // These futures should be managed in the session
    uri      <- ~Https(Path(ManagedConfig().service) / str"await?code=$code")
    _        <- ~log.info(msg"Please visit $uri to log in.")
    future   <- ~Future(blocking(Http.get(uri, Map("code" -> code), Set())))
    _        <- ~Future(blocking(Shell(cli.env).tryXdgOpen(uri)))
    response <- Await.result(future, Duration.Inf)
    json     <- ~Json.parse(new String(response, "UTF-8")).get
    token    <- ~json.token.as[String].get
    config   <- ~ManagedConfig().copy(token = token)
    _        <- ~ManagedConfig.write(config)
    _        <- ~log.info("You are now authenticated")
  } yield log.await()
}

case class AboutCli(cli: Cli)(implicit log: Log) {

  private def resourcesString: String = {
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
  private def since(start: Long): String = str"${formatter.format((System.currentTimeMillis - start)/1000.0)}s"

  private def tasksString: String = Lifecycle.sessions.map { session =>
    str"[${session.pid}] started ${since(session.started)} ago: ${session.cli.args.args.mkString(" ")}"
  }.mkString("\n")

  private def connectionsString: String = BloopServer.workDirectories.map { dir =>
    str"$dir"
  }.mkString("\n")

  private def withTemplate(content: String): String = {
    str"""|     _____
          |    / ___/__ __ ____ __ __
          |   / __/ / // // ._// // /
          |  /_/    \_._//_/  _\_. /
          |                   \___/
          |
          |Fury build tool for Scala, version ${FuryVersion.current}.
          |This software is provided under the Apache 2.0 License.
          |Fury depends on Bloop, Coursier, Git and Nailgun.
          |© Copyright 2018-19 Jon Pretty, Propensive OÜ.
          |
          |See the Fury website at https://fury.build/, or follow @propensive on Twitter
          |for more information.
          |
          |${content}
          |
          |For help on using Fury, run: fury help
          |""".stripMargin
  }

  def resources: Try[ExitStatus] =
    cli.call().map{ _ =>
      log.raw(withTemplate(resourcesString))
      log.await()
    }

  def tasks: Try[ExitStatus] =
    cli.call().map{ _ =>
      log.raw(withTemplate(tasksString))
      log.await()
    }

  def connections: Try[ExitStatus] =
    cli.call().map{ _ =>
      log.raw(withTemplate(connectionsString))
      log.await()
    }

}

case class AliasCli(cli: Cli)(implicit log: Log) {
  def list: Try[ExitStatus] = for {
    layout <- cli.layout
    conf   <- Layer.readFuryConf(layout)
    layer  <- Layer.read(layout, conf)
    cli    <- cli.hint(RawArg)
    table  <- ~Tables().aliases
    cli    <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
    cli    <- cli.hint(AliasArg, layer.aliases)
    call   <- cli.call()
    col    <- ~cli.peek(ColumnArg)
    alias  <- ~cli.peek(AliasArg)
    raw    <- ~call(RawArg).isSuccess
    rows   <- ~layer.aliases.to[List]
    table  <- ~Tables().show(table, cli.cols, rows, raw, col, alias, "alias")
    _      <- ~log.infoWhen(!raw)(conf.focus())
    _      <- ~log.rawln(table)
  } yield log.await()

  def remove: Try[ExitStatus] = for {
    layout     <- cli.layout
    conf       <- Layer.readFuryConf(layout)
    layer      <- Layer.read(layout, conf)
    cli        <- cli.hint(AliasArg, layer.aliases.map(_.cmd))
    call       <- cli.call()
    aliasArg   <- call(AliasArg)
    aliasToDel <- ~layer.aliases.find(_.cmd == aliasArg)
    layer      <- Lenses.updateSchemas(layer) { s => Lenses.layer.aliases } (_(_) --= aliasToDel)
    _          <- Layer.save(layer, layout)
  } yield log.await()

  def add: Try[ExitStatus] = for {
    layout           <- cli.layout
    conf             <- Layer.readFuryConf(layout)
    layer            <- Layer.read(layout, conf)
    optSchemaArg     <- ~Some(SchemaId.default)
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
    project          <- optProject.asTry
    module           <- project.modules.findBy(moduleArg)
    moduleRef        <- ~module.ref(project)
    aliasArg         <- call(AliasArg)
    description      <- call(DescriptionArg)
    alias            <- ~Alias(aliasArg, description, moduleRef, call.suffix)
    layer            <- Lenses.updateSchemas(layer) { s => Lenses.layer.aliases } (_(_) += alias)
    _                <- Layer.save(layer, layout)
  } yield log.await()
}

case class BuildCli(cli: Cli)(implicit log: Log) {

  def notImplemented: Try[ExitStatus] = Success(Abort)

  def upgrade: Try[ExitStatus] = Installation.tmpFile { tmpFile => for {
    layout <- cli.layout
    conf   <- Layer.readFuryConf(layout)
    layer  <- Layer.read(layout, conf)
    layout        <- cli.layout
    call          <- cli.call()
    records       <- Dns.lookup(ManagedConfig().service)
    latestRef     <- records.filter(_.startsWith("fury.latest:")).headOption.map(_.drop(12)).map(IpfsRef(_)).ascribe(NoLatestVersion())
    ipfs          <- Ipfs.daemon(false)
    file          <- ipfs.get(latestRef, tmpFile)
    _             <- TarGz.extract(file, Installation.upgradeDir)
  } yield log.await() }

  def prompt: Try[ExitStatus] = for {
    layout  <- cli.layout
    conf    <- Layer.readFuryConf(layout)
    call    <- cli.call()
    layer   <- Layer.read(layout, conf)
    schema  <- layer.schemas.findBy(SchemaId.default)
    project <- schema.mainProject
    module  <- ~project.flatMap(_.main)
    _       <- ~log.raw(Prompt.rewrite(conf.focus(project.fold(ProjectId("?"))(_.id), module.getOrElse(ModuleId("?"))).string(ManagedConfig().theme)))
  } yield log.await()

  def clean: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.read(layout, conf)
    schemaArg    <- ~SchemaId.default
    schema       <- layer.schemas.findBy(schemaArg)
    cli          <- cli.hint(ProjectArg, schema.projects)
    optProjectId <- ~cli.peek(ProjectArg).orElse(schema.main)
    optProject   <- ~optProjectId.flatMap(schema.projects.findBy(_).toOption)
    cli          <- cli.hint(ModuleArg, optProject.map(_.modules).getOrElse(Nil))
    optModuleId  <- ~cli.peek(ModuleArg).orElse(optProject.flatMap(_.main))
    optModule    <- ~optModuleId.flatMap { arg => optProject.flatMap(_.modules.findBy(arg).toOption) }
    call         <- cli.call()
    project      <- optProject.asTry
    module       <- optModule.asTry
    moduleRef    =  module.ref(project)

    compilation  <- Compilation.syncCompilation(schema, moduleRef, layout, https = false)

    result    <- compilation.cleanCache(moduleRef, layout)
  } yield {
    Option(result.getMessage()).foreach(log.info(_))
    val success = result.getCleaned()
    if(success){
      log.note(msg"Cleaned compiler caches for $moduleRef and its dependencies")
    } else {
      log.warn(msg"Compiler caches for $moduleRef and its dependencies were not cleaned")
    }
    log.await(success)
  }

  def onlyOne(watch: Boolean, wait: Boolean): Try[Unit] =
    if(watch && wait) Failure(CantWatchAndWait()) else Success(())

  def compile(moduleRef: Option[ModuleRef], args: List[String] = Nil): Try[ExitStatus] = for {
    layout         <- cli.layout
    conf           <- Layer.readFuryConf(layout)
    layer          <- Layer.read(layout, conf)
    cli            <- cli.hint(HttpsArg)
    cli            <- cli.hint(WaitArg)
    cli            <- cli.hint(WatchArg)
    schemaArg      <- ~SchemaId.default
    schema         <- layer.schemas.findBy(schemaArg)
    cli            <- cli.hint(ProjectArg, schema.projects)
    optProjectId   <- ~cli.peek(ProjectArg).orElse(moduleRef.map(_.projectId)).orElse(schema.main)
    cli            <- cli.hint(PipeliningArg, List("on", "off"))
    optProject     <- ~optProjectId.flatMap(schema.projects.findBy(_).toOption)
    cli            <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    cli            <- cli.hint(DirArg)
    cli            <- cli.hint(FatJarArg)
    cli            <- cli.hint(ReporterArg, Reporter.all)
    call           <- cli.call()
    dir            <- ~call(DirArg).toOption
    https          <- ~call(HttpsArg).isSuccess
    project        <- optProject.asTry
    optModuleId    <- ~call(ModuleArg).toOption.orElse(moduleRef.map(_.moduleId)).orElse(project.main)
    optModule      <- ~optModuleId.flatMap(project.modules.findBy(_).toOption)
    module         <- optModule.asTry
    pipelining     <- ~call(PipeliningArg).toOption
    fatJar         =  call(FatJarArg).isSuccess
    globalPolicy   <- ~Policy.read(log)
    reporter       <- ~call(ReporterArg).toOption.getOrElse(GraphReporter)
    watch          =  call(WatchArg).isSuccess
    waiting        =  call(WaitArg).isSuccess
    _              <- onlyOne(watch, waiting)
    compilation    <- Compilation.syncCompilation(schema, module.ref(project), layout, https)
    r              =  repeater(compilation.allSources, waiting) {
      for {
        task <- compileOnce(compilation, schema, module.ref(project), layout,
          globalPolicy, if(call.suffix.isEmpty) args else call.suffix, pipelining.getOrElse(ManagedConfig().pipelining), reporter, ManagedConfig().theme, https)
      } yield {
        task.transform { completed =>
          for {
            compileResult  <- completed
            compileSuccess <- compileResult.asTry
            _              <- ~(dir.foreach { dir => compilation.saveJars(module.ref(project),
                                  compileSuccess.classDirectories, dir in layout.pwd, layout, fatJar)
                              })
          } yield compileSuccess
        }
      }
    }
    future        <- if(watch || waiting) Try(r.start()).flatten else r.action()
  } yield log.await(Await.result(future, duration.Duration.Inf).isSuccessful)

  private def repeater(sources: Set[Path], onceOnly: Boolean)
                      (fn: => Try[Future[CompileResult]])
                      : Repeater[Try[Future[CompileResult]]] =
    new Repeater[Try[Future[CompileResult]]] {
      private[this] val watcher = new SourceWatcher(sources)
      override def repeatCondition(): Boolean = watcher.hasChanges

      def continue(res: Try[Future[CompileResult]]) = !onceOnly

      override def start(): Try[Future[CompileResult]] = try {
        watcher.start()
        super.start()
      } catch {
        case NonFatal(e)  => throw e
        case x: Throwable => throw new Exception("Fatal exception inside watcher", x)
      } finally watcher.stop()

      override def action() = {
        watcher.clear()
        fn
      }
    }

  def install: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.read(layout, conf)
    cli          <- cli.hint(HttpsArg)
    schemaArg    <- ~SchemaId.default
    schema       <- layer.schemas.findBy(schemaArg)
    cli          <- cli.hint(ProjectArg, schema.projects)
    optProjectId <- ~cli.peek(ProjectArg).orElse(schema.main)
    optProject   <- ~optProjectId.flatMap(schema.projects.findBy(_).toOption)
    cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    cli          <- cli.hint(ExecNameArg)
    //cli          <- cli.hint(DirArg)
    call         <- cli.call()
    exec         <- call(ExecNameArg)
    //dir          <- call(DirArg)
    https        <- ~call(HttpsArg).isSuccess
    project      <- optProject.asTry
    optModuleId  <- ~call(ModuleArg).toOption.orElse(project.main)
    optModule    <- ~optModuleId.flatMap(project.modules.findBy(_).toOption)
    module       <- optModule.asTry
    
    compilation  <- Compilation.syncCompilation(schema, module.ref(project), layout,
                        https)
    
    _            <- if(module.kind == Application) Success(()) else Failure(InvalidKind(Application))
    main         <- module.main.ascribe(UnspecifiedMain(module.id))
    _            <- ~log.info(msg"Building native image for $exec")
    _            <- compilation.saveNative(module.ref(project), Installation.usrDir, layout, main)
    bin          <- ~(Installation.usrDir / main.key.toLowerCase)
    newBin       <- ~(bin.rename { _ => exec.key })
    _            <- bin.moveTo(newBin)
    _            <- ~log.info(msg"Installed $exec executable")
  } yield log.await()

  def console: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.read(layout, conf)
    cli          <- cli.hint(HttpsArg)
    schemaArg    <- ~SchemaId.default
    schema       <- layer.schemas.findBy(schemaArg)
    cli          <- cli.hint(ProjectArg, schema.projects)
    optProjectId <- ~cli.peek(ProjectArg).orElse(schema.main)
    optProject   <- ~optProjectId.flatMap(schema.projects.findBy(_).toOption)
    cli          <- cli.hint(ModuleArg, optProject.map(_.modules).getOrElse(Nil))
    optModuleId  <- ~cli.peek(ModuleArg).orElse(optProject.flatMap(_.main))
    optModule    <- ~optModuleId.flatMap { arg => optProject.flatMap(_.modules.findBy(arg).toOption) }
    call         <- cli.call()
    https        <- ~call(HttpsArg).isSuccess
    project      <- optProject.asTry
    module       <- optModule.asTry
    
    compilation  <- Compilation.syncCompilation(schema, module.ref(project), layout,
                        https)
    
    classpath    <- ~compilation.classpath(module.ref(project), layout)
    bootCp       <- ~compilation.bootClasspath(module.ref(project), layout)
  } yield {
    val cp = classpath.map(_.value).join(":")
    val bcp = bootCp.map(_.value).join(":")
    cli.continuation(str"""java -Xmx256M -Xms32M -Xbootclasspath/a:$bcp -classpath $cp -Dscala.boot.class.path=$cp -Dscala.home=/opt/scala-2.12.8 -Dscala.usejavacp=true scala.tools.nsc.MainGenericRunner""")
  }

  def classpath: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.read(layout, conf)
    cli          <- cli.hint(HttpsArg)
    schemaArg    <- ~SchemaId.default
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
    project      <- optProject.asTry
    module       <- optModule.asTry
    
    compilation  <- Compilation.syncCompilation(schema, module.ref(project), layout,
                        https)
    
    classpath    <- ~compilation.classpath(module.ref(project), layout)
  } yield {
    val separator = if(singleColumn) "\n" else ":"
    log.rawln(classpath.map(_.value).join(separator))
    log.await()
  }

  def describe: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.read(layout, conf)
    cli          <- cli.hint(HttpsArg)
    schemaArg    <- ~SchemaId.default
    schema       <- layer.schemas.findBy(schemaArg)
    cli          <- cli.hint(ProjectArg, schema.projects)
    optProjectId <- ~cli.peek(ProjectArg).orElse(schema.main)
    optProject   <- ~optProjectId.flatMap(schema.projects.findBy(_).toOption)
    cli          <- cli.hint(ModuleArg, optProject.map(_.modules).getOrElse(Nil))
    call         <- cli.call()
    https        <- ~call(HttpsArg).isSuccess
    optModuleId  <- ~call(ModuleArg).toOption.orElse(optProject.flatMap(_.main))
    optModule    <- ~optModuleId.flatMap { arg => optProject.flatMap(_.modules.findBy(arg).toOption) }
    project      <- optProject.asTry
    module       <- optModule.asTry

    compilation  <- Compilation.syncCompilation(schema, module.ref(project), layout,
                        https)
    
    _            <- ~UiGraph.draw(compilation.graph.links, true,
                        Map())(ManagedConfig().theme).foreach(log.info(_))

  } yield log.await()

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
      Lifecycle.currentSession.multiplexer = multiplexer
      val future = compilation.compile(moduleRef, Map(), layout,
        globalPolicy, compileArgs, pipelining).apply(TargetId(moduleRef)).andThen {
        case compRes =>
          multiplexer.closeAll()
          compRes
      }
      reporter.report(compilation.graph, theme, multiplexer)
      future
    }
  }

}

case class LayerCli(cli: Cli)(implicit log: Log) {
  def init: Try[ExitStatus] = for {
    layout <- cli.newLayout
    call   <- cli.call()
    _      <- Layer.init(layout)
    _      =  Bsp.createConfig(layout)
  } yield log.await()

  def projects: Try[ExitStatus] = for {
    layout    <- cli.layout
    conf      <- Layer.readFuryConf(layout)
    layer     <- Layer.read(layout, conf)
    cli       <- cli.hint(HttpsArg)
    schemaArg <- ~SchemaId.default
    schema    <- layer.schemas.findBy(schemaArg)
    cli       <- cli.hint(RawArg)
    cli       <- cli.hint(ProjectArg, schema.projects.map(_.id))
    table     <- ~Tables().projects(None)
    cli       <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
    call      <- cli.call()
    col       <- ~cli.peek(ColumnArg)
    projectId <- ~cli.peek(ProjectArg)
    raw       <- ~call(RawArg).isSuccess
    https     <- ~call(HttpsArg).isSuccess
    projects  <- schema.allProjects(layout, https)
    table     <- ~Tables().show(table, cli.cols, projects.distinct, raw, col, projectId, "project")
    _         <- ~log.infoWhen(!raw)(conf.focus())
    _         <- ~log.rawln(table)
  } yield log.await()

  def select: Try[ExitStatus] = for {
    layout       <- cli.layout
    baseLayer    <- Layer.base(layout)
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.read(layout, conf)
    baseSchema   <- baseLayer.mainSchema
    schema       <- layer.mainSchema
    focus        <- Layer.readFuryConf(layout)
    currentLayer <- ~Some(focus.path)
    absTree      <- ~baseSchema.importTree(layout, true).getOrElse(Nil)
    relLayers    <- ~schema.imports.map { sr => ImportPath(sr.id.key) }
    parentLayer  <- if(baseLayer == layer) ~None else ~Some(ImportPath(".."))
    cli          <- cli.hint(LayerArg, absTree.to[Set] ++ relLayers ++ parentLayer -- currentLayer)
    call         <- cli.call()
    relPath      <- call(LayerArg)
    newPath      <- focus.path.dereference(relPath)
    _            <- verifyLayers(newPath, absTree)
    newConf      <- ~focus.copy(path = newPath)
    _            <- Layer.saveFuryConf(newConf, layout)
  } yield log.await()


  def verifyLayers(path: ImportPath, list: List[ImportPath]): Try[Unit] =
    if (list.map(_.path).contains(path.path))
      Success()
    else
      Failure(LayersFailure(path))

  def extract: Try[ExitStatus] = for {
    cli      <- cli.hint(DirArg)
    cli      <- cli.hint(FileArg)
    call     <- cli.call()
    pwd      <- cli.pwd
    file     <- call(FileArg).map(pwd.resolve(_))
    dir      <- ~cli.peek(DirArg).map(pwd.resolve(_)).getOrElse(pwd)
    layout   <- cli.newLayout.map(_.copy(baseDir = dir))
    _        =  Bsp.createConfig(layout)
    layerRef <- Layer.loadFile(file, layout)
    _        <- Layer.saveFuryConf(FuryConf(layerRef, ImportPath.Root), layout)
  } yield log.await()

  def cloneLayer: Try[ExitStatus] = for {
    cli           <- cli.hint(DirArg)
    cli           <- cli.hint(ImportArg, Layer.pathCompletions().getOrElse(Nil))
    call          <- cli.call()
    fakeLayout    <- cli.newLayout
    layerImport   <- call(ImportArg)
    resolved      <- Layer.parse(layerImport, fakeLayout)
    layerRef      <- Layer.load(resolved, fakeLayout)
    dir           <- call(DirArg).pacify(resolved.suggestedName.map { n => Path(n.key) })
    pwd           <- cli.pwd
    dir           <- ~pwd.resolve(dir)
    _             <- ~dir.mkdir()
    layout        =  fakeLayout.copy(baseDir = dir)
    _             <- Layer.saveFuryConf(FuryConf(layerRef, ImportPath.Root, resolved.publishedLayer), layout)
    _             <- Bsp.createConfig(layout)
    _             <- ~log.info(msg"Cloned layer $layerRef into ${dir.relativizeTo(pwd)}")
  } yield log.await()

  def publish: Try[ExitStatus] = for {
    layout        <- cli.layout
    cli           <- cli.hint(RemoteLayerArg)
    cli           <- cli.hint(RawArg)
    cli           <- cli.hint(BreakingArg)
    call          <- cli.call()
    conf          <- Layer.readFuryConf(layout)
    layer         <- Layer.read(layout, conf)
    path          <- call(RemoteLayerArg)
    breaking      <- ~call(BreakingArg).isSuccess
    raw           <- ~call(RawArg).isSuccess
    ref           <- Layer.share(layer, layout, raw)
    pub           <- Service.publish(ref.key, path, raw, breaking)
    _             <- if(raw) ~log.rawln(str"${ref.uri}") else ~log.info(msg"Shared at ${ref.uri}")
    _             <- if(raw) ~log.rawln(str"${pub.url}")
                     else ~log.info(msg"Published version ${pub.version} to ${pub.url}")
    _             <- Layer.saveFuryConf(FuryConf(Layer.digestLayer(layer), ImportPath.Root, Some(pub)), layout)
  } yield log.await()

  def share: Try[ExitStatus] = for {
    layout        <- cli.layout
    cli           <- cli.hint(RawArg)
    conf          <- Layer.readFuryConf(layout)
    layer         <- Layer.read(layout, conf)
    call          <- cli.call()
    raw           <- ~call(RawArg).isSuccess
    ref           <- Layer.share(layer, layout, raw)
    _             <- if(raw) ~log.rawln(str"${ref.uri}") else ~log.info(msg"Shared at ${ref.uri}")
  } yield log.await()

  def export: Try[ExitStatus] = for {
    layout        <- cli.layout
    cli           <- cli.hint(FileArg)
    conf          <- Layer.readFuryConf(layout)
    layer         <- Layer.read(layout, conf)
    call          <- cli.call()
    pwd           <- cli.pwd
    destination   <- call(FileArg).map(pwd.resolve(_))
    _             <- Layer.export(layer, layout, destination)
    _             <- ~log.info(msg"Saved layer file ${destination}")
  } yield log.await()

  def addImport: Try[ExitStatus] = for {
    layout        <- cli.layout
    conf          <- Layer.readFuryConf(layout)
    layer         <- Layer.read(layout, conf)
    cli           <- cli.hint(ImportNameArg)
    schemaArg     <- ~Some(SchemaId.default)
    defaultSchema <- ~layer.schemas.findBy(schemaArg.getOrElse(layer.main)).toOption
    
    cli           <- cli.hint(ImportArg, Layer.pathCompletions().getOrElse(Nil))
    layerImport   <- ~cli.peek(ImportArg)
    layerRef      <- ~layerImport.flatMap(Layer.parse(_, layout).flatMap(Layer.load(_, layout)).toOption)
    maybeLayer    <- ~layerRef.flatMap(Layer.read(_, layout).toOption)
    call          <- cli.call()
    layerImport   <- call(ImportArg)
    layerInput    <- Layer.parse(layerImport, layout)
    nameArg       <- cli.peek(ImportNameArg).orElse(layerInput.suggestedName).ascribe(MissingArg("name"))
    layerRef      <- Layer.load(layerInput, layout)
    schemaRef     <- ~SchemaRef(nameArg, layerRef, SchemaId.default)
    layer         <- Lenses.updateSchemas(layer)(Lenses.layer.imports(_))(_.modify(_)(_ +
                          schemaRef.copy(id = nameArg)))
    
    _             <- Layer.save(layer, layout)
  } yield log.await()

  def unimport: Try[ExitStatus] = for {
    layout    <- cli.layout
    conf          <- Layer.readFuryConf(layout)
    layer     <- Layer.read(layout, conf)
    schemaArg <- ~Some(SchemaId.default)
    dSchema   <- ~layer.schemas.findBy(schemaArg.getOrElse(layer.main)).toOption
    cli       <- cli.hint(ImportIdArg, dSchema.map(_.imports.map(_.id)).getOrElse(Nil))
    call      <- cli.call()
    schemaId  <- ~SchemaId.default
    importArg <- call(ImportIdArg)
    schema    <- layer.schemas.findBy(schemaId)
    lens      <- ~Lenses.layer.imports(schema.id)
    layer     <- ~lens.modify(layer)(_.filterNot(_.id == importArg))
    _         <- Layer.save(layer, layout)
  } yield log.await()

  def undo: Try[ExitStatus] = for {
    call      <- cli.call()
    layout    <- cli.layout
    _         <- Layer.undo(layout)
  } yield log.await()

  def list: Try[ExitStatus] = {
    for {
      layout    <- cli.layout
      conf      <- Layer.readFuryConf(layout)
      layer     <- Layer.read(layout, conf)
      cli       <- cli.hint(HttpsArg)
      schemaArg <- ~SchemaId.default
      schema    <- layer.schemas.findBy(schemaArg)
      cli       <- cli.hint(RawArg)
      table     <- ~Tables().imports
      cli       <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
      cli       <- cli.hint(ImportArg, schema.imports.map(_.id))
      call      <- cli.call()
      col       <- ~cli.peek(ColumnArg)
      importId  <- ~cli.peek(ImportArg)
      raw       <- ~call(RawArg).isSuccess
      https     <- ~call(HttpsArg).isSuccess
      rows      <- ~schema.imports.to[List].map { i => (i, schema.resolve(i, layout, https)) }
      table     <- ~Tables().show(table, cli.cols, rows, raw, col, importId, "import")
      _         <- ~log.infoWhen(!raw)(conf.focus())
      _         <- ~log.rawln(table)
    } yield log.await()
  }
}
