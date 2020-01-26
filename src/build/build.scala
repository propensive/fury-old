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

object ConfigCli {
  def set(cli: Cli)(implicit log: Log): Try[ExitStatus] = for {
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

  def auth(cli: Cli)(implicit log: Log): Try[ExitStatus] = for {
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

object AboutCli {

  private def resources: String = {
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

  private def tasks: String = Lifecycle.sessions.map { session =>
    str"[${session.pid}] started ${since(session.started)} ago: ${session.cli.args.args.mkString(" ")}"
  }.mkString("\n")

  private def connections: String = BloopServer.workDirectories.map { dir =>
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

  def resources(cli: Cli)(implicit log: Log): Try[ExitStatus] =
    cli.call().map{ _ =>
      log.raw(withTemplate(resources))
      log.await()
    }

  def tasks(cli: Cli)(implicit log: Log): Try[ExitStatus] =
    cli.call().map{ _ =>
      log.raw(withTemplate(tasks))
      log.await()
    }

  def connections(cli: Cli)(implicit log: Log): Try[ExitStatus] =
    cli.call().map{ _ =>
      log.raw(withTemplate(connections))
      log.await()
    }

}

object AliasCli {
  def list(cli: Cli)(implicit log: Log): Try[ExitStatus] = for {
    layout <- cli.layout
    conf   <- Layer.readFuryConf(layout)
    layer  <- Layer.read(layout, conf)
    cli    <- cli.hint(RawArg)
    call   <- cli.call()
    raw    <- ~call(RawArg).isSuccess
    rows   <- ~layer.aliases.to[List]
    table  <- ~Tables().show(Tables().aliases, cli.cols, rows, raw)(identity(_))
    _      <- ~log.infoWhen(!raw)(conf.focus())
    _      <- ~log.rawln(table.join("\n"))
  } yield log.await()

  def remove(cli: Cli)(implicit log: Log): Try[ExitStatus] = for {
    layout     <- cli.layout
    conf       <- Layer.readFuryConf(layout)
    layer      <- Layer.read(layout, conf)
    cli        <- cli.hint(AliasArg, layer.aliases.map(_.cmd))
    call       <- cli.call()
    aliasArg   <- call(AliasArg)
    aliasToDel <- ~layer.aliases.find(_.cmd == aliasArg)
    layer      <- Lenses.updateSchemas(layer) { s => Lenses.layer.aliases } (_(_) --= aliasToDel)
    _          <- ~Layer.save(layer, layout)
  } yield log.await()

  def add(cli: Cli)(implicit log: Log): Try[ExitStatus] = for {
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
    project          <- optProject.ascribe(UnspecifiedProject())
    module           <- project.modules.findBy(moduleArg)
    moduleRef        <- ~module.ref(project)
    aliasArg         <- call(AliasArg)
    description      <- call(DescriptionArg)
    alias            <- ~Alias(aliasArg, description, optSchemaArg, moduleRef)
    layer            <- Lenses.updateSchemas(layer) { s => Lenses.layer.aliases } (_(_) += alias)
    _                <- ~Layer.save(layer, layout)
  } yield log.await()
}

object BuildCli {

  def notImplemented(cli: Cli)(implicit log: Log): Try[ExitStatus] = Success(Abort)

  def getPrompt(layer: Layer, theme: Theme)(implicit log: Log): Try[String] = for {
    schemaId     <- ~layer.main
    schema       <- layer.schemas.findBy(schemaId)
    optProjectId <- ~schema.main
    optProject   <- ~optProjectId.flatMap(schema.projects.findBy(_).toOption)
    optModuleId  <- ~optProject.flatMap(_.main)
    optModule    <- ~optModuleId.flatMap { mId => optProject.flatMap(_.modules.findBy(mId).toOption) }
  } yield Prompt.zsh(layer, optProject, optModule)(theme)

  def upgrade(cli: Cli)(implicit log: Log): Try[ExitStatus] = Installation.tmpFile { tmpFile => for {
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

  def prompt(cli: Cli)(implicit log: Log): Try[ExitStatus] = for {
    layout <- cli.layout
    conf   <- Layer.readFuryConf(layout)
    layer  <- Layer.read(layout, conf)
    layout <- cli.layout
    conf   <- Layer.readFuryConf(layout)
    layer  <- ~Layer.read(layout, conf).toOption
    msg    <- layer.fold(Try(Prompt.empty(ManagedConfig().theme)))(getPrompt(_, ManagedConfig().theme))
    call   <- cli.call()
    _      <- ~log.raw(msg)
  } yield log.await()

  def compile(moduleRef: Option[ModuleRef])(cli: Cli)(implicit log: Log): Try[ExitStatus] = for {
    layout         <- cli.layout
    conf           <- Layer.readFuryConf(layout)
    layer          <- Layer.read(layout, conf)
    cli            <- cli.hint(HttpsArg)
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
    project        <- optProject.ascribe(UnspecifiedProject())
    optModuleId    <- ~call(ModuleArg).toOption.orElse(moduleRef.map(_.moduleId)).orElse(project.main)
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
            _              <- ~(dir.foreach { dir => compilation.saveJars(module.ref(project), compileSuccess.classDirectories,
                                  dir in layout.pwd, layout, fatJar)
                              })
          } yield compileSuccess
        }
      }
    }
    future        <- if(watch) Try(r.start()).flatten else r.action()
  } yield {
    val result = Await.result(future, duration.Duration.Inf)
    log.await(result.isSuccessful)
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
      f(())
    }
  }

  def install(cli: Cli)(implicit log: Log): Try[ExitStatus] = for {
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
    project      <- optProject.ascribe(UnspecifiedProject())
    optModuleId  <- ~call(ModuleArg).toOption.orElse(project.main)
    optModule    <- ~optModuleId.flatMap(project.modules.findBy(_).toOption)
    module       <- optModule.ascribe(UnspecifiedModule())
    
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

  def console(cli: Cli)(implicit log: Log): Try[ExitStatus] = for {
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

  def classpath(cli: Cli)(implicit log: Log): Try[ExitStatus] = for {
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
    project      <- optProject.ascribe(UnspecifiedProject())
    module       <- optModule.ascribe(UnspecifiedModule())
    
    compilation  <- Compilation.syncCompilation(schema, module.ref(project), layout,
                        https)
    
    classpath    <- ~compilation.classpath(module.ref(project), layout)
  } yield {
    val separator = if(singleColumn) "\n" else ":"
    log.rawln(classpath.map(_.value).join(separator))
    log.await()
  }

  def describe(cli: Cli)(implicit log: Log): Try[ExitStatus] = for {
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
    project      <- optProject.ascribe(UnspecifiedProject())
    module       <- optModule.ascribe(UnspecifiedModule())

    compilation  <- Compilation.syncCompilation(schema, module.ref(project), layout,
                        https)
    
    _            <- ~Graph.draw(compilation.graph.links, true,
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
  def init(cli: Cli)(implicit log: Log): Try[ExitStatus] = for {
    layout <- cli.newLayout
    call   <- cli.call()
    _      <- Layer.init(layout)
  } yield log.await()

  def projects(cli: Cli)(implicit log: Log): Try[ExitStatus] = for {
    layout    <- cli.layout
    conf      <- Layer.readFuryConf(layout)
    layer     <- Layer.read(layout, conf)
    cli       <- cli.hint(HttpsArg)
    schemaArg <- ~SchemaId.default
    schema    <- layer.schemas.findBy(schemaArg)
    cli       <- cli.hint(RawArg)
    call      <- cli.call()
    raw       <- ~call(RawArg).isSuccess
    https     <- ~call(HttpsArg).isSuccess
    projects  <- schema.allProjects(layout, https)
    table     <- ~Tables().show(Tables().projects(None), cli.cols, projects.distinct, raw)(_.id)
    _         <- ~log.infoWhen(!raw)(conf.focus())
    _         <- ~log.rawln(table.mkString("\n"))
  } yield log.await()

  def select(cli: Cli)(implicit log: Log): Try[ExitStatus] = for {
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

  def extract(cli: Cli)(implicit log: Log): Try[ExitStatus] = for {
    cli      <- cli.hint(DirArg)
    cli      <- cli.hint(FileArg)
    call     <- cli.call()
    pwd      <- cli.pwd
    file     <- call(FileArg).map(pwd.resolve(_))
    dir      <- ~cli.peek(DirArg).map(pwd.resolve(_)).getOrElse(pwd)
    layout   <- cli.newLayout.map(_.copy(baseDir = dir))
    layerRef <- Layer.loadFile(file, layout)
    _        <- Layer.saveFuryConf(FuryConf(layerRef, ImportPath.Root), layout)
  } yield log.await()

  def clone(cli: Cli)(implicit log: Log): Try[ExitStatus] = for {
    cli           <- cli.hint(DirArg)
    cli           <- cli.hint(ImportArg, Layer.pathCompletions().getOrElse(Nil))
    call          <- cli.call()
    layout        <- cli.newLayout
    layerImport   <- call(ImportArg)
    resolved      <- Layer.parse(layerImport, layout)
    layerRef      <- Layer.load(resolved, layout)
    dir           <- call(DirArg).pacify(resolved.suggestedName.map { n => Path(n.key) })
    pwd           <- cli.pwd
    dir           <- ~pwd.resolve(dir)
    _             <- ~dir.mkdir()

    _             <- Layer.saveFuryConf(FuryConf(layerRef, ImportPath.Root, resolved.publishedLayer),
                         dir / ".fury.conf")

    _             <- ~log.info(msg"Cloned layer $layerRef into ${dir.relativizeTo(pwd)}")
  } yield log.await()

  def publish(cli: Cli)(implicit log: Log): Try[ExitStatus] = for {
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

    _             <- Layer.saveFuryConf(FuryConf(Layer.digestLayer(layer), ImportPath.Root, Some(pub)),
                         layout.confFile)

  } yield log.await()

  def share(cli: Cli)(implicit log: Log): Try[ExitStatus] = for {
    layout        <- cli.layout
    cli           <- cli.hint(RawArg)
    conf          <- Layer.readFuryConf(layout)
    layer         <- Layer.read(layout, conf)
    call          <- cli.call()
    raw           <- ~call(RawArg).isSuccess
    ref           <- Layer.share(layer, layout, raw)
    _             <- if(raw) ~log.rawln(str"${ref.uri}") else ~log.info(msg"Shared at ${ref.uri}")
  } yield log.await()

  def export(cli: Cli)(implicit log: Log): Try[ExitStatus] = for {
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

  def addImport(cli: Cli)(implicit log: Log): Try[ExitStatus] = for {
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
    
    _             <- ~Layer.save(layer, layout)
  } yield log.await()

  def unimport(cli: Cli)(implicit log: Log): Try[ExitStatus] = for {
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
    _         <- ~Layer.save(layer, layout)
  } yield log.await()

  def list(cli: Cli)(implicit log: Log): Try[ExitStatus] = {
    for {
      layout    <- cli.layout
      conf      <- Layer.readFuryConf(layout)
      layer     <- Layer.read(layout, conf)
      cli       <- cli.hint(HttpsArg)
      schemaArg <- ~SchemaId.default
      schema    <- layer.schemas.findBy(schemaArg)
      cli       <- cli.hint(RawArg)
      call      <- cli.call()
      raw       <- ~call(RawArg).isSuccess
      https     <- ~call(HttpsArg).isSuccess
      rows      <- ~schema.imports.to[List].map { i => (i, schema.resolve(i, layout, https)) }
      table     <- ~Tables().show(Tables().imports, cli.cols, rows, raw)(_._1.schema.key)
      _         <- ~log.infoWhen(!raw)(conf.focus())
      _         <- ~log.rawln(table.mkString("\n"))
    } yield log.await()
  }
}
