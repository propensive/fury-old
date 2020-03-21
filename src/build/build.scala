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

import fury.strings._, fury.core._, fury.model._, fury.io._, fury.utils._

import exoskeleton._
import euphemism._
import guillotine._

import Args._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._, duration._
import scala.util._
import java.text.DecimalFormat

import language.higherKinds
import scala.util.control.NonFatal

case class ConfigCli(cli: Cli)(implicit log: Log) {
  def set: Try[ExitStatus] = for {
    cli      <- cli.hint(ThemeArg, Theme.all)
    cli      <- cli.hint(TimestampsArg, List("on", "off"))
    cli      <- cli.hint(PipeliningArg, List("on", "off"))
    cli      <- cli.hint(TraceArg, List("on", "off"))
    cli      <- cli.hint(NoIpfsArg, List("on", "off"))
    cli      <- cli.hint(ServiceArg, List("furore.dev"))
    call     <- cli.call()
    newTheme <- ~call(ThemeArg).toOption
    timestamps <- ~call(TimestampsArg).toOption
    pipelining <- ~call(PipeliningArg).toOption
    trace    <- ~call(TraceArg).toOption
    service  <- ~call(ServiceArg).toOption
    noIpfs   <- ~call(NoIpfsArg).toOption
    config   <- ~ManagedConfig()
    config   <- ~newTheme.map { th => config.copy(theme = th) }.getOrElse(config)
    config   <- ~service.map { s => config.copy(service = s) }.getOrElse(config)
    config   <- ~timestamps.map { ts => config.copy(timestamps = ts) }.getOrElse(config)
    config   <- ~pipelining.map { p => config.copy(pipelining = p) }.getOrElse(config)
    config   <- ~trace.map { t => config.copy(trace = t) }.getOrElse(config)
    config   <- ~noIpfs.map { x => config.copy(skipIpfs = x) }.getOrElse(config)
    _        <- ManagedConfig.write(config)
  } yield log.await()

  def install: Try[ExitStatus] = for {
    cli   <- cli.hint(ForceArg)
    call  <- cli.call()
    force <- ~call(ForceArg).isSuccess
    _     <- Install(cli.env, force)
  } yield log.await()

  def auth: Try[ExitStatus] = for {
    call     <- cli.call()
    token    <- doAuth
    config   <- ~ManagedConfig().copy(token = Some(token))
    _        <- ~ManagedConfig.write(config)
    _        <- ~log.info("You are now authenticated")
  } yield log.await()

  def doAuth: Try[OauthToken] = for {
    code     <- ~Rnd.token(18)
    // These futures should be managed in the session
    uri      <- ~Https(Path(ManagedConfig().service) / "await", Query & "code" -> code)
    future   <- ~Future(blocking(Http.get(uri, Set())))
    uri      <- ~Https(Path(ManagedConfig().service) / "auth", Query & "code" -> code)
    _        <- ~log.info(msg"Please visit $uri to authenticate using GitHub.")
    _        <- ~Future(blocking(Shell(cli.env).tryXdgOpen(uri)))
    response <- Await.result(future, Duration.Inf)
    json     <- ~Json.parse(new String(response, "UTF-8")).get
    token    <- ~json.token.as[String].get
  } yield OauthToken(token)

  def software: Try[ExitStatus] = for {
    layout <- cli.layout
    conf   <- Layer.readFuryConf(layout)
    layer  <- Layer.retrieve(conf)
    cli    <- cli.hint(RawArg)
    table  <- ~Tables().software(cli.env)
    cli    <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
    call   <- cli.call()
    col    <- ~cli.peek(ColumnArg)
    raw    <- ~call(RawArg).isSuccess
    rows   <- ~Software.all
    table  <- ~Tables().show[Software, Boolean](table, cli.cols, rows, raw, col)
    _      <- ~log.infoWhen(!raw)(conf.focus())
    _      <- ~log.rawln(table)
  } yield log.await()

}

case class AboutCli(cli: Cli)(implicit log: Log) {

  def version: Try[ExitStatus] = for {
    call <- cli.call()
    _    <- ~log.raw(str"fury version ${FuryVersion.current}")
  } yield log.await()

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
  }.join("\n")

  private def connectionsString: String = BloopServer.workDirectories.map { dir => str"$dir" }.join("\n")

  private def withTemplate(content: String): String = {
    str"""|     _____
          |    / ___/__ __ ____ __ __
          |   / __/ / // // ._// // /
          |  /_/    \_._//_/  _\_. /
          |                   \___/
          |
          |Fury build tool, version ${FuryVersion.current}, built ${FuryVersion.built}
          |This software is provided under the Apache 2.0 License.
          |Fury depends on Bloop, Coursier, Git and Nailgun.
          |© Copyright 2018-20 Jon Pretty, Propensive OÜ.
          |
          |See the Fury website at https://fury.build/, or follow @propensive on Twitter
          |for more information.
          |
          |${content}
          |
          |For help on using Fury, run: fury help
          |""".stripMargin
  }

  def resources: Try[ExitStatus] = cli.call().map { _ =>
    log.raw(withTemplate(resourcesString))
    log.await()
  }

  def tasks: Try[ExitStatus] = cli.call().map{ _ =>
    log.raw(withTemplate(tasksString))
    log.await()
  }

  def connections: Try[ExitStatus] = cli.call().map{ _ =>
    log.raw(withTemplate(connectionsString))
    log.await()
  }

}

case class AliasCli(cli: Cli)(implicit log: Log) {
  def list: Try[ExitStatus] = for {
    layout <- cli.layout
    conf   <- Layer.readFuryConf(layout)
    layer  <- Layer.retrieve(conf)
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
    layer      <- Layer.retrieve(conf)
    cli        <- cli.hint(AliasArg, layer.aliases.map(_.cmd))
    call       <- cli.call()
    aliasArg   <- call(AliasArg)
    aliasToDel <- ~layer.aliases.find(_.cmd == aliasArg)
    layer      <- ~Layer(_.aliases).modify(layer)(_ -- aliasToDel)
    _          <- Layer.commit(layer, conf, layout)
  } yield log.await()

  def add: Try[ExitStatus] = for {
    layout           <- cli.layout
    conf             <- Layer.readFuryConf(layout)
    layer            <- Layer.retrieve(conf)
    cli              <- cli.hint(AliasArg)
    cli              <- cli.hint(DescriptionArg)
    cli              <- cli.hint(ProjectArg, layer.projects)
    optProjectId     <- ~cli.peek(ProjectArg)
    
    optProject       <- ~optProjectId.orElse(layer.main).flatMap { id =>
                            layer.projects.findBy(id).toOption }.to[List].headOption
    
    cli              <- cli.hint(ModuleArg, optProject.map(_.modules).getOrElse(Nil))
    call             <- cli.call()
    moduleArg        <- call(ModuleArg)
    project          <- optProject.asTry
    module           <- project.modules.findBy(moduleArg)
    moduleRef        <- ~module.ref(project)
    aliasArg         <- call(AliasArg)
    description      <- call(DescriptionArg)
    alias            <- ~Alias(aliasArg, description, moduleRef, call.suffix)
    layer            <- ~Layer(_.aliases).modify(layer)(_ + alias)
    _                <- Layer.commit(layer, conf, layout)
  } yield log.await()
}

case class BuildCli(cli: Cli)(implicit log: Log) {

  def notImplemented: Try[ExitStatus] = Success(Abort)

  def upgrade: Try[ExitStatus] = Installation.tmpFile { tmpFile => for {
    layout <- cli.layout
    conf   <- Layer.readFuryConf(layout)
    layer  <- Layer.retrieve(conf)
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
    layer   <- Layer.retrieve(conf)
    project <- layer.mainProject
    module  <- ~project.flatMap(_.main)
    _       <- ~log.raw(Prompt.rewrite(conf.focus(project.fold(ProjectId("?"))(_.id), module.getOrElse(ModuleId("?"))).string(ManagedConfig().theme)))
  } yield log.await()

  def clean: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    cli          <- cli.hint(ProjectArg, layer.projects)
    optProjectId <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject   <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
    cli          <- cli.hint(ModuleArg, optProject.map(_.modules).getOrElse(Nil))
    moduleId     <- cli.preview(ModuleArg)(optProject.flatMap(_.main))
    call         <- cli.call()
    project      <- optProject.asTry
    module       <- project.modules.findBy(moduleId)
    moduleRef    =  module.ref(project)

    compilation  <- Compilation.syncCompilation(layer, moduleRef, layout, https = false, noSecurity = true)

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
    layer          <- Layer.retrieve(conf)
    cli            <- cli.hint(HttpsArg)
    cli            <- cli.hint(WaitArg)
    cli            <- cli.hint(NoSecurityArg)
    cli            <- cli.hint(WatchArg)
    cli            <- cli.hint(ProjectArg, layer.projects)
    autocProjectId <- ~cli.peek(ProjectArg).orElse(moduleRef.map(_.projectId)).orElse(layer.main)
    cli            <- cli.hint(PipeliningArg, List("on", "off"))
    autocProject   <- ~autocProjectId.flatMap(layer.projects.findBy(_).toOption)
    cli            <- cli.hint(ModuleArg, autocProject.to[List].flatMap(_.modules))
    cli            <- cli.hint(DirArg)
    cli            <- cli.hint(FatJarArg)
    cli            <- cli.hint(ReporterArg, Reporter.all)
    call           <- cli.call()
    dir            <- ~call(DirArg).toOption
    https          <- ~call(HttpsArg).isSuccess
    optProjectId   <- ~cli.peek(ProjectArg).orElse(moduleRef.map(_.projectId).orElse(layer.main))
    optProject     <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
    project        <- optProject.asTry
    moduleId       <- moduleRef.map(~_.moduleId).getOrElse(cli.preview(ModuleArg)(project.main))
    module         <- project.modules.findBy(moduleId)
    pipelining     <- ~call(PipeliningArg).toOption
    fatJar         =  call(FatJarArg).isSuccess
    globalPolicy   <- ~Policy.read(log)
    reporter       <- ~call(ReporterArg).toOption.getOrElse(GraphReporter)
    watch          =  call(WatchArg).isSuccess
    waiting        =  call(WaitArg).isSuccess
    noSecurity     =  call(NoSecurityArg).isSuccess
    _              <- onlyOne(watch, waiting)
    compilation    <- Compilation.syncCompilation(layer, module.ref(project), layout, https, noSecurity)
    r              =  repeater(compilation.allSources, waiting) {
      for {
        task <- compileOnce(compilation, layer, module.ref(project), layout,
          globalPolicy, if(call.suffix.isEmpty) args else call.suffix, pipelining.getOrElse(ManagedConfig().pipelining), reporter, ManagedConfig().theme, https, noSecurity)
      } yield {
        task.transform { completed =>
          for {
            compileResult  <- completed
            compileSuccess <- compileResult.asTry
            _              <- (dir.map { dir => compilation.saveJars(module.ref(project),
                                  compileSuccess.classDirectories, dir in layout.pwd, layout, fatJar)
                              }).getOrElse(Success(()))
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
    layer        <- Layer.retrieve(conf)
    cli          <- cli.hint(HttpsArg)
    cli          <- cli.hint(ProjectArg, layer.projects)
    optProjectId <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject   <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
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
    compilation  <- Compilation.syncCompilation(layer, module.ref(project), layout, https, false)
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
    layer        <- Layer.retrieve(conf)
    cli          <- cli.hint(HttpsArg)
    cli          <- cli.hint(NoSecurityArg)
    cli          <- cli.hint(ProjectArg, layer.projects)
    optProjectId <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject   <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
    cli          <- cli.hint(ModuleArg, optProject.map(_.modules).getOrElse(Nil))
    moduleId     <- cli.preview(ModuleArg)(optProject.flatMap(_.main))
    call         <- cli.call()
    https        <- ~call(HttpsArg).isSuccess
    noSecurity   <- ~call(NoSecurityArg).isSuccess
    project      <- optProject.asTry
    module       <- project.modules.findBy(moduleId)
    
    compilation  <- Compilation.syncCompilation(layer, module.ref(project), layout,
                        https, noSecurity)
    
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
    layer        <- Layer.retrieve(conf)
    cli          <- cli.hint(HttpsArg)
    cli          <- cli.hint(ProjectArg, layer.projects)
    optProjectId <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject   <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
    cli          <- cli.hint(ModuleArg, optProject.map(_.modules).getOrElse(Nil))
    moduleId     <- cli.preview(ModuleArg)(optProject.flatMap(_.main))
    cli          <- cli.hint(SingleColumnArg)
    call         <- cli.call()
    https        <- ~call(HttpsArg).isSuccess
    singleColumn <- ~call(SingleColumnArg).isSuccess
    project      <- optProject.asTry
    module       <- project.modules.findBy(moduleId)
    
    compilation  <- Compilation.syncCompilation(layer, module.ref(project), layout,
                        https, false)
    
    classpath    <- ~compilation.classpath(module.ref(project), layout)
  } yield {
    val separator = if(singleColumn) "\n" else ":"
    log.rawln(classpath.map(_.value).join(separator))
    log.await()
  }

  def describe: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    cli          <- cli.hint(HttpsArg)
    cli          <- cli.hint(ProjectArg, layer.projects)
    optProjectId <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject   <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
    cli          <- cli.hint(ModuleArg, optProject.map(_.modules).getOrElse(Nil))
    call         <- cli.call()
    https        <- ~call(HttpsArg).isSuccess
    moduleId     <- cli.preview(ModuleArg)(optProject.flatMap(_.main))
    project      <- optProject.asTry
    module       <- project.modules.findBy(moduleId)

    compilation  <- Compilation.syncCompilation(layer, module.ref(project), layout,
                        https, false)
    
    _            <- ~UiGraph.draw(compilation.graph.links, true,
                        Map())(ManagedConfig().theme).foreach(log.info(_))

  } yield log.await()

  private[this] def compileOnce(compilation: Compilation,
                                layer: Layer,
                                moduleRef: ModuleRef,
                                layout: Layout,
                                globalPolicy: Policy,
                                compileArgs: List[String],
                                pipelining: Boolean,
                                reporter: Reporter,
                                theme: Theme,
                                https: Boolean,
                                noSecurity: Boolean)
                               (implicit log: Log): Try[Future[CompileResult]] = {
    for {
      _            <- compilation.checkoutAll(layout, https)
    } yield {
      val multiplexer = new Multiplexer[ModuleRef, CompileEvent](compilation.targets.map(_._1).to[Set])
      Lifecycle.currentSession.multiplexer = multiplexer
      val future = compilation.compile(moduleRef, Map(), layout,
        globalPolicy, compileArgs, pipelining, noSecurity).apply(TargetId(moduleRef)).andThen {
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
    layer     <- Layer.retrieve(conf)
    cli       <- cli.hint(HttpsArg)
    cli       <- cli.hint(RawArg)
    cli       <- cli.hint(ProjectArg, layer.projects.map(_.id))
    table     <- ~Tables().projects(None)
    cli       <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
    call      <- cli.call()
    col       <- ~cli.peek(ColumnArg)
    projectId <- ~cli.peek(ProjectArg)
    raw       <- ~call(RawArg).isSuccess
    https     <- ~call(HttpsArg).isSuccess
    projects  <- layer.allProjects(layout, https)
    table     <- ~Tables().show(table, cli.cols, projects.distinct, raw, col, projectId, "project")
    _         <- ~log.infoWhen(!raw)(conf.focus())
    _         <- ~log.rawln(table)
  } yield log.await()

  def select: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    baseLayer    <- Layer.get(conf.layerRef)
    layer        <- Layer.retrieve(conf)
    focus        <- Layer.readFuryConf(layout)
    currentLayer <- ~Some(focus.path)
    absTree      <- ~baseLayer.importTree.getOrElse(Nil)
    relLayers    <- ~layer.imports.map { sr => ImportPath(sr.id.key) }
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

  def cloneLayer: Try[ExitStatus] = for {
    cli        <- cli.hint(DirArg)
    cli        <- cli.hint(EditorArg)
    cli        <- cli.hint(DocsArg)
    cli        <- cli.hint(ImportArg, Layer.pathCompletions().getOrElse(Nil))
    call       <- cli.call()
    edit       <- ~call(EditorArg).isSuccess
    useDocsDir <- ~call(DocsArg).isSuccess
    layerName  <- call(ImportArg)
    layerRef   <- Layer.resolve(layerName)
    layer      <- Layer.get(layerRef)
    dir        <- call(DirArg).pacify(layerName.suggestedName.map { n => Path(n.key) })
    pwd        <- cli.pwd
    dir        <- ~(if(useDocsDir) Xdg.docsDir else pwd).resolve(dir).uniquify()
    _          <- ~log.info(msg"Cloning layer $layerName into ${if(useDocsDir) dir else dir.relativizeTo(pwd)}")
    _          <- ~dir.mkdir()
    newLayout  <- cli.newLayout
    layout     =  newLayout.copy(baseDir = dir)
    optRepo    <- ~layer.mainRepo.flatMap(layer.repos.findBy(_).toOption)
    _          <- optRepo.fold(Try(()))(_.doCleanCheckout(layout, true))
    _          <- ~log.info(msg"Saving Fury configuration file ${layout.confFile.relativizeTo(layout.pwd)}")
    _          <- Layer.saveFuryConf(FuryConf(layerRef, ImportPath.Root, layerName.publishedLayer), layout)
    _          <- Bsp.createConfig(layout)
    _          <- ~log.info(msg"Cloning complete")
    
    _          <- if(edit) VsCodeSoftware.installedPath(cli.env, false).flatMap { path =>
                    implicit val env: Environment = cli.env
                    sh"${path.value} ${dir.value}".exec[Try[String]]
                  }
                  else Success(())
  } yield log.await()

  def publish: Try[ExitStatus] = for {
    layout        <- cli.layout
    cli           <- cli.hint(RemoteLayerArg)
    cli           <- cli.hint(RawArg)
    cli           <- cli.hint(BreakingArg)
    cli           <- cli.hint(PublicArg)
    call          <- cli.call()
    token         <- ManagedConfig().token.ascribe(NotAuthenticated()).orElse(ConfigCli(cli).doAuth)
    conf          <- Layer.readFuryConf(layout)
    layer         <- Layer.retrieve(conf)
    remoteLayerId <- call(RemoteLayerArg)
    breaking      <- ~call(BreakingArg).isSuccess
    public        <- ~call(PublicArg).isSuccess
    raw           <- ~call(RawArg).isSuccess
    hashes        <- Layer.hashes(layer)
    ref           <- Layer.share(ManagedConfig().service, layer, token)
    pub           <- Service.publish(ManagedConfig().service, ref.ipfsRef, remoteLayerId.group,
                         Some(remoteLayerId.name), false, breaking, public,
                         conf.published.fold(0)(_.version.major), conf.published.fold(0)(_.version.minor),
                         token, hashes)
    _             <- if(raw) ~log.rawln(str"${ref} ${pub}") else {
                       log.info(msg"Shared at ${ref}")
                       ~log.info(msg"Published version ${pub.version} to ${pub.url}")
                     }
    _             <- Layer.saveFuryConf(FuryConf(ref, ImportPath.Root, Some(pub)), layout)
  } yield log.await()

  def share: Try[ExitStatus] = for {
    layout        <- cli.layout
    cli           <- cli.hint(RawArg)
    conf          <- Layer.readFuryConf(layout)
    layer         <- Layer.retrieve(conf)
    call          <- cli.call()
    raw           <- ~call(RawArg).isSuccess
    token         <- ManagedConfig().token.ascribe(NotAuthenticated()).orElse(ConfigCli(cli).doAuth)
    ref           <- Layer.share(ManagedConfig().service, layer, token)
    _             <- if(raw) ~log.rawln(str"${ref}") else ~log.info(msg"Shared at ${ref}")
  } yield log.await()

  /*def export: Try[ExitStatus] = for {
    layout        <- cli.layout
    cli           <- cli.hint(FileArg)
    conf          <- Layer.readFuryConf(layout)
    layer         <- Layer.retrieve(conf)
    call          <- cli.call()
    pwd           <- cli.pwd
    destination   <- call(FileArg).map(pwd.resolve(_))
    _             <- Layer.export(layer, layout, destination)
    _             <- ~log.info(msg"Saved layer file ${destination}")
  } yield log.await()*/

  def addImport: Try[ExitStatus] = for {
    layout        <- cli.layout
    conf          <- Layer.readFuryConf(layout)
    layer         <- Layer.retrieve(conf)
    cli           <- cli.hint(ImportNameArg)
    cli           <- cli.hint(ImportArg, Layer.pathCompletions().getOrElse(Nil))
    call          <- cli.call()
    layerName     <- call(ImportArg)
    nameArg       <- cli.peek(ImportNameArg).orElse(layerName.suggestedName).ascribe(MissingArg("name"))
    newLayerRef   <- Layer.resolve(layerName)
    newLayer      <- Layer.get(newLayerRef)

    pub           <- ~(layerName match {
                       case u: FuryUri => Some(PublishedLayer(u, LayerVersion.Zero))
                       case _          => None
                     })

    ref           <- ~Import(nameArg, newLayerRef, pub)
    layer         <- ~Layer(_.imports).modify(layer)(_ + ref.copy(id = nameArg))
    _             <- Layer.commit(layer, conf, layout)
  } yield log.await()

  def unimport: Try[ExitStatus] = for {
    layout    <- cli.layout
    conf          <- Layer.readFuryConf(layout)
    layer     <- Layer.retrieve(conf)
    cli       <- cli.hint(ImportIdArg, layer.imports.map(_.id))
    call      <- cli.call()
    importArg <- call(ImportIdArg)
    layer     <- ~Layer(_.imports).modify(layer)(_.evict(importArg))
    _         <- Layer.commit(layer, conf, layout)
  } yield log.await()

  /*def undo: Try[ExitStatus] = for {
    call      <- cli.call()
    layout    <- cli.layout
    _         <- Layer.undo(layout)
  } yield log.await()*/

  def update: Try[ExitStatus] = for {
    layout    <- cli.layout
    conf      <- Layer.readFuryConf(layout)
    layer     <- Layer.retrieve(conf)
    cli       <- cli.hint(AllArg)
    cli       <- cli.hint(RecursiveArg)
    cli       <- cli.hint(ImportIdArg)
    call      <- cli.call()
    recursive <- call(RecursiveArg)
    all       <- ~call(AllArg).isSuccess
    importArg <- call(ImportIdArg)
    imported  <- layer.imports.findBy(importArg)
    published <- imported.remote.ascribe(ImportHasNoRemote())
    artifact  <- Service.latest(published.url.domain, published.url.path, Some(published.version))
    newPub    <- ~PublishedLayer(FuryUri(published.url.domain, published.url.path), artifact.version)
    layer     <- ~(Layer(_.imports(importArg).remote)(layer) = Some(newPub))
    _         <- Layer.commit(layer, conf, layout)
  } yield log.await()

  def list: Try[ExitStatus] = {
    for {
      layout    <- cli.layout
      conf      <- Layer.readFuryConf(layout)
      layer     <- Layer.retrieve(conf)
      cli       <- cli.hint(HttpsArg)
      cli       <- cli.hint(RawArg)
      table     <- ~Tables().imports
      cli       <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
      cli       <- cli.hint(ImportArg, layer.imports.map(_.id))
      call      <- cli.call()
      col       <- ~cli.peek(ColumnArg)
      importId  <- ~cli.peek(ImportArg)
      raw       <- ~call(RawArg).isSuccess
      https     <- ~call(HttpsArg).isSuccess
      rows      <- ~layer.imports.to[List].map { i => (i, Layer.get(i.layerRef, true)) }
      table     <- ~Tables().show(table, cli.cols, rows, raw, col, importId, "import")
      _         <- ~log.infoWhen(!raw)(conf.focus())
      _         <- ~log.rawln(table)
    } yield log.await()
  }
}
