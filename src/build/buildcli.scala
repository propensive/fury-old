/*

    Fury, version 0.33.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury

import fury.text._, fury.core._, fury.model._, fury.io._, fury.utils._

import exoskeleton._
import euphemism._
import mercator._
import antiphony._
import guillotine._
import jovian._

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
    cli      <- cli.hint(ServiceArg, List("vent.dev"))
    cli      <- cli.hint(DefaultImportArg, Service.catalog(cli.peek(ServiceArg).getOrElse(ManagedConfig().service)).getOrElse(Nil))
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
    uri      <- ~(Https(ManagedConfig().service) / "await").query("code" -> code)
    future   <- ~Future(blocking(Http.get(uri.key, Set()).to[Try]))
    uri      <- ~(Https(ManagedConfig().service) / "auth").query("code" -> code)
    _        <- ~log.info(msg"Please visit $uri to authenticate using GitHub.")
    _        <- ~Future(blocking(Shell(cli.env).tryXdgOpen(uri)))
    response <- Await.result(future, Duration.Inf)
    json     <- Json.parse(new String(response, "UTF-8")).to[Try]
    token    <- json.token.as[String].to[Try]
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
    cli        <- cli.hint(AliasArg, layer.aliases.map(_.id))
    call       <- cli.call()
    aliasArg   <- call(AliasArg)
    aliasToDel <- ~layer.aliases.find(_.id == aliasArg)
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
  
  private val allReporters = Reporter.all.map(_.name).mkString(", ")
  val ReporterArg = CliParam[Reporter]('o', 'output, s"format for build output ($allReporters)")

  def notImplemented: Try[ExitStatus] = Success(Abort)

  def prompt: Try[ExitStatus] = for {
    layout  <- cli.layout
    conf    <- Layer.readFuryConf(layout)
    call    <- cli.call()
    layer   <- Layer.retrieve(conf)
    project <- layer.mainProject
    module  <- ~project.flatMap(_.main)

    _       <- ~log.raw(Prompt.rewrite(conf.focus(project.fold(ProjectId("?"))(_.id), module.getOrElse(
                   ModuleId("?"))).string(ManagedConfig().theme)))

  } yield log.await()

  def clean: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    (cli, tryProject, tryModule) <- cli.askProjectAndModule(layer)
    call         <- cli.call()
    project      <- tryProject
    module       <- tryModule
    moduleRef    =  module.ref(project)
    build        <- Build.syncBuild(layer, moduleRef, layout, noSecurity = true)
    result       <- new build.TargetExtras(build.target).cleanCache()
  } yield {
    Option(result.getMessage()).foreach(log.info(_))
    val success = result.getCleaned()
    if(success) log.note(msg"Cleaned compiler caches for $moduleRef and its dependencies")
    else log.warn(msg"Compiler caches for $moduleRef and its dependencies were not cleaned")
    log.await(success)
  }

  def compile(moduleRef: Option[ModuleRef], args: List[String] = Nil): Try[ExitStatus] = for {
    layout         <- cli.layout
    conf           <- Layer.readFuryConf(layout)
    layer          <- Layer.retrieve(conf)
    cli            <- cli.hint(WaitArg)
    cli            <- cli.hint(NoSecurityArg)
    cli            <- cli.hint(WatchArg)
    cli            <- cli.hint(ProjectArg, layer.projects)
    autocProjectId <- ~cli.peek(ProjectArg).orElse(moduleRef.map(_.projectId)).orElse(layer.main)
    cli            <- cli.hint(PipeliningArg, List("on", "off"))
    autocProject   <- ~autocProjectId.flatMap(layer.projects.findBy(_).toOption)
    cli            <- cli.hint(ModuleArg, autocProject.to[List].flatMap(_.modules))
    cli            <- cli.hint(PathArg)
    cli            <- cli.hint(FatJarArg)
    cli            <- cli.hint(JsArg)
    cli            <- cli.hint(ReporterArg, Reporter.all)
    call           <- cli.call()
    dir            <- ~call(PathArg).toOption
    optProjectId   <- ~cli.peek(ProjectArg).orElse(moduleRef.map(_.projectId).orElse(layer.main))
    projectId      <- optProjectId.asTry
    optProject     <- ~layer.projects.findBy(projectId).toOption
    project        <- optProject.ascribe(ItemNotFound(projectId))
    moduleId       <- moduleRef.map(~_.moduleId).getOrElse(cli.preview(ModuleArg)(project.main))
    module         <- project.modules.findBy(moduleId)
    pipelining     <- ~call(PipeliningArg).toOption
    output         <- call.atMostOne(FatJarArg, JsArg).map(_.map { v => if(v.isLeft) FatJarArg else JsArg })
    globalPolicy   <- ~Policy.read(log)
    reporter       <- ~call(ReporterArg).toOption.getOrElse(GraphReporter)
    watch           = call(WatchArg).isSuccess
    waiting         = call(WaitArg).isSuccess
    _              <- Inotify.check(watch || waiting)
    noSecurity      = call(NoSecurityArg).isSuccess
    _              <- call.atMostOne(WatchArg, WaitArg)
    build          <- Build.syncBuild(layer, module.ref(project), layout, noSecurity)

    r               = repeater(build.allSources, waiting) {
                        for {
                          task <- compileOnce(build, layer, module.ref(project), layout, globalPolicy,
                                      if(call.suffix.isEmpty) args else call.suffix, pipelining.getOrElse(
                                      ManagedConfig().pipelining), reporter, ManagedConfig().theme,
                                      noSecurity)
                        } yield {
                          task.transform { completed =>
                            for {
                              compileResult  <- completed
                              compileSuccess <- compileResult.asTry
                              _              <- (dir.map { dir => new build.TargetExtras(build.target).saveJars(layer,
                                                    compileSuccess.classDirectories.values.to[Set],
                                                    dir in layout.pwd, output)
                                                }).getOrElse(Success(()))
                            } yield compileSuccess
                          }
                        }
                      }

    future         <- if(watch || waiting) Try(r.start()).flatten else r.action()
  } yield log.await(Await.result(future, duration.Duration.Inf).success)

  private def repeater(sources: Set[Path], onceOnly: Boolean)
                      (fn: => Try[Future[BuildResult]])
                      : Repeater[Try[Future[BuildResult]]] =
    new Repeater[Try[Future[BuildResult]]] {
      private[this] val watcher = new SourceWatcher(sources)
      override def repeatCondition(): Boolean = watcher.hasChanges

      def continue(res: Try[Future[BuildResult]]) = !onceOnly

      var lastResult: Try[Future[BuildResult]] = Failure(BuildDidNotSucceed())

      override def start(): Try[Future[BuildResult]] = try {
        watcher.start()
        super.start()
      } catch {
        case NonFatal(e)  => throw e
        case e: InterruptedException =>
          log.info(msg"Stopped watching for changes")
          lastResult
      } finally watcher.stop()

      override def action() = {
        watcher.clear()
        val result = fn
        lastResult = result
        result
      }
    }

  def install: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    (cli, tryProject, tryModule) <- cli.askProjectAndModule(layer)
    cli          <- cli.hint(ExecNameArg)
    cli          <- cli.hint(ReporterArg, Reporter.all)
    cli          <- cli.hint(NoSecurityArg)
    cli          <- cli.hint(WatchArg)
    cli          <- cli.hint(WaitArg)
    call         <- cli.call()
    project      <- tryProject
    globalPolicy <- ~Policy.read(log)
    module       <- tryModule
    reporter     <- ~call(ReporterArg).toOption.getOrElse(GraphReporter)
    watch         = call(WatchArg).isSuccess
    waiting       = call(WaitArg).isSuccess
    noSecurity   <- ~call(NoSecurityArg).isSuccess
    build        <- Build.syncBuild(layer, module.ref(project), layout, false)
    _            <- module.kind.as[App].ascribe(InvalidKind(App))
    main         <- module.kind.as[App].map(_.main).ascribe(UnspecifiedMain(module.id))

    r             = repeater(build.allSources, waiting) { for {
                      task <- compileOnce(build, layer, module.ref(project), layout, globalPolicy,
                                  if(call.suffix.isEmpty) Nil else call.suffix, false, reporter,
                                  ManagedConfig().theme, noSecurity)
                    } yield {
                      task.transform { completed =>
                        for {
                          compileResult  <- completed
                          compileSuccess <- compileResult.asTry
                        } yield compileSuccess
                      }
                    } }

    future       <- if(watch || waiting) Try(r.start()).flatten else r.action()
    exec         <- call(ExecNameArg)
    _            <- ~log.info(msg"Building native image for $exec")
    _            <- new build.TargetExtras(build.target).saveNative(Installation.optDir, main)
    bin          <- ~(Installation.optDir / main.key.toLowerCase)
    newBin       <- ~(bin.rename { _ => exec.key })
    _            <- bin.moveTo(newBin)
    globalPolicy <- ~Policy.read(log)
    javaVersion  <- build.universe.javaVersion(module.ref(project), layout)

    _            <- Try(Shell(cli.env).runJava(new build.TargetExtras(build.targets(module.ref(project))).classpath.to[List].map(_.value),
                        ClassRef("exoskeleton.Generate"), false, Map("FPATH" ->
                        Installation.completionsDir.value), Map(), globalPolicy, List(exec.key), true,
                        layout.workDir(module.ref(project)), javaVersion)(log.info(_)).await())

    _            <- ~log.info(msg"Installed $exec executable to ${Installation.optDir}")
  } yield log.await()

  def console: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    (cli, tryProject, tryModule) <- cli.askProjectAndModule(layer)
    cli          <- cli.hint(PipeliningArg, List("on", "off"))
    cli          <- cli.hint(NoSecurityArg)
    cli          <- cli.hint(IgnoreErrorsArg)
    cli          <- cli.hint(ReporterArg, Reporter.all)
    call         <- cli.call()
    force        <- ~call(IgnoreErrorsArg).isSuccess
    pipelining   <- ~call(PipeliningArg).toOption
    reporter     <- ~call(ReporterArg).toOption.getOrElse(GraphReporter)
    noSecurity   <- ~call(NoSecurityArg).isSuccess
    project      <- tryProject
    module       <- tryModule
    
    build        <- Build.syncBuild(layer, module.ref(project), layout, noSecurity)
    
    result       <- for {
                      globalPolicy <- ~Policy.read(log)
                      task <- compileOnce(build, layer, module.ref(project), layout,
                                  globalPolicy, Nil, pipelining.getOrElse(ManagedConfig().pipelining), reporter,
                                  ManagedConfig().theme, noSecurity)
                    } yield { task.transform { completed => for {
                      compileResult  <- completed
                      compileSuccess <- compileResult.asTry
                    } yield compileSuccess } }

    result       <- Try(Try(Await.result(result, Duration.Inf)))
    _            <- if(force) {
                      if(result.isFailure) log.warn(msg"Errors occurred during compilation; launching console "+
                          msg"with an incomplete classpath")
                      Success(())
                    } else result
    compilerMod  <- module.compiler.as[BspCompiler].map(_.ref).map(build.universe(_)).ascribe(NoRepl(module.compiler))
    repl         <- compilerMod.map(_.kind.as[Compiler].get.repl)
    target       <- build(module.ref(project))
    bootCp       <- Success(new build.TargetExtras(target).bootClasspath)
    javaVersion  <- build.universe.javaVersion(module.ref(project), layout)
  } yield {
    val cp = new build.TargetExtras(target).classpath.map(_.value).join(":")
    val bcp = bootCp.map(_.value).join(":")
    cli.continuation(str"""${Jdk.javaExec(javaVersion)} -Xmx256M -Xms32M -Xbootclasspath/a:$bcp -classpath $cp """+
        str"""-Dscala.boot.class.path=$cp -Dscala.home=/opt/scala-2.12.8 -Dscala.usejavacp=true $repl""")
  }

  def classpath: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    (cli, tryProject, tryModule) <- cli.askProjectAndModule(layer)
    cli          <- cli.hint(SingleColumnArg)
    call         <- cli.call()
    singleColumn <- ~call(SingleColumnArg).isSuccess
    project      <- tryProject
    module       <- tryModule
    ref          <- ~module.ref(project)
    build        <- Build.syncBuild(layer, ref, layout, false)
    target       <- build(ref)
    classpath    <- Try(new build.TargetExtras(target).bootClasspath)
  } yield {
    val separator = if(singleColumn) "\n" else ":"
    log.rawln(classpath.map(_.value).join(separator))
    log.await()
  }

  def describe: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    (cli, tryProject, tryModule) <- cli.askProjectAndModule(layer)
    call         <- cli.call()
    project      <- tryProject
    module       <- tryModule
    build        <- Build.syncBuild(layer, module.ref(project), layout, false)
    
    _            <- ~UiGraph.draw(build.graph.links, true,
                        Map())(ManagedConfig().theme).foreach(log.info(_))

  } yield log.await()

  private[this] def compileOnce(build: Build,
                                layer: Layer,
                                moduleRef: ModuleRef,
                                layout: Layout,
                                globalPolicy: Policy,
                                compileArgs: List[String],
                                pipelining: Boolean,
                                reporter: Reporter,
                                theme: Theme,
                                noSecurity: Boolean)
                               (implicit log: Log): Try[Future[BuildResult]] = {
    for(_ <- build.checkoutAll()) yield {
      val multiplexer = new Multiplexer[ModuleRef, CompileEvent](build.targets.map(_._1).to[Set])
      Lifecycle.currentSession.multiplexer = multiplexer
      val future = new build.TargetExtras(build.target).compile(Map(),
        globalPolicy, compileArgs, pipelining, noSecurity).apply(moduleRef).andThen {
        case compRes =>
          multiplexer.closeAll()
          compRes
      }
      reporter.report(build.graph, theme, multiplexer)

      future
    }
  }
}

case class ShadeCli(cli: Cli)(implicit log: Log) {
  def list: Try[ExitStatus] = for {
    layout    <- cli.layout
    conf      <- Layer.readFuryConf(layout)
    layer     <- Layer.retrieve(conf)
    cli       <- cli.hint(RawArg)
    table     <- ~Tables().shades
    cli       <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
    call      <- cli.call()
    _         <- ~log.warn(msg"Shading is not yet implemented.")
    col       <- ~cli.peek(ColumnArg)
    raw       <- ~call(RawArg).isSuccess
    rows      <- ~layer.imports.to[List].flatMap { i => i.shades.map { s => (i.id, s.id) } }
    table     <- ~Tables().show[(ImportId, ProjectId), ImportId](table, cli.cols, rows, raw, col, None, "shade")
    _         <- ~log.infoWhen(!raw)(conf.focus())
    _         <- ~log.rawln(table)
  } yield log.await()

  def add: Try[ExitStatus] = for {
    layout    <- cli.layout
    conf      <- Layer.readFuryConf(layout)
    layer     <- Layer.retrieve(conf)
    cli       <- cli.hint(ImportIdArg, layer.imports.map(_.id))
    imported  <- ~cli.peek(ImportIdArg).flatMap(layer.imports.findBy(_).toOption)
    iLayer    <- imported.fold(~layer) { i => Layer.dereference(layer, Pointer.Empty / i.id) }
    universe  <- iLayer.universe()
    cli       <- cli.hint(ProjectArg, universe.projects.keySet)
    call      <- cli.call()
    _         <- ~log.warn(msg"Shading is not yet implemented.")
    projectId <- call(ProjectArg)
    project   <- universe(projectId)
    project   <- layer.projects.findBy(projectId)
    importId  <- call(ImportIdArg)
    imported  <- layer.imports.findBy(importId)
    shade     <- ~Shade(project.id, false)
    layer     <- ~Layer(_.imports(imported.id).shades).modify(layer)(_ + shade)
    _         <- Layer.commit(layer, conf, layout)
  } yield log.await()
  
  def remove: Try[ExitStatus] = for {
    layout     <- cli.layout
    conf       <- Layer.readFuryConf(layout)
    layer      <- Layer.retrieve(conf)
    cli        <- cli.hint(ImportIdArg, layer.imports.map(_.id))
    
    projectIds <- Try(cli.peek(ImportIdArg).fold(layer.projects.map(_.id).to[List]) { i =>
                     layer.imports.findBy(i).toOption.fold(List[ProjectId]())(_.shades.map(_.id).to[List]) })
    
    cli        <- cli.hint(ProjectArg, projectIds)
    call       <- cli.call()
    _          <- ~log.warn(msg"Shading is not yet implemented.")
    projectId  <- call(ProjectArg)
    importId   <- call(ImportIdArg)
    imported   <- layer.imports.findBy(importId)
    layer      <- ~Layer(_.imports(imported.id).shades).modify(layer)(_.filter(_.id != projectId))
    _          <- Layer.commit(layer, conf, layout)
  } yield log.await()
}

case class ImportCli(cli: Cli)(implicit log: Log) {
  def list: Try[ExitStatus] = for {
    layout    <- cli.layout
    conf      <- Layer.readFuryConf(layout)
    layer     <- Layer.retrieve(conf)
    cli       <- cli.hint(RawArg)
    table     <- ~Tables().imports
    cli       <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
    cli       <- cli.hint(ImportIdArg, layer.imports.map(_.id))
    call      <- cli.call()
    col       <- ~cli.peek(ColumnArg)
    importId  <- ~cli.peek(ImportIdArg)
    raw       <- ~call(RawArg).isSuccess
    rows      <- ~layer.imports.to[List].map { i => (i, Layer.get(i.layerRef, i.remote)) }
    table     <- ~Tables().show(table, cli.cols, rows, raw, col, importId, "import")
    _         <- ~log.infoWhen(!raw)(conf.focus())
    _         <- ~log.rawln(table)
  } yield log.await()
  
  def add: Try[ExitStatus] = for {
    layout      <- cli.layout
    conf        <- Layer.readFuryConf(layout)
    layer       <- Layer.retrieve(conf)
    cli         <- cli.hint(ImportNameArg)
    cli         <- cli.hint(IgnoreArg)
    cli         <- cli.hint(ImportArg, Layer.pathCompletions().getOrElse(Nil))

    cli         <- cli.hint(LayerVersionArg,
                       cli.peek(ImportArg).to[List].flatMap(Layer.versionCompletions(_).getOrElse(Nil)))

    call        <- cli.call()
    layerName   <- call(ImportArg)
    version     =  call(LayerVersionArg).toOption
    nameArg     <- cli.peek(ImportNameArg).orElse(layerName.suggestedName).ascribe(MissingParam(ImportNameArg))
    ignore      <- ~call(IgnoreArg).isSuccess
    newLayerRef <- Layer.resolve(layerName, version)
    pub         <- Layer.published(layerName, version)
    newLayer    <- Layer.get(newLayerRef, pub)
    _           <- newLayer.verify(ignore, false, Pointer.Root)
    ref         <- ~Import(nameArg, newLayerRef, pub)
    layer       <- ~Layer(_.imports).modify(layer)(_ + ref.copy(id = nameArg))
    _           <- Layer.commit(layer, conf, layout)
  } yield log.await()

  def remove: Try[ExitStatus] = for {
    layout    <- cli.layout
    conf      <- Layer.readFuryConf(layout)
    layer     <- Layer.retrieve(conf)
    cli       <- cli.hint(ImportIdArg, layer.imports.map(_.id))
    call      <- cli.call()
    importArg <- call(ImportIdArg)
    layer     <- ~Layer(_.imports).modify(layer)(_.evict(importArg))
    _         <- Layer.commit(layer, conf, layout)
  } yield log.await()
  
  def pull: Try[ExitStatus] = for {
    layout    <- cli.layout
    conf      <- Layer.readFuryConf(layout)
    layer     <- Layer.retrieve(conf)
    cli       <- cli.hint(RecursiveArg)
    cli       <- cli.hint(LayerVersionArg)
    cli       <- cli.hint(AllArg)
    cli       <- cli.hint(ImportIdArg, layer.imports.map(_.id))
    call      <- cli.call()
    version   <- ~call(LayerVersionArg).toOption
    recursive <- ~call(RecursiveArg).isSuccess
    all       <- ~call(AllArg).isSuccess
    importId  <- ~call(ImportIdArg)
    current   <- Try(!all && importId.isFailure)
    
    imports   <- if(all || current) Try(layer.imports.map(_.id).to[List])
                 else ~importId.toOption.fold(List[ImportId]())(List(_))

    _         <- call.atMostOne(AllArg, LayerVersionArg)
    _         <- call.atMostOne(AllArg, ImportIdArg)
    _         <- call.atMostOne(AllArg, RecursiveArg)
    cache     <- ~Service.Cache()
    future    <- ~updateAll(layer, Pointer.Empty, imports, recursive, if(current) None else version, cache)
    layer     <- ~Await.result(future, Duration.Inf)
    conf      <- updateCurrent(layer, conf, version, cache)
    _         <- Layer.commit(layer, conf, layout, force = true)
  } yield log.await()

  private def updateCurrent(layer: Layer, conf: FuryConf, version: Option[LayerVersion], cache: Service.Cache)
                           : Try[FuryConf] =
    if(conf.published.isEmpty) Success(conf)
    else for {

      _                  <- if(conf.path != Pointer.Root) Failure(RootLayerNotSelected(conf.path))
                            else Success(())
      
      published          <- conf.published.ascribe(ImportHasNoRemote(Pointer.Root))
      (newPub, layerRef) <- getNewLayer(conf.layerRef, published, version, Pointer.Root, cache)
      newLayer           <- Layer.get(layerRef, Some(newPub))
      layerRef           <- Layer.store(newLayer)
    } yield conf.copy(layerRef = layerRef, published = Some(newPub))

  private def updateAll(layer: Layer,
                        pointer: Pointer,
                        imports: List[ImportId],
                        recursive: Boolean,
                        version: Option[LayerVersion],
                        cache: Service.Cache)
                       : Future[Layer] =
    imports.map(layer.imports.findBy(_)).map {
      case Success(imp) => updateOne(imp, pointer, recursive, version, cache)
      case Failure(e)   => Future(identity[Layer](_))
    }.sequence.flatMap(_.foldLeft(Future(layer)) { case (l, update) => l.map(update(_)) })

  private def updateOne(imported: Import,
                        pointer: Pointer,
                        recursive: Boolean,
                        version: Option[LayerVersion],
                        cache: Service.Cache)
                       : Future[Layer => Layer] = {
    val future: Future[Try[(PublishedLayer, LayerRef)]] = Future { { for {
      published          <- imported.remote.ascribe(ImportHasNoRemote(pointer / imported.id))
      (newPub, newRef)   <- getNewLayer(imported.layerRef, published, version, pointer / imported.id, cache)
      newLayer           <- Layer.get(newRef, Some(newPub))

      future             <- (if(recursive) ~updateAll(newLayer, pointer / imported.id,
                                newLayer.imports.map(_.id).to[List], recursive, None, cache) else Try(Future(newLayer))): Try[Future[Layer]]
      newLayer           <- ~Await.result(future, Duration.Inf)
      layerRef           <- Layer.store(newLayer)
    } yield (newPub, layerRef) } }

    future.flatMap {
      case Success((newPub, layerRef)) => Future { (layer: Layer) =>
        val layer1 = Layer(_.imports(imported.id).remote)(layer) = Some(newPub)
        Layer(_.imports(imported.id).layerRef)(layer1) = layerRef
      }
      case Failure(e) =>
        Future.fromTry(Failure(e))
    }
  }

  private def getNewLayer(oldLayerRef: LayerRef,
                          published: PublishedLayer,
                          version: Option[LayerVersion],
                          pointer: Pointer,
                          cache: Service.Cache)
                         : Try[(PublishedLayer, LayerRef)] =
    cache(published.url, version).toOption.fold {
      log.warn(msg"Could not update layer ${published.url}}")
      Try((published, oldLayerRef))
    } { artifact =>
      for {
        newPub   <- ~PublishedLayer(published.url, LayerVersion(artifact.version),
                        LayerRef(artifact.ref), Some(artifact.expiry))
        doUpdate  = artifact.version != published.version.major
        
        _         = if(doUpdate)
                      log.info(msg"Updated layer ${pointer} from ${published.url} from version "+
                          msg"${published.version} (${published.layerRef}) to ${artifact.version} (${LayerRef(artifact.ref)})")
      } yield (newPub, if(doUpdate) artifact.layerRef else oldLayerRef)
    }

}

case class LayerCli(cli: Cli)(implicit log: Log) {
  def init: Try[ExitStatus] = for {
    layout <- cli.newLayout
    cli    <- cli.hint(BareArg)
    cli    <- cli.hint(GithubActionsArg)
    cli    <- cli.hint(GitArg)
    cli    <- cli.hint(EditorArg)
    call   <- cli.call()
    bare   <- ~call(BareArg).isSuccess
    edit   <- ~call(EditorArg).isSuccess
    ci     <- ~call(GithubActionsArg).isSuccess
    git    <- ~call(GitArg).isSuccess
    _      <- Layer.init(layout, git, ci, bare)
    _      =  Bsp.createConfig(layout)

    _      <- if(edit) VsCodeSoftware.installedPath(cli.env, false).flatMap { path =>
                implicit val env: Environment = cli.env
                sh"${path} ${layout.baseDir}".exec[Try[String]]
              }
              else Success(())
  } yield log.await()

  def select: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    baseLayer    <- Layer.get(conf.layerRef, conf.published)
    layer        <- Layer.retrieve(conf)
    focus        <- Layer.readFuryConf(layout)
    currentLayer <- ~Some(focus.path)
    absTree      <- ~baseLayer.importTree.getOrElse(Nil)
    relLayers    <- ~layer.imports.map { sr => Pointer(sr.id.key) }
    parentLayer  <- if(baseLayer == layer) ~None else ~Some(Pointer(".."))
    cli          <- cli.hint(LayerArg, absTree.to[Set] ++ relLayers ++ parentLayer -- currentLayer)
    call         <- cli.call()
    relPath      <- call(LayerArg)
    newPath      <- focus.path.dereference(relPath)
    _            <- verifyLayers(newPath, absTree)
    newConf      <- ~focus.copy(path = newPath)
    _            <- Layer.saveFuryConf(newConf, layout)
  } yield log.await()


  def verifyLayers(path: Pointer, list: List[Pointer]): Try[Unit] =
    if(list.map(_.path).contains(path.path)) Success(()) else Failure(LayersFailure(path))

  def cloneLayer: Try[ExitStatus] = for {
    cli        <- cli.hint(PathArg)
    cli        <- cli.hint(EditorArg)
    cli        <- cli.hint(DocsArg)
    cli        <- cli.hint(IgnoreArg)
    cli        <- cli.hint(ImportArg, Layer.pathCompletions().getOrElse(Nil))

    cli        <- cli.hint(LayerVersionArg,
                      cli.peek(ImportArg).to[List].flatMap(Layer.versionCompletions(_).getOrElse(Nil)))

    call       <- cli.call()
    edit       <- ~call(EditorArg).isSuccess
    useDocsDir <- ~call(DocsArg).isSuccess
    layerName  <- call(ImportArg)
    version     = call(LayerVersionArg).toOption
    layerRef   <- Layer.resolve(layerName, version)
    published  <- Layer.published(layerName, version)
    layer      <- Layer.get(layerRef, published)
    ignore     <- ~call(IgnoreArg).isSuccess
    _          <- layer.verify(ignore, true, Pointer.Root)
    dir        <- call(PathArg).pacify(layerName.suggestedName.map { n => Path(n.key) })
    pwd        <- cli.pwd
    dir        <- ~(if(useDocsDir) Xdg.docsDir else pwd).resolve(dir).uniquify()
    _          <- ~log.info(msg"Cloning layer $layerName into ${if(useDocsDir) dir else dir.relativizeTo(pwd)}")
    _          <- ~dir.mkdir()
    newLayout  <- cli.newLayout
    layout     =  newLayout.copy(baseDir = dir)
    optRepo    <- ~layer.mainRepo.flatMap(layer.repos.findBy(_).toOption)
    _          <- optRepo.fold(Try(()))(_.doCleanCheckout(layout))
    _          <- ~log.info(msg"Saving Fury configuration file ${layout.confFile.relativizeTo(layout.pwd)}")
    _          <- Layer.saveFuryConf(FuryConf(layerRef, Pointer.Root, published), layout)
    _          <- Bsp.createConfig(layout)
    _          <- ~log.info(msg"Cloning complete")
    
    _          <- if(edit) VsCodeSoftware.installedPath(cli.env, false).flatMap { path =>
                    implicit val env: Environment = cli.env
                    sh"$path $dir".exec[Try[String]]
                  }
                  else Success(())
  } yield log.await()

  def publish: Try[ExitStatus] = for {
    layout        <- cli.layout
    conf          <- Layer.readFuryConf(layout)
    cli           <- cli.hint(RemoteLayerArg, List(layout.pwd.name) ++ conf.published.map(_.url.path))
    cli           <- cli.hint(RawArg)
    cli           <- cli.hint(PublicArg)
    cli           <- cli.hint(DescriptionArg)
    cli           <- cli.hint(ExpiryArg)
    cli           <- cli.hint(ForceArg)
    call          <- cli.call()
    token         <- ManagedConfig().token.ascribe(NotAuthenticated()).orElse(ConfigCli(cli).doAuth)
    layer         <- Layer.retrieve(conf)
    base          <- Layer.get(conf.layerRef, None)
    
    currentPub    <- if(conf.path.isEmpty) ~conf.published
                     else for {
                       parent <- Layer.dereference(base, conf.path.init)
                       imprt  <- parent.imports.findBy(conf.path.last)
                     } yield imprt.remote

    defaultId     <- Try(currentPub.map(_.url.path).flatMap(RemoteLayerId.unapply(_)))
    remoteLayerId <- call(RemoteLayerArg).toOption.orElse(defaultId).ascribe(MissingParam(RemoteLayerArg))
    public        <- ~call(PublicArg).isSuccess
    raw           <- ~call(RawArg).isSuccess
    force         <- ~call(ForceArg).isSuccess
    description   <- ~call(DescriptionArg).toOption
    ttl           <- Try(call(ExpiryArg).toOption.getOrElse(if(public) 30 else 7))
    _             <- layer.verifyConf(false, conf, Pointer.Root, quiet = false, force)
    _             <- ~log.info(msg"Publishing layer to service ${ManagedConfig().service}")
    ref           <- Layer.share(ManagedConfig().service, layer, token, ttl)
    
    pub           <- Service.tag(ManagedConfig().service, ref.ipfsRef, remoteLayerId.group, remoteLayerId.name,
                         public, conf.published.fold(0)(_.version.major), description, ttl, token, force,
                         conf.published.fold("")(_.url.path.split("/").head))

    _             <- if(raw) ~log.rawln(str"${ref} ${pub}") else {
                       log.info(msg"Shared layer ${LayerRef(ref.key)}")
                       
                       ~log.info(msg"Published version ${pub.version}${if(public) " " else
                           " privately "}to ${pub.url}")
                       ~pub.expiry.foreach { exp => log.info(msg"The layer expires at ${dateFormat.format(new java.util.Date(exp))}") }
                     }

    _             <- conf.path match {
                       case Pointer.Root =>
                         Layer.saveFuryConf(FuryConf(ref, conf.path, Some(pub)), layout)
                       case path            => for {
                         parent <- Layer.dereference(base, path.init)
                         imprt  <- parent.imports.findBy(path.last)
                         parent <- ~parent.copy(imports = parent.imports + imprt.copy(remote = Some(pub)))

                         _      <- Layer.commit(parent, conf.copy(path = conf.path.init,
                                       published = conf.published), layout, false)
                         conf   <- Layer.readFuryConf(layout)
                         _      <- Layer.saveFuryConf(conf.copy(path = path), layout)
                       } yield ()
                     }
  } yield log.await()

  def share: Try[ExitStatus] = for {
    layout <- cli.layout
    cli    <- cli.hint(RawArg)
    cli    <- cli.hint(PublicArg)
    cli    <- cli.hint(ForceArg)
    cli    <- cli.hint(ExpiryArg)
    conf   <- Layer.readFuryConf(layout)
    layer  <- Layer.retrieve(conf)
    call   <- cli.call()
    force  <- ~call(ForceArg).isSuccess
    public <- ~call(PublicArg).isSuccess
    raw    <- ~call(RawArg).isSuccess
    ttl    <- Try(call(ExpiryArg).toOption.getOrElse(if(public) 30 else 7))
    _      <- layer.verifyConf(false, conf, Pointer.Root, quiet = raw, force)

    ref    <- if(!public) Layer.store(layer)
              else for {
                token  <- ManagedConfig().token.ascribe(NotAuthenticated()).orElse(ConfigCli(cli).doAuth)
                ref    <- Layer.share(ManagedConfig().service, layer, token, ttl)
              } yield ref

    _      <- if(raw) ~log.rawln(str"${ref.ipfsRef.uri}") else ~log.info(msg"Shared at ${ref.ipfsRef.uri}")
  } yield log.await()

  def commit: Try[ExitStatus] = for {
    layout <- cli.layout
    conf   <- Layer.readFuryConf(layout)
    layer  <- Layer.retrieve(conf)
    call   <- cli.call()
    _      <- layer.verifyConf(true, conf, Pointer.Root, quiet = false, force = false)
    ref    <- Layer.store(layer)
    _      <- ~log.info(msg"Writing layer database to ${layout.layerDb.relativizeTo(layout.baseDir)}")
    _      <- Layer.writeDb(layer, layout)
    gitDir <- ~GitDir(layout)

    _      <- ~log.info(msg"Adding Fury files to ${layout.layerDb.relativizeTo(layout.baseDir)} and "+
                  msg"${layout.confFile.relativizeTo(layout.baseDir)} to the current repo.")

    _      <- gitDir.add(layout.layerDb, force = true)
    _      <- gitDir.add(layout.confFile, force = true)
    _      <- ~log.info(msg"Don't forget to run ${ExecName("git commit")} to commit the layer to the repo.")
  } yield log.await()

  def undo: Try[ExitStatus] = for {
    call     <- cli.call()
    layout   <- cli.layout
    conf     <- Layer.readFuryConf(layout)
    layer    <- Layer.get(conf.layerRef, conf.published)
    previous <- layer.previous.ascribe(CannotUndo())
    layer    <- Layer.get(previous, None)
    layerRef <- Layer.store(layer)
    _        <- ~log.info(msg"Reverted to layer $layerRef")
    _        <- Layer.saveFuryConf(conf.copy(layerRef = layerRef), layout)
  } yield log.await()

  def diff: Try[ExitStatus] = for {
    layout <- cli.layout
    conf   <- Layer.readFuryConf(layout)
    layer  <- Layer.retrieve(conf)
    cli    <- cli.hint(RawArg)
    cli    <- cli.hint(ImportArg)
    table  <- ~Tables().differences("Current", "Other")
    cli    <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
    call   <- cli.call()
    col    <- ~cli.peek(ColumnArg)
    raw    <- ~call(RawArg).isSuccess
                  
    other  <- call(ImportArg).orElse(conf.published.map { layer => IpfsRef(layer.layerRef.key) }.ascribe(
                  NoOtherLayer()))

    other  <- Layer.resolve(other)
    other  <- Layer.get(other, None)
    rows   <- ~Diff.gen[Layer].diff(layer, other)
    table  <- ~Tables().show[Difference, Difference](table, cli.cols, rows, raw, col)
    _      <- if(!rows.isEmpty) ~log.rawln(table) else ~log.info("No changes")
  } yield log.await()

  def remote: Try[ExitStatus] = for {
    cli    <- cli.hint(RawArg)
    cli    <- cli.hint(ServiceArg)
    svc    <- ~cli.peek(ServiceArg).getOrElse(ManagedConfig().service)
    table  <- ~Tables().layerStatuses(ManagedConfig().service)
    cli    <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
    call   <- cli.call()
    raw    <- ~call(RawArg).isSuccess
    col    <- ~cli.peek(ColumnArg)
    token  <- ManagedConfig().token.ascribe(NotAuthenticated())
    rows   <- Service.published(ManagedConfig().service, token)
    _      <- ~log.rawln(Tables().show[LayerStatus, Boolean](table, cli.cols, rows, raw, col, None, "layer"))
  } yield log.await()

}