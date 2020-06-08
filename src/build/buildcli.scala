/*

    Fury, version 0.16.1. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury

import fury.text._, fury.core._, fury.model._, fury.io._, fury.utils._, Args._

import scala.concurrent._, duration._, ExecutionContext.Implicits.global
import scala.util._, control.NonFatal

case class BuildCli(cli: Cli)(implicit log: Log) {

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

    result       <- build.cleanCache(moduleRef, layout)
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
    cli            <- cli.hint(DirArg)
    cli            <- cli.hint(FatJarArg)
    cli            <- cli.hint(JsArg)
    cli            <- cli.hint(ReporterArg, Reporter.all)
    call           <- cli.call()
    dir            <- ~call(DirArg).toOption
    optProjectId   <- ~cli.peek(ProjectArg).orElse(moduleRef.map(_.projectId).orElse(layer.main))
    optProject     <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
    project        <- optProject.asTry
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
                              _              <- (dir.map { dir => build.saveJars(module.ref(project),
                                                    compileSuccess.classDirectories, dir in layout.pwd, layout,
                                                    output)
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

      override def start(): Try[Future[BuildResult]] = try {
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
    (cli, tryProject, tryModule) <- cli.askProjectAndModule(layer)
    cli          <- cli.hint(ExecNameArg)
    call         <- cli.call()
    exec         <- call(ExecNameArg)
    project      <- tryProject
    module       <- tryModule
    build        <- Build.syncBuild(layer, module.ref(project), layout, false)
    _            <- module.kind.as[App].ascribe(InvalidKind(App))
    main         <- module.kind.as[App].map(_.main).ascribe(UnspecifiedMain(module.id))
    _            <- ~log.info(msg"Building native image for $exec")
    _            <- build.saveNative(module.ref(project), Installation.optDir, layout, main)
    bin          <- ~(Installation.optDir / main.key.toLowerCase)
    newBin       <- ~(bin.rename { _ => exec.key })
    _            <- bin.moveTo(newBin)
    globalPolicy <- ~Policy.read(log)

    _            <- Try(Shell(cli.env).runJava(build.classpath(module.ref(project),
                        layout).to[List].map(_.value), ClassRef("exoskeleton.Generate"), false, Map("FPATH" ->
                        Installation.completionsDir.value), Map(), globalPolicy, layout, List(exec.key),
                        true)(log.info(_)).await())

    _            <- ~log.info(msg"Installed $exec executable to ${Installation.optDir}")
  } yield log.await()

  def eval: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    (cli,
     tryProject,
     tryModule)  <- cli.askProjectAndModule(layer)
    cli          <- cli.hint(NoSecurityArg)
    cli          <- cli.hint(IgnoreErrorsArg)
    cli          <- cli.hint(CodeArg)
    cli          <- cli.hint(ReporterArg, Reporter.all)
    call         <- cli.call()
    force        <- ~call(IgnoreErrorsArg).isSuccess
    reporter     <- ~call(ReporterArg).toOption.getOrElse(GraphReporter)
    noSecurity   <- ~call(NoSecurityArg).isSuccess
    project      <- tryProject
    module       <- tryModule
    
    build        <- Build.syncBuild(layer, module.ref(project), layout, noSecurity)
    
    result       <- for {
                      policy <- ~Policy.read(log)
                      task   <- compileOnce(build, layer, module.ref(project), layout, policy, Nil,
                                    ManagedConfig().pipelining, reporter, ManagedConfig().theme, noSecurity)
                    } yield { task.transform { completed => for { result  <- completed; success <- result.asTry } yield success } }

    result       <- Try(Try(Await.result(result, Duration.Inf)))
    _            <- if(force) {
                      if(result.isFailure) log.warn(msg"Errors occurred during compilation; launching console "+
                          msg"with an incomplete classpath")
                      Success(())
                    } else result
    compiler     <- module.compiler.as[BspCompiler].map(_.ref).map(build.universe(_)).ascribe(NoRepl(module.compiler))
    repl         <- compiler.map(_.kind.as[Compiler].get.repl)
    classpath    <- ~build.classpath(module.ref(project), layout)
    bootCp       <- ~build.bootClasspath(module.ref(project), layout)
  } yield {
    val cp = classpath.map(_.value).join(":")
    val bcp = bootCp.map(_.value).join(":")
    cli.continuation(str"""java -Xmx256M -Xms32M -Xbootclasspath/a:$bcp -classpath $cp """+
        str"""-Dscala.boot.class.path=$cp -Dscala.home=/opt/scala-2.12.8 -Dscala.usejavacp=true $repl""")
  }

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
    classpath    <- ~build.classpath(module.ref(project), layout)
    bootCp       <- ~build.bootClasspath(module.ref(project), layout)
  } yield {
    val cp = classpath.map(_.value).join(":")
    val bcp = bootCp.map(_.value).join(":")
    cli.continuation(str"""java -Xmx256M -Xms32M -Xbootclasspath/a:$bcp -classpath $cp """+
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
    build        <- Build.syncBuild(layer, module.ref(project), layout, false)
    classpath    <- ~build.classpath(module.ref(project), layout)
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
    for(_ <- build.checkoutAll(layout)) yield {
      val multiplexer = new Multiplexer[ModuleRef, CompileEvent](build.targets.map(_._1).to[Set])
      Lifecycle.currentSession.multiplexer = multiplexer
      val future = build.compile(moduleRef, Map(), layout,
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