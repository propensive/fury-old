/*

    Fury, version 0.17.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

import java.io._
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.channels._
import java.util.concurrent.{CompletableFuture, ExecutionException, TimeoutException}

import bloop.launcher.LauncherMain
import bloop.launcher.LauncherStatus._
import ch.epfl.scala.bsp4j.{CompileResult => _, _}
import com.google.gson.{Gson, JsonElement}
import fury.core.UiGraph.CompileIssue
import fury.core.Lifecycle.Session
import fury.io._
import fury.model._
import fury.text._
import fury.utils._
import gastronomy._
import kaleidoscope._
import mercator._
import org.eclipse.lsp4j.jsonrpc.Launcher

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.concurrent.TrieMap
import scala.collection.mutable.HashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.higherKinds
import scala.reflect.{ClassTag, classTag}
import scala.util._
import scala.util.control.NonFatal

trait FuryBspServer extends BuildServer with ScalaBuildServer

object BloopServer extends Lifecycle.Shutdown with Lifecycle.ResourceHolder {
  override type Resource = Connection

  Lifecycle.bloopServer.complete(Success(this))
 
  private var lock: Promise[Unit] = Promise.successful(())
  private var connections: Map[Path, Connection] = Map.empty
  private var usages: Map[Session, Connection] = Map.empty
  private val bloopVersion = "1.4.3"

  def singleTasking[T](work: Promise[Unit] => T): Future[T] = {
    val newLock: Promise[Unit] = Promise()
    BloopServer.synchronized {
      val future = lock.future.map { case _ => work(newLock) }
      lock = newLock
      future
    }
  }

  def subscribers(bspClient: FuryBuildClient): List[Session] = usages.synchronized {
    usages.collect { case (session, conn) if conn.client == bspClient => session }.to[List]
  }

  override def acquire(session: Session, connection: Connection): Unit =
    usages.synchronized { usages += session -> connection }

  override def release(session: Session): Unit =
    usages.synchronized { usages -= session }

  case class Connection(server: FuryBspServer, client: FuryBuildClient, thread: Thread)
  
  private def connect(dir: Path,
                      build: Build,
                      ref: ModuleRef,
                      layout: Layout,
                      trace: Option[Path] = None)
                     (implicit log: Log): Future[Connection] =
    singleTasking { promise =>

      val traceOut = trace.map { path => new FileOutputStream(path.javaFile, true) }
      val serverIoPipe = Pipe.open()
      val serverIn = Channels.newInputStream(serverIoPipe.source())
      
      val clientOut = {
        val out = Channels.newOutputStream(serverIoPipe.sink())
        traceOut.fold(out)(new TeeOutputStream(out, _))
      }
      
      val clientIoPipe = Pipe.open()
      val clientIn = Channels.newInputStream(clientIoPipe.source())
      
      val serverOut = {
        val out = Channels.newOutputStream(clientIoPipe.sink())
        traceOut.fold(out)(new TeeOutputStream(out, _))
      }

      val logging: PrintStream = log.stream { str =>
        (if(str.indexOf("[D]") == 9) str.drop(17) else str) match {
          case r"Starting the bsp launcher.*" =>
            log.note(msg"Starting the BSP launcher...")
          case r"Opening a bsp server connection with.*" =>
            log.note(msg"Opening a BSP server connection")
          case r"Waiting for a connection at" =>
            log.info(msg"Waiting for a socket connection")
          case r"Loading project from .*" =>
            None
          case r"Loading previous analysis for .*" =>
            None
          case r".*detected in file:.*" =>
            None
          case r"  => .*" =>
            None
          case r"Creating a scala instance from Bloop" =>
            log.info("Instantiating a new instance of the Scala compiler")
          case r"The server is listening for incoming connections.*" =>
            log.note(msg"BSP server is listening for incoming connections")
          case r"Waiting.*" =>
            None
          case r"Starting thread.*" =>
            None
          case r"Deduplicating compilation of .*" =>
            None
          case r"No server running at .*" =>
            log.info("Could not detect a BSP server running locally")
          case r"Command: .*" =>
            None
          case r"A bloop installation has been detected.*" =>
            log.info("Detected an existing Bloop installation")
          case "\n" =>
            None
          case other =>
            log.note(msg"${'['}bsp${']'} ${other}")
        }
      }

      val launcher: LauncherMain = new LauncherMain(serverIn, serverOut, logging,
          StandardCharsets.UTF_8, bloop.bloopgun.core.Shell.default, None, None, promise)
      
      val thread = Threads.launcher.newThread { () =>
        launcher.runLauncher(
          bloopVersionToInstall = bloopVersion,
          skipBspConnection = false,
          serverJvmOptions = Nil
        )
      }

      thread.start()
      
      val client = new FuryBuildClient(layout)
        
      val bspServer = new Launcher.Builder[FuryBspServer]()
        .setRemoteInterface(classOf[FuryBspServer])
        .setExecutorService(Threads.bsp)
        .setInput(clientIn)
        .setOutput(clientOut)
        .setLocalService(client)
        .create()
      
      bspServer.startListening()
        
      val proxy = bspServer.getRemoteProxy
        
      val capabilities = new BuildClientCapabilities(List("scala").asJava)
      val initParams = new InitializeBuildParams("fury", FuryVersion.current, "2.0.0-M4", dir.uriString,
          capabilities)

      proxy.buildInitialize(initParams).get
      proxy.onBuildInitialized()
      
      Connection(proxy, client, thread)
    }

  def borrow[T](dir: Path, build: Build, ref: ModuleRef, layout: Layout)
               (fn: Connection => T)
               (implicit log: Log)
               : Try[T] = {

    val conn = BloopServer.synchronized(connections.get(dir)).getOrElse {
      log.note(msg"Opening a new BSP connection at $dir")
      val tracePath: Option[Path] = if(ManagedConfig().trace) {
        val path = layout.logsDir / s"${java.time.LocalDateTime.now().toString}.log"
        log.note(str"BSP trace log is at $path")
        Some(path)
      } else None

      val newConnection =
        Await.result(connect(dir, build, ref, layout, trace = tracePath), Duration.Inf)
      
      connections += dir -> newConnection
      newConnection
    }

    Try {
      acquire(Lifecycle.currentSession, conn)
      conn.synchronized(fn(conn))
    }
  }
  
  def workDirectories: Set[Path] = connections.keySet

  override def shutdown(): Unit = {
    BloopServer.synchronized {
      connections.foreach { case(dir, conn) =>
        conn.synchronized(try {
          conn.server.buildShutdown().get()
          conn.server.onBuildExit()
        } catch {
          case NonFatal(e) => println(s"Error while closing the connection for $dir. Cause: ${e.getMessage}")
        })
      }
      connections = Map.empty
    }
  }
}

object Build {

  private val buildCache: collection.mutable.Map[Path, Future[Try[Build]]] = TrieMap()
  private val requestOrigins: collection.mutable.Map[RequestOriginId, Build] = TrieMap()

  def findBy(ref: ModuleRef): Iterable[Build] = requestOrigins.values.filter(_.target.ref == ref)

  def apply(layer: Layer, dependency: Dependency, layout: Layout, noSecurity: Boolean)
           (implicit log: Log)
           : Try[Build] = for {

    hierarchy <- layer.hierarchy()
    universe  <- hierarchy.universe
    build     <- Build(universe, dependency, hierarchy, layout)
    _         <- Policy.read(log).checkAll(build.requiredPermissions, noSecurity)
    _         <- build.generateFiles(layout)
  } yield build

  def apply(universe: Universe, dependency: Dependency, hierarchy: Hierarchy, layout: Layout)
           (implicit log: Log)
           : Try[Build] = {
    def directDependencies(target: Target): Set[Dependency] = target.module.dependencies ++ target.module.compiler()
    def canAffectBuild(target: Target): Boolean = !target.module.kind.is[Lib]

    def graph(target: Target): Try[Target.Graph] = for {
      requiredModules <- universe.dependencies(dependency.ref, layout)
      requiredTargets <- requiredModules.map(_.ref).traverse(Target(_, hierarchy, universe, layout))
    } yield {
      val targetGraph = (requiredTargets + target).map { t => t.ref -> directDependencies(t) }
      Target.Graph(targetGraph.toMap, requiredTargets.map { t => t.ref -> t }.toMap)
    }

    for {
      target      <- Target(dependency.ref, hierarchy, universe, layout)
      graph       <- graph(target)

      targetIndex <- graph.dependencies.keys.filter(_ != ModuleRef.JavaRef).traverse { ref =>
                       Target(ref, hierarchy, universe, layout).map(ref -> _)
                     }
      
      targets      = targetIndex.unzip._2.to[Set]
      snapshots   <- graph.dependencies.keys.filter(_ != ModuleRef.JavaRef).traverse(universe.snapshots(_, hierarchy, layout))
      policy       = (if(target.module.kind.needsExec) targets else targets - target).flatMap(_.module.policy)
    } yield {
      val moduleRefToTarget = (targets ++ target.module.compiler().map { d => graph.targets(d.ref) }).map { t => t.ref -> t }.toMap
      val intermediateTargets = targets.filter(canAffectBuild)
      
      val subgraphs = Dag(graph.dependencies.mapValues(_.map(_.ref))).subgraph(intermediateTargets.map(_.ref).to[Set] + dependency.ref).connections
      
      Build(target, graph, subgraphs, snapshots.foldLeft(Snapshots())(_ ++ _),
          moduleRefToTarget, targetIndex.toMap, policy.to[Set], universe)
    }
  }

  def asyncBuild(layer: Layer, ref: ModuleRef, layout: Layout)(implicit log: Log): Future[Try[Build]] = {
    def fn: Future[Try[Build]] = Future(Build(layer, Dependency(ref), layout, false))
    buildCache(layout.furyDir) = buildCache.get(layout.furyDir).fold(fn)(_.transformWith(fn.waive))

    buildCache(layout.furyDir)
  }

  def syncBuild(layer: Layer, ref: ModuleRef, layout: Layout, noSecurity: Boolean)
               (implicit log: Log)
               : Try[Build] = {
    val build = Build(layer, Dependency(ref), layout, noSecurity)
    buildCache(layout.furyDir) = Future.successful(build)
    build
  }

  def nextOriginId(build: Build)(implicit log: Log): RequestOriginId = {
    val originId = RequestOriginId.next(log.pid)
    requestOrigins(originId) = build
    originId
  }

  def findOrigin(originId: RequestOriginId): Option[Build] = requestOrigins.get(originId)
}

class FuryBuildClient(layout: Layout) extends BuildClient {

  def broadcast(event: CompileEvent): Unit = event match {
    case e: ModuleCompileEvent =>
      BloopServer.subscribers(this).map(_.multiplexer).filter(_.contains(e.ref)).foreach(_.fire(e.ref, event))
    case Tick =>
      BloopServer.subscribers(this).map(_.multiplexer).foreach(_.updateAll(Tick))
  }

  override def onBuildShowMessage(params: ShowMessageParams): Unit = for {
    idString <- Option(params.getOriginId)
    originId <- RequestOriginId.unapply(idString)
    build    <- Build.findOrigin(originId)
  } yield broadcast(Print(build.target.ref, params.getMessage))

  override def onBuildLogMessage(params: LogMessageParams): Unit = for {
    idString <- Option(params.getOriginId)
    originId <- RequestOriginId.unapply(idString)
    build    <- Build.findOrigin(originId)
  } yield broadcast(Print(build.target.ref, params.getMessage))

  override def onBuildPublishDiagnostics(params: PublishDiagnosticsParams): Unit = {
    val ref = extractModuleRef(params.getBuildTarget.getUri)
    val fileName = new java.net.URI(params.getTextDocument.getUri).getRawPath
    val build = for {
      idString <- Option(params.getOriginId)
      originId <- RequestOriginId.unapply(idString)
      build    <- Build.findOrigin(originId)
    } yield build

    val repos = build match {
      case Some(c) => c.snapshots.snapshots.map { case (hash, snapshot) => (snapshot.path.value, snapshot.repoId)}.toMap
      case None    => Map()
    }

    params.getDiagnostics.asScala.foreach { diag =>
      val lineNo  = LineNo(diag.getRange.getStart.getLine + 1)
      val charNum = diag.getRange.getStart.getCharacter
      // FIXME: This reads the same file potentially many times
      val codeLine = scala.io.Source.fromFile(fileName).getLines.toList(lineNo.line - 1)

      def isSymbolic(ch: Char)     = (ch == '_' || !ch.isLetterOrDigit) && ch != ' '
      def isAlphanumeric(ch: Char) = ch == '_' || ch.isLetterOrDigit

      def takeSame(str: String): (String, String) = {
        val ch = str.find(_ != '_').getOrElse('_')
        val matching = if(isSymbolic(ch)) str.takeWhile(isSymbolic) else str.takeWhile(isAlphanumeric)
        val remainder = str.drop(matching.length)

        (matching, remainder)
      }

      val linePrefix = codeLine.take(charNum)
      val (matching, remainder) = takeSame(codeLine.drop(linePrefix.length))
      
      val highlightedLine = UserMsg { theme =>
        msg"$linePrefix${theme.failure(theme.underline(matching))}$remainder".string(theme).dropWhile(_ == ' ')
      }

      val (repo, filePath) = repos.find { case (k, v) => fileName.startsWith(k) }.map {
        case (k, v) => (v, Path(fileName.drop(k.length + 1)))
      }.getOrElse((RepoId("local"), Path(fileName.drop(layout.baseDir.value.length + 1))))

      val severity = UserMsg { theme => diag.getSeverity.toString.toLowerCase match {
        case "error"       => msg"${'['}${theme.failure("E")}${']'}".string(theme)
        case "warning"     => msg"${'['}${theme.ongoing("W")}${']'}".string(theme)
        case "information" => msg"${'['}${theme.info("I")}${']'}".string(theme)
        case _             => msg"${'['}${theme.info("H")}${']'}".string(theme)
      } }

      
      broadcast(DiagnosticMsg(
        ref,
        CompileIssue(
          msg"""$severity ${ref}${'>'}${repo}${':'}${filePath}${':'}${lineNo}${':'}${(charNum +
              1).toString}
${'|'} ${UserMsg(
            theme =>
              diag.getMessage.split("\n").to[List].map(theme.gray(_)).join(msg"""
${'|'} """.string(theme)))}
${'|'} ${highlightedLine}
""",
          repo,
          filePath,
          lineNo,
          charNum
        )
      ))
    }
  }

  override def onBuildTargetDidChange(params: DidChangeBuildTarget): Unit = ()

  private[this] def convertDataTo[A: ClassTag](data: Object): A = {
    val gson = new Gson()
    val json = data.asInstanceOf[JsonElement]
    
    gson.fromJson[A](json, classTag[A].runtimeClass)
  }

  private[this] def getCompileRef(taskNotificationData: AnyRef): ModuleRef =
    extractModuleRef(convertDataTo[CompileTask](taskNotificationData).getTarget.getUri)

  private[this] def extractModuleRef(uri: String) = {
    val params = new java.net.URI(uri).getRawQuery.split("^").map(_.split("=", 2)).map {
      param => param(0) -> param(1)
    }.toMap

    ModuleRef(params("id").split("_", 2).mkString("/"))
  }

  override def onBuildTaskProgress(params: TaskProgressParams): Unit = {
    val ref = getCompileRef(params.getData)
    broadcast(Progress(ref, params.getProgress.toDouble / params.getTotal))
  }

  override def onBuildTaskStart(params: TaskStartParams): Unit = {
    val ref = getCompileRef(params.getData)
    broadcast(StartCompile(ref))
    
    for {
      build         <- Build.findBy(ref)
      dependencyRef <- build.deepDependencies(ref)
    } yield broadcast(NoCompile(dependencyRef))  
  }

  override def onBuildTaskFinish(params: TaskFinishParams): Unit = params.getDataKind match {
    case TaskDataKind.COMPILE_REPORT =>
      val ref = getCompileRef(params.getData)
      val success = params.getStatus == StatusCode.OK
      broadcast(StopCompile(ref, success))
      Build.findBy(ref).foreach { build =>
        val signal = if(success && build.targets(ref).module.kind.needsExec) StartRun(ref) else StopRun(ref)
        broadcast(signal)
      }
  }
}

case class Build(target: Target, 
                 graph: Target.Graph,
                 subgraphs: Map[ModuleRef, Set[ModuleRef]],
                 snapshots: Snapshots,
                 targets: Map[ModuleRef, Target],
                 targetIndex: Map[ModuleRef, Target],
                 requiredPermissions: Set[Permission],
                 universe: Universe) {

  private[this] val hashes: HashMap[ModuleRef, Digest] = new HashMap()
  lazy val allDependencies: Set[Target] = targets.values.to[Set]

  lazy val deepDependencies: Map[ModuleRef, Set[ModuleRef]] = {
    
    @tailrec
    def flatten[T](aggregated: Set[T], children: T => Set[T], next: Set[T]): Set[T] = {
      if(next.isEmpty) aggregated
      else flatten(aggregated + next.head, children, next - next.head ++ children(next.head))
    }
    
    targetIndex.map { case (targetId, _) =>
      targetId -> flatten[ModuleRef](Set.empty, graph.dependencies(_).map(_.ref).to[Set], Set(targetId))
    }
  }

  def apply(ref: ModuleRef): Try[Target] = targets.get(ref).ascribe(ItemNotFound(ref.moduleId))

  def apply(compiler: CompilerRef): Try[Option[Target]] = compiler match {
    case Javac(_)         => Success(None)
    case BspCompiler(ref) => apply(ref).map(Some(_))
  }

  def checkoutAll(layout: Layout)(implicit log: Log): Try[Unit] =
    snapshots.snapshots.traverse { case (hash, snapshot) => snapshot.get(layout) }.map { _ => () }

  def generateFiles(layout: Layout)(implicit log: Log): Try[Iterable[Path]] =
    synchronized { Bloop.generateFiles(this, layout) }

  def copyInclude(ref: ModuleRef, include: Include, layout: Layout)(implicit log: Log): Try[Unit] =
    include.kind match {
      case ClassesDir(deep) =>
        include.path.mkdir().flatMap {
          layout.classesDir(include.ref).copyTo(include.path in layout.workDir(ref)).waive
        }.munit
      case FileRef(source) =>
        /*glob(layout.workDir(include.ref), layout.workDir(include.ref).walkTree).traverse { p =>
          val work = layout.workDir(include.ref)
          val relSrc = p.relativizeTo(work)
          val dest = relSrc in (include.path in layout.workDir(ref))
          log.note(msg"Copying $relSrc in ${include.ref} to ${dest.relativizeTo(layout.workDir(ref))} in $ref")
          dest.mkParents() >>= p.copyTo(dest).waive
        }.map(_.unit)*/
        ???
      case DirRef(source) =>
        ???
      case TarFile(gzip) =>
        ???
      case Jarfile(fat) =>
        ???
    }

  def classpath(ref: ModuleRef, layout: Layout): Set[Path] = ref.javac.fold(Set[Path]()) { ref =>
    requiredTargets(ref).flatMap { target =>
      Set(layout.classesDir(target.ref), layout.resourcesDir(target.ref)) ++ target.binaries
    } ++ targets(ref).binaries
  }

  def persistentOpts(ref: ModuleRef, layout: Layout)(implicit log: Log): Try[Set[Provenance[Opt]]] =
    ref.javac.fold(Try(Set[Provenance[Opt]]())) { ref => for {
      target      <- apply(ref)
      compiler    <- apply(target.module.compiler)
      compileOpts <- ~compiler.to[Set].flatMap(_.module.optDefs.filter(_.persistent).map(_.opt(target.module.compiler, Origin.Compiler)))
      refParams   <- ~target.module.opts.filter(_.persistent).map(Provenance(_, target.module.compiler, Origin.Module(target.ref)))
      removals    <- ~refParams.filter(_.value.remove).map(_.value.id)
      inherited   <- target.module.dependencies.to[List].map(_.ref).traverse(persistentOpts(_, layout)).map(_.flatten)

      pOpts       <- ~(if(target.module.kind.is[Plugin]) Set(
                      Provenance(Opt(OptId(str"Xplugin:${layout.classesDir(target.ref)}"), persistent = true,
                          remove = false), target.module.compiler, Origin.Plugin)
                    ) else Set())

    } yield (pOpts ++ compileOpts ++ inherited ++ refParams.filter(!_.value.remove)).filterNot(removals contains
        _.value.id) }

  def aggregatedOpts(ref: ModuleRef, layout: Layout)(implicit log: Log): Try[Set[Provenance[Opt]]] =
    ref.javac.fold(Try(Set[Provenance[Opt]]())) { ref => for {
      target    <- apply(ref)
      compiler  <- ~target.module.compiler
      tmpParams <- ~target.module.opts.filter(!_.persistent).map(Provenance(_, compiler, Origin.Local))
      removals  <- ~tmpParams.filter(_.value.remove).map(_.value.id)
      opts      <- persistentOpts(ref, layout)
    } yield (opts ++ tmpParams.filter(!_.value.remove)).filterNot(removals contains _.value.id) }

  def aggregatedOptDefs(ref: ModuleRef): Try[Set[Provenance[OptDef]]] =
    ref.javac.fold(Try(Set[Provenance[OptDef]]())) { ref => for {
      target  <- apply(ref)
      optDefs <- target.module.dependencies.to[List].map(_.ref).traverse(aggregatedOptDefs(_))
    } yield optDefs.flatten.to[Set] ++ target.module.optDefs.map(Provenance(_, target.module.compiler,
        Origin.Module(target.ref))) ++ target.module.compiler().flatMap { dependency =>
        apply(dependency.ref).get.module.optDefs.map(Provenance(_, target.module.compiler, Origin.Compiler)) } }

  def aggregatedPlugins(ref: ModuleRef): Try[Set[Provenance[PluginDef]]] =
    ref.javac.fold(Try(Set[Provenance[PluginDef]]())) { ref => for {
      target           <- apply(ref)
      inheritedPlugins <- target.module.dependencies.to[List].map(_.ref).traverse(aggregatedPlugins(_))
    } yield inheritedPlugins.flatten.to[Set] ++ target.module.kind.as[Plugin].map { plugin =>
        Provenance(PluginDef(plugin.id, ref, plugin.main), target.module.compiler, Origin.Plugin)
    } }
  
  def aggregatedResources(ref: ModuleRef): Try[Set[Source]] =
    ref.javac.fold(Try(Set[Source]())) { ref => for {
      target    <- apply(ref)
      inherited <- target.module.dependencies.to[List].map(_.ref).traverse(aggregatedResources(_))
    } yield inherited.flatten.to[Set] ++ target.module.resources }
  
  def bootClasspath(ref: ModuleRef, layout: Layout): Set[Path] = ref.javac.fold(Set[Path]()) { ref =>
    val requiredPlugins = requiredTargets(ref).filter(_.module.kind.is[Plugin]).flatMap { target =>
      Set(layout.classesDir(target.ref), layout.resourcesDir(target.ref)) ++ target.binaries
    }

    val compilerClasspath = targets(ref).module.compiler().flatMap { dependency => classpath(dependency.ref, layout) }
    
    compilerClasspath ++ requiredPlugins
  }

  private def requiredTargets(ref: ModuleRef): Set[Target] =
    ref.javac.fold(Set[Target]()) { ref => deepDependencies(ref).map(targetIndex.apply) }

  def allSources: Set[Path] = targets.values.to[Set].flatMap(_.sourcePaths.to[Set])

  def writePlugin(ref: ModuleRef, layout: Layout): Unit = {
    val target = targets(ref)
    if(target.module.kind.is[Plugin]) {
      val file = layout.classesDir(target.ref) / "scalac-plugin.xml"

      target.module.kind.as[Plugin].foreach { plugin =>
        file.writeSync(str"""|<plugin>
                             | <name>${plugin.id}</name>
                             | <classname>${plugin.main}</classname>
                             |</plugin>""".stripMargin)
      }
    }
  }

  def saveNative(ref: ModuleRef, dest: Path, layout: Layout, main: ClassRef)(implicit log: Log): Try[Unit] =
    for {
      dest <- Try(dest.extant())
      cp   = runtimeClasspath(ref, layout).to[List].map(_.value)
      _    <- Shell(layout.env).native(dest, cp, main.key)
    } yield ()

  def saveJars(ref: ModuleRef, srcs: Set[Path], destination: Path, layout: Layout, out: Option[CliParam])
              (implicit log: Log)
              : Try[Unit] = {
    val bins = allDependencies.flatMap(_.binaries)
    val fatJar = out == Some(Args.FatJarArg)
    val js = out == Some(Args.JsArg)

    def saveJar(staging: Path, module: Module): Try[Unit] = {
      val dest = destination.extant()
      val path = dest / str"${ref.projectId.key}-${ref.moduleId.key}.jar"
      val enc = System.getProperty("file.encoding")
      val manifest = JarManifest(bins.map(_.name), module.kind.as[App].map(_.main.key))
      val jarInputs: Set[Path] = if(fatJar) bins else Set.empty
      log.info(msg"Saving JAR file ${path.relativizeTo(layout.baseDir)} using ${enc}")
      
      for {
        resources        <- aggregatedResources(ref)
        _                <- resources.traverse(snapshots.copy(_, layout, staging))
        _                <- Shell(layout.env).jar(path, jarInputs, staging.children.map(staging / _).to[Set], manifest)
        _                <- if(!fatJar) bins.traverse { bin => bin.copyTo(dest / bin.name) } else Success(())
      } yield {
        if(fatJar) log.info(msg"Wrote ${path.size} to ${path.relativizeTo(layout.baseDir)}")
        else log.info(msg"Wrote ${bins.size + 1} JAR files (total ${bins.foldLeft(ByteSize(0
        ))(_ + _.size)}) to ${path.parent.relativizeTo(layout.baseDir)}")
      }
    }

    def packageJson(bin: Option[String]): euphemism.Json = {
      //TODO move this to fury.utils.ScalaJs
      import euphemism._
      Json.of(
        name = s"${ref.projectId.key}-${ref.moduleId.key}",
        version = "0.0.1",
        bin = bin
      )
    }

    def saveJs(staging: Path, module: Module): Try[Unit] = {
      val jsName = str"${ref.projectId.key}-${ref.moduleId.key}.js"
      val dest = destination.extant()
      val path = dest / jsName
      val enc  = System.getProperty("file.encoding")
      val launcherName = "main.js"
      val json = packageJson(if(module.kind.needsExec) Some(launcherName) else None)
      log.info(msg"Saving Javascript file ${path.relativizeTo(layout.baseDir)} using ${enc}")
      for {
        _ <- ScalaJs.link(module.kind.as[App].map(_.main.key), List(staging) ++ bins, path)
        _ <- (dest / "package.json").writeSync(json.toString())
        _ <- if(module.kind.needsExec) saveJsLauncher(launcherName, jsName) else ~()
      } yield ()
    }

    def saveJsLauncher(launcherName: String, mainFile: String): Try[Unit] = {
      val launcherPath = destination / launcherName
      val launcherScript = s"""#!/usr/bin/env node
                              |var launcher = require('./$mainFile');""".stripMargin
      for {
        _ <- launcherPath.writeSync(launcherScript)
        _ <- launcherPath.setExecutable(true)
      } yield ()
    }

    for {
      staging <- aggregateResults(ref, srcs, layout)
      project <- universe(ref.projectId)
      module  <- project(ref.moduleId)
      _       <- if(!js) saveJar(staging, module) else saveJs(staging, module)
      _       <- staging.delete()
    } yield ()
  }

  private[this] def aggregateResults(ref: ModuleRef, compileResults: Set[Path], layout: Layout): Try[Path] = {
    val staging = layout.workDir(ref) / "staging"
    for(_ <- compileResults.filter(_.exists()).traverse(_.copyTo(staging))) yield staging
  }

  def jmhRuntimeClasspath(ref: ModuleRef, classesDirs: Set[Path], layout: Layout): Set[Path] =
    classesDirs ++ targets(ref).module.compiler().map(_.ref).map(layout.resourcesDir(_)) ++ classpath(ref, layout)

  def runtimeClasspath(ref: ModuleRef, layout: Layout): Set[Path] =
    targets(ref).module.compiler().flatMap { c => Set(layout.resourcesDir(c.ref), layout.classesDir(c.ref)) } ++
        classpath(ref, layout) + layout.classesDir(ref) + layout.resourcesDir(ref)

  def cleanCache(moduleRef: ModuleRef, layout: Layout)(implicit log: Log): Try[CleanCacheResult] = {
    val target = targets(moduleRef)
    val furyTargetIds = deepDependencies(target.ref).toList
    val bspTargetIds = furyTargetIds.map { ref =>
      new BuildTargetIdentifier(str"file://${layout.workDir(ref).value}?id=${ref.urlSafe}")
    }
    val params = new CleanCacheParams(bspTargetIds.asJava)

    BloopServer.borrow(layout.baseDir, this, target.ref, layout) { conn =>
      val result: Try[CleanCacheResult] = wrapServerErrors(conn.server.buildTargetCleanCache(params))
      result.get
    }
  }

  def compileModule(target: Target,
                    layout: Layout,
                    pipelining: Boolean,
                    globalPolicy: Policy,
                    args: List[String],
                    noSecurity: Boolean)(implicit log: Log)
  : Future[BuildResult] = Future.fromTry {
    val originId = Build.nextOriginId(this)
    val uri: String = str"file://${layout.workDir(target.ref)}?id=${target.ref.urlSafe}"
    val params = new CompileParams(List(new BuildTargetIdentifier(uri)).asJava)
    params.setOriginId(originId.key)
    if(pipelining) params.setArguments(List("--pipeline").asJava)
    val furyTargetIds = deepDependencies(target.ref).toList
    
    val bspTargetIds = furyTargetIds.map { ref =>
      new BuildTargetIdentifier(str"file://${layout.workDir(ref)}?id=${ref.urlSafe}")
    }
    
    val bspToFury = (bspTargetIds zip furyTargetIds).toMap
    val scalacOptionsParams = new ScalacOptionsParams(bspTargetIds.asJava)

    BloopServer.borrow(layout.baseDir, this, target.ref, layout) { conn =>

      val result: Try[BuildResult] = for {
        res  <- wrapServerErrors(conn.server.buildTargetCompile(params))
        opts <- wrapServerErrors(conn.server.buildTargetScalacOptions(scalacOptionsParams))
      } yield BuildResult(res, opts, None)

      val responseOriginId = result.get.bspResult.getOriginId
      if(responseOriginId != originId.key) {
        log.warn(msg"buildTarget/compile: Expected ${originId.key}, but got $responseOriginId")
      }

      result.get.scalacOptions.getItems.asScala.foreach { case soi =>
        val bti = soi.getTarget
        val classDir = soi.getClassDirectory
        val targetId = bspToFury(bti)
        val permanentClassesDir = layout.classesDir(targetId)
        val temporaryClassesDir = Path(new URI(classDir))
        temporaryClassesDir.copyTo(permanentClassesDir)
        //TODO the method setClassDirectory modifies a mutable structure. Consider refactoring
        soi.setClassDirectory(permanentClassesDir.javaFile.toURI.toString)
      }

      (result.get, conn.client)
    }.flatMap {
      case (compileResult, client) if compileResult.success && target.module.kind.needsExec =>
        val timeout = target.module.kind.as[App].fold(0)(_.timeout)
        val classDirectories = compileResult.classDirectories
        client.broadcast(StartRun(target.ref))
        target.module.includes.traverse(copyInclude(target.ref, _, layout)).map { _ =>
          val future = Future(blocking(run(target, classDirectories, layout, globalPolicy, args, noSecurity)))
          val result = Try(Await.result(future, if(timeout == 0) Duration.Inf else Duration(timeout, SECONDS)))
          val exitCode = result.recover { case _: TimeoutException => 124 }.getOrElse(1)
          client.broadcast(StopRun(target.ref))
          compileResult.copy(exitCode = Some(exitCode))
        }
      case (otherResult, _) =>
        Success(otherResult)
    }
  }

  private[this] def wrapServerErrors[T](f: => CompletableFuture[T]): Try[T] =
    Try(f.get).recoverWith { case e: ExecutionException => Failure(BuildServerError(e.getCause)) }


  def compile(moduleRef: ModuleRef,
              futures: Map[ModuleRef, Future[BuildResult]] = Map(),
              layout: Layout,
              globalPolicy: Policy,
              args: List[String],
              pipelining: Boolean,
              noSecurity: Boolean)
             (implicit log: Log)
             : Map[ModuleRef, Future[BuildResult]] = {
    val target = targets(moduleRef)

    val newFutures = subgraphs(target.ref).foldLeft(futures) { (futures, dependencyRef) =>
      if(futures.contains(dependencyRef)) futures
      else compile(dependencyRef, futures, layout, globalPolicy, args, pipelining, noSecurity)
    }

    val dependencyFutures = Future.sequence(subgraphs(target.ref).map(newFutures))

    val multiplexer = Lifecycle.currentSession.multiplexer
    multiplexer.start()

    val future = dependencyFutures.map(BuildResult.merge).flatMap { required =>
      if(!required.success) {
        multiplexer.fire(target.ref, SkipCompile(target.ref))
        multiplexer.close(target.ref)
        Future.successful(required)
      } else {
        val noCompilation = target.sourcePaths.isEmpty && !target.module.kind.needsExec

        if(noCompilation) {
          deepDependencies(target.ref).foreach { ref =>
            multiplexer.fire(ref, NoCompile(ref))
          }
          Future.successful(required)
        } else compileModule(target, layout, pipelining, globalPolicy, args, noSecurity)
      }
    }

    newFutures.updated(target.ref, future)
  }

  private def run(target: Target, classDirectories: Set[Path], layout: Layout, globalPolicy: Policy,
                  args: List[String], noSecurity: Boolean)
                 (implicit log: Log): Int = {
    val multiplexer = Lifecycle.currentSession.multiplexer
    if (target.module.kind.is[Bench]) {
      classDirectories.foreach { classDirectory =>
        Jmh.instrument(classDirectory, layout.benchmarksDir(target.ref), layout.resourcesDir(target.ref))
        val javaSources = layout.benchmarksDir(target.ref).findChildren(_.endsWith(".java"))

        Shell(layout.env).javac(
          classpath(target.ref, layout).to[List].map(_.value),
          classDirectory.value,
          javaSources.map(_.value).to[List])
      }
    }
    
    val exitCode = Shell(layout.env.copy(workDir = Some(layout.workDir(target.ref).value))).runJava(
      jmhRuntimeClasspath(target.ref, classDirectories, layout).to[List].map(_.value),
      if(target.module.kind.is[Bench]) ClassRef("org.openjdk.jmh.Main")
      else target.module.kind.as[App].fold(ClassRef(""))(_.main),
      securePolicy = target.module.kind.is[App],
      env = target.environment,
      properties = target.properties,
      policy = globalPolicy.forContext(layout, target.ref.projectId),
      layout = layout,
      args,
      noSecurity,
      layout.workDir(target.ref)
    ) { ln => multiplexer.fire(target.ref, Print(target.ref, ln)) }.await()

    deepDependencies(target.ref).foreach { ref => multiplexer.fire(ref, NoCompile(ref)) }

    multiplexer.close(target.ref)
    multiplexer.fire(target.ref, StopRun(target.ref))

    exitCode
  }
}
