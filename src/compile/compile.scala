/*

    Fury, version 0.15.1. Copyright 2018-20 Jon Pretty, Propensive OÜ.

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
import java.util.concurrent.{CompletableFuture, ExecutionException}

import bloop.launcher.LauncherMain
import bloop.launcher.LauncherStatus._
import ch.epfl.scala.bsp4j.{CompileResult => _, _}
import com.google.gson.{Gson, JsonElement}
import fury.core.UiGraph.CompilerDiagnostic
import fury.core.Lifecycle.Session
import fury.io._
import fury.model._
import fury.strings._
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
import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration.Duration
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
  private val bloopVersion = "1.4.0-RC1"

  def singleTasking[T](work: Promise[Unit] => T): Future[T] = {
    val newLock: Promise[Unit] = Promise()
    BloopServer.synchronized {
      val future = lock.future.map { case _ => work(newLock) }
      lock = newLock
      future
    }
  }

  def subscribers(bspClient: FuryBuildClient): List[Session] = usages.synchronized {
    usages.collect { case (session, connection) if connection.client == bspClient => session }.to[List]
  }

  override def acquire(session: Session, connection: Connection): Unit = usages.synchronized {
    usages += session -> connection
  }

  override def release(session: Session): Unit = usages.synchronized {
    usages -= session
  }

  case class Connection(server: FuryBspServer, client: FuryBuildClient, thread: Thread)
  
  private def connect(dir: Path,
                      compilation: Compilation,
                      targetId: TargetId,
                      layout: Layout,
                      trace: Option[Path] = None)
                     (implicit log: Log): Future[Connection] =
    singleTasking { promise =>

      val traceOut = trace.map{ path => new FileOutputStream(path.javaFile, true) }
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
            log.info(msg"Starting the BSP launcher...")
          case r"Opening a bsp server connection with.*" =>
            log.info(msg"Opening a BSP server connection")
          case r"Waiting for a connection at" =>
            log.info(msg"Waiting for a socket connection")
          case r"Loading project from .*" => None
          case r"Loading previous analysis for .*" => None
          case r".*detected in file:.*" => None
          case r"  => .*" => None
          case r"Creating a scala instance from Bloop" =>
            log.info("Instantiating a new instance of the Scala compiler")
          case r"The server is listening for incoming connections.*" =>
            log.info(msg"BSP server is listening for incoming connections")
          case r"Waiting.*" => None
          case r"Starting thread.*" => None
          case r"Deduplicating compilation of .*" => None
          case r"No server running at .*" =>
            log.info("Could not detect a BSP server running locally")
          case r"Command: .*" => None
          case r"A bloop installation has been detected.*" =>
            log.info("Detected an existing Bloop installation")
          case "\n" => None
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

  def borrow[T](dir: Path, compilation: Compilation, targetId: TargetId, layout: Layout)
               (fn: Connection => T)
               (implicit log: Log)
               : Try[T] = {

    val conn = BloopServer.synchronized {
      connections.get(dir)
    }.getOrElse {
      val tracePath: Option[Path] = if(ManagedConfig().trace) {
        Some(layout.logsDir / str"${java.time.LocalDateTime.now().toString}.log")
      } else None
      
      val newConnection =
        Await.result(connect(dir, compilation, targetId, layout, trace = tracePath), Duration.Inf)
      
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

object Compilation {

  private val compilationCache: collection.mutable.Map[Path, Future[Try[Compilation]]] = TrieMap()

  private val requestOrigins: collection.mutable.Map[RequestOriginId, Compilation] = TrieMap()

  def findBy(targetId: TargetId): Iterable[Compilation] = requestOrigins.values.filter(_.target.id == targetId)

  def mkCompilation(layer: Layer,
                    ref: ModuleRef,
                    layout: Layout,
                    noSecurity: Boolean)(implicit log: Log)
  : Try[Compilation] = for {

    hierarchy   <- layer.hierarchy()
    universe    <- hierarchy.universe
    policy      <- ~Policy.read(log)
    compilation <- fromUniverse(universe, ref, layout)
    _           <- policy.checkAll(compilation.requiredPermissions, noSecurity)
    _           <- compilation.generateFiles(layout)
  } yield compilation

  def fromUniverse(universe: Universe, ref: ModuleRef, layout: Layout)(implicit log: Log): Try[Compilation] = {
    import universe._

    def directDependencies(target: Target): Set[TargetId] =
      (target.dependencies ++ target.compiler.map(_.id)).to[Set]

    def graph(target: Target): Try[Target.Graph] = for {
      requiredModules <- dependencies(ref, layout)
      requiredTargets <- requiredModules.traverse(makeTarget(_, layout))
    } yield {
      val targetGraph = (requiredTargets + target).map { t => t.id -> directDependencies(t) }
      Target.Graph(targetGraph.toMap, requiredTargets.map { t => t.id -> t }.toMap)
    }

    def canAffectBuild(target: Target): Boolean =
      Set[Kind](Compiler, Application, Plugin, Benchmarks).contains(target.kind)

    for {
      target              <- makeTarget(ref, layout)
      graph               <- graph(target)
      
      targetIndex         <- graph.dependencies.keys.traverse { targetId => makeTarget(targetId.ref,
                                 layout).map(t => targetId -> t) }

      requiredTargets     =  targetIndex.unzip._2.toSet
      
      requiredPermissions =  (if(target.kind.needsExecution) requiredTargets else requiredTargets -
                                 target).flatMap(_.permissions)

      checkouts           <- graph.dependencies.keys.traverse { targetId => checkout(targetId.ref, layout) }
    } yield {
      val moduleRefToTarget = (requiredTargets ++ target.compiler).map(t => t.ref -> t).toMap
      val intermediateTargets = requiredTargets.filter(canAffectBuild)
      
      val subgraphs = DirectedGraph(graph.dependencies).subgraph(intermediateTargets.map(_.id).to[Set] +
          TargetId(ref)).connections
      
      Compilation(target, graph, subgraphs, checkouts.foldLeft(Checkouts(Set()))(_ ++ _),
          moduleRefToTarget, targetIndex.toMap, requiredPermissions.toSet, universe)
    }
  }

  def asyncCompilation(layer: Layer, ref: ModuleRef, layout: Layout)
                      (implicit log: Log)
                      : Future[Try[Compilation]] = {

    def fn: Future[Try[Compilation]] = Future(mkCompilation(layer, ref, layout, false))

    compilationCache(layout.furyDir) = compilationCache.get(layout.furyDir) match {
      case Some(future) => future.transformWith(fn.waive)
      case None         => fn
    }

    compilationCache(layout.furyDir)
  }

  def syncCompilation(layer: Layer,
                      ref: ModuleRef,
                      layout: Layout,
                      noSecurity: Boolean)(implicit log: Log): Try[Compilation] = {
    val compilation = mkCompilation(layer, ref, layout, noSecurity)
    compilationCache(layout.furyDir) = Future.successful(compilation)
    compilation
  }

  def nextOriginId(compilation: Compilation)(implicit log: Log): RequestOriginId = {
    val originId = RequestOriginId.next(log.pid)
    requestOrigins(originId) = compilation
    originId
  }

  def findOrigin(originId: RequestOriginId): Option[Compilation] = requestOrigins.get(originId)

}

class FuryBuildClient(layout: Layout) extends BuildClient {

  def broadcast(event: CompileEvent): Unit = {
    event match {
      case e: ModuleCompileEvent =>
        BloopServer.subscribers(this).map(_.multiplexer)
          .filter(_.contains(e.ref)).foreach(_.fire(e.ref, event))
      case Tick =>
        BloopServer.subscribers(this).map(_.multiplexer)
          .foreach(_.updateAll(Tick))
    }
  }

  override def onBuildShowMessage(params: ShowMessageParams): Unit = {
    val ref = for {
      idString    <- Option(params.getOriginId)
      originId    <- RequestOriginId.unapply(idString)
      compilation <- Compilation.findOrigin(originId)
    } yield compilation.target.ref
    broadcast(Print(ref.get, params.getMessage))
  }

  override def onBuildLogMessage(params: LogMessageParams): Unit = {
    val ref = for {
      idString    <- Option(params.getOriginId)
      originId    <- RequestOriginId.unapply(idString)
      compilation <- Compilation.findOrigin(originId)
    } yield compilation.target.ref
    broadcast(Print(ref.get, params.getMessage))
  }

  override def onBuildPublishDiagnostics(params: PublishDiagnosticsParams): Unit = {
    val targetId: TargetId = params.getBuildTarget.getUri.as[TargetId].get
    val fileName = new java.net.URI(params.getTextDocument.getUri).getRawPath
    val compilation = for {
      idString    <- Option(params.getOriginId)
      originId    <- RequestOriginId.unapply(idString)
      compilation <- Compilation.findOrigin(originId)
    } yield compilation

    val repos = compilation match {
      case Some(c) => c.checkouts.checkouts.map { checkout => (checkout.path.value, checkout.repoId)}.toMap
      case None =>
        //FIXME
        //println(str"Request with originId: ${params.getOriginId} could not be matched to a compilation")
        Map.empty
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
        targetId.ref,
        CompilerDiagnostic(
          msg"""$severity ${targetId.ref}${'>'}${repo}${':'}${filePath}${':'}${lineNo}${':'}${(charNum +
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

  override def onBuildTargetDidChange(params: DidChangeBuildTarget): Unit = {}

  private[this] def convertDataTo[A: ClassTag](data: Object): A = {
    val gson = new Gson()
    val json = data.asInstanceOf[JsonElement]
    val report =
      gson.fromJson[A](json, classTag[A].runtimeClass)
    report
  }

  private[this] def getCompileTargetId(taskNotificationData: AnyRef): TargetId = {
    val report = convertDataTo[CompileTask](taskNotificationData)
    report.getTarget.getUri.as[TargetId].get
  }

  override def onBuildTaskProgress(params: TaskProgressParams): Unit = {
    val targetId = getCompileTargetId(params.getData)
    broadcast(CompilationProgress(targetId.ref, params.getProgress.toDouble / params.getTotal))
  }

  override def onBuildTaskStart(params: TaskStartParams): Unit = {
    val targetId = getCompileTargetId(params.getData)
    broadcast(StartCompile(targetId.ref))
    for {
      compilation <- Compilation.findBy(targetId)
      dependencyTargetId <- compilation.deepDependencies(targetId)
    } yield {
      broadcast(NoCompile(dependencyTargetId.ref))  
    }
  }

  override def onBuildTaskFinish(params: TaskFinishParams): Unit = params.getDataKind match {
    case TaskDataKind.COMPILE_REPORT =>
      val targetId = getCompileTargetId(params.getData)
      val ref = targetId.ref
      val success = params.getStatus == StatusCode.OK
      broadcast(StopCompile(ref, success))
      Compilation.findBy(targetId).foreach { compilation =>
        val signal = if(success && compilation.targets(ref).kind.needsExecution) StartRun(ref) else StopRun(ref)
        broadcast(signal)
      }
  }
}

case class Compilation(target: Target, 
                       graph: Target.Graph,
                       subgraphs: Map[TargetId, Set[TargetId]],
                       checkouts: Checkouts,
                       targets: Map[ModuleRef, Target],
                       targetIndex: Map[TargetId, Target],
                       requiredPermissions: Set[Permission],
                       universe: Universe) {

  private[this] val hashes: HashMap[ModuleRef, Digest] = new HashMap()
  lazy val allDependencies: Set[Target] = targets.values.to[Set]

  lazy val deepDependencies: Map[TargetId, Set[TargetId]] = {
    @tailrec
    def flatten[T](aggregated: Set[T], children: T => Set[T], next: Set[T]): Set[T] = {
      if(next.isEmpty) aggregated
      else {
        val node = next.head
        flatten(aggregated + node, children, next - node ++ children(node))
      }
    }
    targetIndex.map { case (targetId, _) =>
      targetId -> flatten[TargetId](Set.empty, graph.dependencies(_).to[Set], Set(targetId))
    }
  }

  def apply(ref: ModuleRef): Try[Target] = targets.get(ref).ascribe(ItemNotFound(ref.moduleId))

  def checkoutAll(layout: Layout)(implicit log: Log): Try[Unit] =
    checkouts.checkouts.traverse(_.get(layout)).map{ _ => ()}

  def generateFiles(layout: Layout)(implicit log: Log): Try[Iterable[Path]] = synchronized {
    Bloop.generateFiles(this, layout)
  }

  def classpath(ref: ModuleRef, layout: Layout): Set[Path] = {
    requiredTargets(ref).flatMap { target =>
      Set(layout.classesDir(target.id), layout.resourcesDir(target.id)) ++ target.binaries
    } ++ targets(ref).binaries
  }

  def persistentOpts(ref: ModuleRef, layout: Layout)(implicit log: Log): Try[Set[Provenance[Opt]]] = for {
    target      <- this(ref)
    optCompiler <- ~target.compiler

    compileOpts <- ~optCompiler.to[Set].flatMap { c => c.optDefs.filter(_.persistent).map(_.opt(c.ref,
                       Origin.Compiler)) }

    refParams   <- ~target.params.filter(_.persistent).map(Provenance(_, optCompiler.map(_.ref).getOrElse(
                       ModuleRef.JavaRef), Origin.Module(target.ref)))

    removals    <- ~refParams.filter(_.value.remove).map(_.value.id)
    inherited   <- target.dependencies.map(_.ref).traverse(persistentOpts(_, layout)).map(_.flatten)
    pOpts       <- ~(if(target.kind == Plugin) Set(
                     Provenance(Opt(OptId(str"Xplugin:${layout.classesDir(target.id)}"), persistent = true,
                         remove = false), target.compiler.fold(ModuleRef.JavaRef)(_.ref), Origin.Plugin)
                   ) else Set())
  } yield (pOpts ++ compileOpts ++ inherited ++ refParams.filter(!_.value.remove)).filterNot(removals contains
      _.value.id)

  def aggregatedOpts(ref: ModuleRef, layout: Layout)(implicit log: Log): Try[Set[Provenance[Opt]]] = for {
    target      <- apply(ref)
    optCompiler <- ~target.compiler
    
    tmpParams   <- ~target.params.filter(!_.persistent).map(Provenance(_, optCompiler.map(_.ref).getOrElse(
                       ModuleRef.JavaRef), Origin.Local))

    removals    <- ~tmpParams.filter(_.value.remove).map(_.value.id)
    opts        <- persistentOpts(ref, layout)
  } yield (opts ++ tmpParams.filter(!_.value.remove)).filterNot(removals contains _.value.id)

  def aggregatedOptDefs(ref: ModuleRef): Try[Set[Provenance[OptDef]]] = for {
    target  <- this(ref)
    optDefs <- target.dependencies.map(_.ref).traverse(aggregatedOptDefs(_))
  } yield optDefs.flatten.to[Set] ++ target.optDefs.map(Provenance(_, target.impliedCompiler,
      Origin.Module(target.ref))) ++ target.compiler.to[Set].flatMap { c => c.optDefs.map(Provenance(_, c.ref,
      Origin.Compiler)) }

  def aggregatedPlugins(ref: ModuleRef): Try[Set[Provenance[Plugin]]] = for {
    target           <- apply(ref)
    inheritedPlugins <- target.dependencies.map(_.ref).traverse(aggregatedPlugins(_))
  } yield inheritedPlugins.flatten.to[Set] ++ target.plugin.map { m =>
      Provenance(Plugin(m, target.ref, target.main.get), target.compiler.fold(ModuleRef.JavaRef)(_.ref),
      Origin.Plugin) }
  
  def aggregatedResources(ref: ModuleRef): Try[Set[Source]] = for {
    target    <- apply(ref)
    inherited <- target.dependencies.map(_.ref).traverse(aggregatedResources(_))
  } yield inherited.flatten.to[Set] ++ target.resources
  
  def bootClasspath(ref: ModuleRef, layout: Layout): Set[Path] = {
    val requiredPlugins = requiredTargets(ref).filter(_.kind == Plugin).flatMap { target =>
      Set(layout.classesDir(target.id), layout.resourcesDir(target.id)) ++ target.binaries
    }
    val compilerClasspath = targets(ref).compiler.to[Set].flatMap { c => classpath(c.ref, layout) }
    compilerClasspath ++ requiredPlugins
  }

  private def requiredTargets(ref: ModuleRef): Set[Target] = {
    val requiredIds = deepDependencies(targets(ref).id)
    requiredIds.map(targetIndex.apply)
  }

  def allSources: Set[Path] = targets.values.to[Set].flatMap{x: Target => x.sourcePaths.to[Set]}

  def writePlugin(ref: ModuleRef, layout: Layout): Unit = {
    val target = targets(ref)
    if(target.kind == Plugin) {
      val file = layout.classesDir(target.id) / "scalac-plugin.xml"

      target.main.foreach { main =>
        file.writeSync(str"""|<plugin>
                             | <name>${target.plugin.fold("plugin")(_.key)}</name>
                             | <classname>${main}</classname>
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

  def saveJars(ref: ModuleRef,
               srcs: Set[Path],
               destination: Path,
               layout: Layout,
               fatJar: Boolean)(implicit log: Log)
  : Try[Unit] = {
    val bins = allDependencies.flatMap(_.binaries)
    for {
      entity           <- universe.entity(ref.projectId)
      module           <- entity.project(ref.moduleId)
      manifest          = JarManifest(bins.map(_.name), module.main.map(_.key))
      dest              = destination.extant()
      path              = (dest / str"${ref.projectId.key}-${ref.moduleId.key}.jar")
      enc               = System.getProperty("file.encoding")
      _                 = log.info(msg"Saving JAR file ${path.relativizeTo(layout.baseDir)} using ${enc}")
      stagingDirectory <- aggregateCompileResults(ref, srcs, layout)
      resources        <- aggregatedResources(ref)
      _                <- resources.traverse(_.copyTo(checkouts, layout, stagingDirectory))
      
      _                <- Shell(layout.env).jar(path, if(fatJar) bins else Set.empty,
                              stagingDirectory.children.map(stagingDirectory / _).to[Set], manifest)

      _                <- if(!fatJar) bins.traverse { bin => bin.copyTo(dest / bin.name) } else Success(())

      _                 = if(fatJar) log.info(msg"Wrote ${path.size} to ${path.relativizeTo(layout.baseDir)}")
                          else log.info(msg"Wrote ${bins.size + 1} JAR files (total ${bins.foldLeft(ByteSize(0
                              ))(_ + _.size)}) to ${path.parent.relativizeTo(layout.baseDir)}")

    } yield ()
  }

  private[this] def aggregateCompileResults(ref: ModuleRef,
                                            compileResults: Set[Path],
                                            layout: Layout): Try[Path] = {
    val stagingDirectory = layout.workDir(targets(ref).id) / "staging"
    for(_ <- compileResults.filter(_.exists()).traverse(_.copyTo(stagingDirectory))) yield stagingDirectory
  }

  def jmhRuntimeClasspath(ref: ModuleRef, classesDirs: Set[Path], layout: Layout): Set[Path] =
    classesDirs ++ targets(ref).compiler.to[Set].map { compilerTarget =>
      layout.resourcesDir(compilerTarget.id)
    } ++ classpath(ref, layout)

  def runtimeClasspath(ref: ModuleRef, layout: Layout): Set[Path] =
    targets(ref).compiler.to[Set].flatMap { compilerTarget =>
      Set(layout.classesDir(compilerTarget.id), layout.resourcesDir(compilerTarget.id))
    } ++ classpath(ref, layout) + layout.classesDir(targets(ref).id) + layout.resourcesDir(targets(ref).id)

  def cleanCache(moduleRef: ModuleRef, layout: Layout)(implicit log: Log): Try[CleanCacheResult] = {
    val target = targets(moduleRef)
    val furyTargetIds = deepDependencies(target.id).toList
    val bspTargetIds = furyTargetIds.map { dep =>
      new BuildTargetIdentifier(str"file://${layout.workDir(dep).value}?id=${dep.key}")
    }
    val params = new CleanCacheParams(bspTargetIds.asJava)

    BloopServer.borrow(layout.baseDir, this, target.id, layout) { conn =>
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
  : Future[CompileResult] = Future.fromTry {
    val originId = Compilation.nextOriginId(this)

    val uri: String = str"file://${layout.workDir(target.id).value}?id=${target.id.key}"
    val params = new CompileParams(List(new BuildTargetIdentifier(uri)).asJava)
    params.setOriginId(originId.key)
    if(pipelining) params.setArguments(List("--pipeline").asJava)
    val furyTargetIds = deepDependencies(target.id).toList
    
    val bspTargetIds = furyTargetIds.map { dep =>
      new BuildTargetIdentifier(str"file://${layout.workDir(dep).value}?id=${dep.key}")
    }
    
    val bspToFury = (bspTargetIds zip furyTargetIds).toMap
    val scalacOptionsParams = new ScalacOptionsParams(bspTargetIds.asJava)

    BloopServer.borrow(layout.baseDir, this, target.id, layout) { conn =>

      val result: Try[CompileResult] = {
        for {
          res <- wrapServerErrors(conn.server.buildTargetCompile(params))
          opts <- wrapServerErrors(conn.server.buildTargetScalacOptions(scalacOptionsParams))
        } yield CompileResult(res, opts)
      }

      val responseOriginId = result.get.bspCompileResult.getOriginId
      if(responseOriginId != originId.key){
        log.warn(s"buildTarget/compile: Expected ${originId.key}, but got $responseOriginId")
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
    }.map {
      case (compileResult, client) if compileResult.isSuccessful && target.kind.needsExecution =>
        val classDirectories = compileResult.classDirectories
        client.broadcast(StartRun(target.ref))
        val exitCode = run(target, classDirectories, layout, globalPolicy, args, noSecurity)
        client.broadcast(StopRun(target.ref))
        compileResult.copy(exitCode = Some(exitCode))
      case (otherResult, _) =>
        otherResult
    }
  }

  private[this] def wrapServerErrors[T](f: => CompletableFuture[T]): Try[T] =
    Try(f.get).recoverWith { case e: ExecutionException => Failure(BuildServerError(e.getCause)) }


  def compile(moduleRef: ModuleRef,
              futures: Map[TargetId, Future[CompileResult]] = Map(),
              layout: Layout,
              globalPolicy: Policy,
              args: List[String],
              pipelining: Boolean,
              noSecurity: Boolean)(implicit log: Log)
  : Map[TargetId, Future[CompileResult]] = {
    val target = targets(moduleRef)

    val newFutures = subgraphs(target.id).foldLeft(futures) { (futures, dependencyTarget) =>
      if(futures.contains(dependencyTarget)) futures
      else compile(dependencyTarget.ref, futures, layout, globalPolicy, args, pipelining, noSecurity)
    }

    val dependencyFutures = Future.sequence(subgraphs(target.id).map(newFutures))

    val multiplexer = Lifecycle.currentSession.multiplexer
    multiplexer.start()

    val future = dependencyFutures.map(CompileResult.merge).flatMap { required =>
      if(!required.isSuccessful) {
        multiplexer.fire(target.ref, SkipCompile(target.ref))
        multiplexer.close(target.ref)
        Future.successful(required)
      } else {
        val noCompilation = target.sourcePaths.isEmpty && !target.kind.needsExecution

        if(noCompilation) {
          deepDependencies(target.id).foreach { targetId =>
            multiplexer.fire(targetId.ref, NoCompile(targetId.ref))
          }
          Future.successful(required)
        } else compileModule(target, layout, pipelining, globalPolicy, args, noSecurity)
      }
    }

    newFutures.updated(target.id, future)
  }

  private def run(target: Target, classDirectories: Set[Path], layout: Layout, globalPolicy: Policy,
                  args: List[String], noSecurity: Boolean)
                 (implicit log: Log): Int = {
    val multiplexer = Lifecycle.currentSession.multiplexer
    if (target.kind == Benchmarks) {
      classDirectories.foreach { classDirectory =>
        Jmh.instrument(classDirectory, layout.benchmarksDir(target.id), layout.resourcesDir(target.id))
        val javaSources = layout.benchmarksDir(target.id).findChildren(_.endsWith(".java"))

        Shell(layout.env).javac(
          classpath(target.ref, layout).to[List].map(_.value),
          classDirectory.value,
          javaSources.map(_.value).to[List])
      }
    }
    val exitCode = Shell(layout.env).runJava(
      jmhRuntimeClasspath(target.ref, classDirectories, layout).to[List].map(_.value),
      if (target.kind == Benchmarks) "org.openjdk.jmh.Main" else target.main.fold("")(_.key),
      securePolicy = target.kind == Application,
      env = target.environment,
      properties = target.properties,
      policy = globalPolicy.forContext(layout, target.ref.projectId),
      layout = layout,
      args,
      noSecurity
    ) { ln =>
      multiplexer.fire(target.ref, Print(target.ref, ln))
    }.await()

    deepDependencies(target.id).foreach { targetId =>
      multiplexer.fire(targetId.ref, NoCompile(targetId.ref))
    }

    multiplexer.close(target.ref)
    multiplexer.fire(target.ref, StopRun(target.ref))

    exitCode
  }

}
