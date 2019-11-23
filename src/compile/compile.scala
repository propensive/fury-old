/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.7.5. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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
package fury.core

import java.io._
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.channels._
import java.time.LocalDateTime
import java.util.concurrent.{CompletableFuture, ExecutionException, Executors, TimeUnit}

import bloop.bloopgun.BloopgunCli
import bloop.launcher.{LauncherMain, LauncherStatus}
import bloop.launcher.LauncherStatus._
import ch.epfl.scala.bsp4j.{CompileResult => BspCompileResult, _}
import com.google.gson.{Gson, JsonElement}
import fury._
import fury.core.Graph.CompilerDiagnostic
import fury.io._
import fury.model._
import fury.strings._
import fury.utils._
import gastronomy._
import mercator._
import org.eclipse.lsp4j.jsonrpc.{JsonRpcException, Launcher}

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

object BloopServer {
 
  private var lock: Promise[Unit] = Promise.successful(())
  private var connections: List[Connection] = Nil
  private val bloopVersion = "1.3.5"

  def singleTasking[T](work: Promise[Unit] => T): Future[T] = {
    val newLock: Promise[Unit] = Promise()
    BloopServer.synchronized {
      val future = lock.future.map { case _ => work(newLock) }
      lock = newLock
      future
    }
  }

  case class Connection(server: FuryBspServer, client: FuryBuildClient, thread: Thread)
  
  private def connect(dir: Path, multiplexer: Multiplexer[ModuleRef, CompileEvent], compilation: Compilation, targetId: TargetId, layout: Layout): Future[Connection] = singleTasking { promise =>
    val serverIoPipe = Pipe.open()
    val serverIn = Channels.newInputStream(serverIoPipe.source())
    val clientOut = Channels.newOutputStream(serverIoPipe.sink())
    val clientIoPipe = Pipe.open()
    val clientIn = Channels.newInputStream(clientIoPipe.source())
    val serverOut = Channels.newOutputStream(clientIoPipe.sink())

    val launcher: LauncherMain = new LauncherMain(serverIn, serverOut, System.out,
        StandardCharsets.UTF_8, bloop.bloopgun.core.Shell.default, None, None, promise)
    
    val thread = Threads.launcher.newThread { () =>
      launcher.runLauncher(
        bloopVersionToInstall = bloopVersion,
        skipBspConnection = false,
        serverJvmOptions = Nil
      )
    }
    
    thread.start()
    
    val client = new FuryBuildClient(multiplexer, compilation, targetId, layout)
      
    val bspServer = new Launcher.Builder[FuryBspServer]()
      //.traceMessages(log)
      .setRemoteInterface(classOf[FuryBspServer])
      .setExecutorService(Threads.bsp)
      .setInput(clientIn)
      .setOutput(clientOut)
      .setLocalService(client)
      .create()
    
    bspServer.startListening()
      
    val proxy = bspServer.getRemoteProxy
      
    val capabilities = new BuildClientCapabilities(List("scala").asJava)
    val initParams = new InitializeBuildParams("fury", Version.current, "2.0.0-M4", dir.uriString, capabilities)

    proxy.buildInitialize(initParams).get(5, TimeUnit.SECONDS)
    proxy.onBuildInitialized()
    
    Connection(proxy, client, thread)
  }

  def borrow[T](dir: Path, multiplexer: Multiplexer[ModuleRef, CompileEvent], compilation: Compilation, targetId: TargetId, layout: Layout)(fn: Connection => T): Try[T] = {
    val conn = BloopServer.synchronized {
      connections match {
        case head :: tail =>
          connections = tail
          Some(head)
        case Nil =>
          None
      }
    }.getOrElse(Await.result(connect(dir, multiplexer, compilation, targetId, layout), Duration.Inf))

    try {
      val result = fn(conn)
      BloopServer.synchronized(conn.server.buildShutdown().get())
      //BloopServer.synchronized(connections ::= conn)
      Success(result)
    } catch {
      case e: Exception =>
        //conn.terminate()
        Failure(e)
    }
  }
}

object Compilation {

  //FIXME
  var receiverClient: Option[BuildClient] = None

  private val compilationCache: collection.mutable.Map[Path, Future[Try[Compilation]]] = TrieMap()

  def mkCompilation(log: Log,
                    schema: Schema,
                    ref: ModuleRef,
                    layout: Layout,
                    https: Boolean)
  : Try[Compilation] = for {

    hierarchy   <- schema.hierarchy(log, layout, https)
    universe    <- hierarchy.universe
    policy      <- ~Policy.read(log)
    compilation <- fromUniverse(log, universe, ref, policy, layout)
    _           <- compilation.generateFiles(log, layout)
  } yield compilation

  private def fromUniverse(log: Log, universe: Universe, ref: ModuleRef, policy: Policy, layout: Layout): Try[Compilation] = {
    import universe._
    for {
      target    <- makeTarget(log, ref, layout)
      entity    <- entity(ref.projectId)
      graph     <- dependencies(ref, layout).map(_.map(makeTarget(log, _, layout)).map { a =>
        a.map { dependencyTarget =>
          (dependencyTarget.id, dependencyTarget.dependencies ++ dependencyTarget.compiler.map(_.id))
        }
      }.sequence.map(_.toMap.updated(target.id, target.dependencies ++
        target.compiler.map(_.id)))).flatten
      targets   <- graph.keys.map { targetId =>
        makeTarget(log, targetId.ref, layout).map(targetId.ref -> _)
      }.sequence.map(_.toMap)
      permissions = targets.flatMap(_._2.permissions)
      _         <- policy.checkAll(permissions)
      appModules = targets.filter(_._2.executed)
      subgraphs  = DirectedGraph(graph.mapValues(_.to[Set])).subgraph(appModules.map(_._2.id).to[Set] +
        TargetId(entity.schema.id, ref)).connections.mapValues(_.to[List])
      checkouts <- graph.keys.map { targetId => checkout(targetId.ref, layout) }.sequence
    } yield
      Compilation(graph, subgraphs, checkouts.foldLeft(Set[Checkout]())(_ ++ _),
        targets ++ (target.compiler.map { compilerTarget => compilerTarget.ref -> compilerTarget }), universe)
  }

  def asyncCompilation(log: Log,
                       schema: Schema,
                       ref: ModuleRef,
                       layout: Layout,
                       https: Boolean)
  : Future[Try[Compilation]] = {

    def fn: Future[Try[Compilation]] = Future(mkCompilation(log, schema, ref, layout, https))

    compilationCache(layout.furyDir) = compilationCache.get(layout.furyDir) match {
      case Some(future) => future.transformWith(fn.waive)
      case None         => fn
    }

    compilationCache(layout.furyDir)
  }

  def syncCompilation(log: Log,
                      schema: Schema,
                      ref: ModuleRef,
                      layout: Layout,
                      https: Boolean): Try[Compilation] = {
    val compilation = mkCompilation(log, schema, ref, layout, https)
    compilationCache(layout.furyDir) = Future.successful(compilation)
    compilation
  }
}

class FuryBuildClient(multiplexer: Multiplexer[ModuleRef, CompileEvent], compilation: Compilation, targetId: TargetId, layout: Layout) extends BuildClient {
  override def onBuildShowMessage(params: ShowMessageParams): Unit =
    multiplexer(targetId.ref) = Print(targetId.ref, params.getMessage)

  override def onBuildLogMessage(params: LogMessageParams): Unit =
    multiplexer(targetId.ref) = Print(targetId.ref, params.getMessage)

  override def onBuildPublishDiagnostics(params: PublishDiagnosticsParams): Unit = {
    val targetId: TargetId = getTargetId(params.getBuildTarget.getUri)
    val fileName = new java.net.URI(params.getTextDocument.getUri).getRawPath
    val repos = compilation.checkouts.map { checkout => (checkout.path.value, checkout.repoId)}.toMap

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

      val linePrefix            = codeLine.take(charNum)
      val (matching, remainder) = takeSame(codeLine.drop(linePrefix.length))
      val highlightedLine       = linePrefix + Ansi.brightRed(Ansi.underline(matching)) + remainder

      val (repo, filePath) = repos.find { case (k, v) => fileName.startsWith(k) }.map {
        case (k, v) => (v, Path(fileName.drop(k.length + 1)))
      }.getOrElse((RepoId("local"), Path(fileName.drop(layout.baseDir.value.length + 1))))

      import escritoire.Ansi

      implicitly[MsgShow[ModuleRef]]

      val severity = diag.getSeverity.toString.toLowerCase match {
        case "error" =>
          msg"${Ansi.Color.base01("[")}${Ansi.Color.red("E")}${Ansi.Color.base01("]")}"
        case "warning" =>
          msg"${Ansi.Color.base01("[")}${Ansi.Color.yellow("W")}${Ansi.Color.base01("]")}"
        case "information" =>
          msg"${Ansi.Color.base01("[")}${Ansi.Color.blue("I")}${Ansi.Color.base01("]")}"
        case _ => msg"${Ansi.Color.base01("[")}${Ansi.Color.blue("H")}${Ansi.Color.base01("]")}"
      }

      multiplexer(targetId.ref) = DiagnosticMsg(
        targetId.ref,
        CompilerDiagnostic(
          msg"""$severity ${targetId.ref}${'>'}${repo}${':'}${filePath} ${'+'}${lineNo}${':'}
${'|'} ${UserMsg(
            theme =>
              diag.getMessage
                .split("\n")
                .to[List]
                .map { ln =>
                  Ansi.Color.base1(ln)
                }
                .join(msg"""
${'|'} """.string(theme)))}
${'|'} ${highlightedLine.dropWhile(_ == ' ')}
""",
          repo,
          filePath,
          lineNo,
          charNum
        )
      )
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

  // FIXME: We should implement this using a regular expression
  private[this] def getTargetId(bspUri: String): TargetId = {
    val uriQuery = new java.net.URI(bspUri).getRawQuery
      .split("&")
      .map(_.split("=", 2))
      .map { x => x(0) -> x(1) }
      .toMap

    TargetId(uriQuery("id"))
  }

  override def onBuildTaskProgress(params: TaskProgressParams): Unit = {
    val report   = convertDataTo[CompileTask](params.getData)
    val targetId = getTargetId(report.getTarget.getUri)
    multiplexer(targetId.ref) = CompilationProgress(targetId.ref, params.getProgress.toDouble / params.getTotal)
  }

  override def onBuildTaskStart(params: TaskStartParams): Unit = {
    val report   = convertDataTo[CompileTask](params.getData)
    val targetId: TargetId = getTargetId(report.getTarget.getUri)
    multiplexer(targetId.ref) = StartCompile(targetId.ref)
    compilation.deepDependencies(targetId).foreach { dependencyTargetId =>
      multiplexer(dependencyTargetId.ref) = NoCompile(dependencyTargetId.ref)
    }
  }
  override def onBuildTaskFinish(params: TaskFinishParams): Unit = params.getDataKind match {
    case TaskDataKind.COMPILE_REPORT =>
      val report = convertDataTo[CompileReport](params.getData)
      val targetId: TargetId = getTargetId(report.getTarget.getUri)
      val ref = targetId.ref
      multiplexer(ref) = StopCompile(ref, params.getStatus == StatusCode.OK)
      if(!compilation.targets(ref).kind.needsExecution) multiplexer(ref) = StopRun(ref)
      else multiplexer(ref) = StartRun(ref)
  }
}

case class Compilation(graph: Map[TargetId, List[TargetId]],
                       subgraphs: Map[TargetId, List[TargetId]],
                       checkouts: Set[Checkout],
                       targets: Map[ModuleRef, Target],
                       universe: Universe) {

  private[this] val hashes: HashMap[ModuleRef, Digest] = new HashMap()
  lazy val allDependencies: Set[Target] = targets.values.to[Set]

  def apply(ref: ModuleRef): Try[Target] = targets.get(ref).ascribe(ItemNotFound(ref.moduleId))

  def checkoutAll(log: Log, layout: Layout, https: Boolean): Try[Unit] =
    checkouts.traverse(_.get(log, layout, https)).map{ _ => ()}

  def deepDependencies(targetId: TargetId): Set[TargetId] = {
    @tailrec
    def flatten[T](aggregated: Set[T], children: T => Set[T], next: Set[T]): Set[T] = {
      if(next.isEmpty) aggregated
      else {
        val node = next.head
        flatten(aggregated + node, children, next - node ++ children(node))
      }
    }
    flatten[TargetId](Set.empty, graph(_).to[Set], Set(targetId))
  }

  def generateFiles(log: Log, layout: Layout): Try[Iterable[Path]] = synchronized {
    Bloop.clean(layout).flatMap(Bloop.generateFiles(log, this, layout).waive)
  }

  def classpath(ref: ModuleRef, layout: Layout): Set[Path] = allDependencies.flatMap { target =>
    Set(layout.classesDir(target.id), layout.resourcesDir(target.id))
  } ++ allDependencies.flatMap(_.binaries) ++ targets(ref).binaries

  def allSources: Set[Path] = targets.values.to[Set].flatMap{x: Target => x.sourcePaths.to[Set]}

  def writePlugin(ref: ModuleRef, layout: Layout): Unit = {
    val target = targets(ref)
    if(target.kind == Plugin) {
      val file = layout.classesDir(target.id) / "scalac-plugin.xml"

      target.main.foreach { main =>
        file.writeSync(str"""|<plugin>
                             | <name>${target.plugin.getOrElse("plugin"): String}</name>
                             | <classname>${main}</classname>
                             |</plugin>""".stripMargin)
      }
    }
  }

  def saveNative(log: Log, ref: ModuleRef, dest: Path, layout: Layout, main: String): Try[Unit] =
    for {
      dest <- dest.directory
      cp   = runtimeClasspath(ref, layout).to[List].map(_.value)
      _    <- Shell(layout.env).native(dest, cp, main)
    } yield ()

  def saveJars(log: Log,
               ref: ModuleRef,
               srcs: Set[Path],
               destination: Path,
               layout: Layout,
               fatJar: Boolean)
  : Try[Unit] = {
    val bins = allDependencies.flatMap(_.binaries)
    for {
      entity           <- universe.entity(ref.projectId)
      module           <- entity.project(ref.moduleId)
      manifest          = Manifest(bins.map(_.name), module.main)
      dest             <- destination.directory
      path              = (dest / str"${ref.projectId.key}-${ref.moduleId.key}.jar")
      _                 = log.info(msg"Saving JAR file ${path.relativizeTo(layout.baseDir)}")
      stagingDirectory <- aggregateCompileResults(ref, srcs, layout)
      _                <- if(fatJar) bins.traverse { bin => Zipper.unpack(bin, stagingDirectory) }
      else Success(())
      _                <- Shell(layout.env).jar(path, stagingDirectory.children.map(stagingDirectory / _).to[Set],
        manifest)
      _                <- if(!fatJar) bins.traverse { bin => bin.copyTo(dest / bin.name) } else Success(())
    } yield ()
  }

  private[this] def aggregateCompileResults(ref: ModuleRef,
                                            compileResults: Set[Path],
                                            layout: Layout): Try[Path] = {
    val stagingDirectory = layout.workDir(targets(ref).id) / "staging"
    for(_ <- compileResults.filter(_.exists()).traverse(_.copyTo(stagingDirectory))) yield stagingDirectory
  }

  def allParams(log: Log, ref: ModuleRef, layout: Layout): List[String] = {
    def pluginParam(pluginTarget: Target): Parameter =
      Parameter(str"Xplugin:${layout.classesDir(pluginTarget.id)}")

    val allPlugins = allDependencies
      .filter(_.kind == Plugin)
      .filterNot(_.ref == ref)

    val params = targets(ref).params ++ allPlugins.map(pluginParam)

    params.map(_.parameter)
  }

  def jmhRuntimeClasspath(ref: ModuleRef, classesDirs: Set[Path], layout: Layout): Set[Path] =
    classesDirs ++ targets(ref).compiler.to[Set].map { compilerTarget =>
      layout.resourcesDir(compilerTarget.id)
    } ++ classpath(ref, layout)

  def runtimeClasspath(ref: ModuleRef, layout: Layout): Set[Path] =
    targets(ref).compiler.to[Set].flatMap { compilerTarget =>
      Set(layout.classesDir(compilerTarget.id), layout.resourcesDir(compilerTarget.id))
    } ++ classpath(ref, layout) + layout.classesDir(targets(ref).id) + layout.resourcesDir(targets(ref).id)

  def compileModule(log: Log,
                    target: Target,
                    layout: Layout,
                    application: Boolean,
                    multiplexer: Multiplexer[ModuleRef, CompileEvent],
                    pipelining: Boolean)
  : Future[CompileResult] = Future.fromTry {

    val uri: String = str"file://${layout.workDir(target.id).value}?id=${target.id.key}"
    val params = new CompileParams(List(new BuildTargetIdentifier(uri)).asJava)
    if(pipelining) params.setArguments(List("--pipeline").asJava)
    val furyTargetIds = deepDependencies(target.id).toList
    val bspTargetIds = furyTargetIds.map { dep =>
      new BuildTargetIdentifier(str"file://${layout.workDir(dep).value}?id=${dep.key}")
    }
    val bspToFury = (bspTargetIds zip furyTargetIds).toMap
    val scalacOptionsParams = new ScalacOptionsParams(bspTargetIds.asJava)

    BloopServer.borrow(layout.baseDir, multiplexer, this, target.id, layout) { conn =>
      
      val result: Try[CompileResult] = {
        for {
          res <- wrapServerErrors(conn.server.buildTargetCompile(params))
          opts <- wrapServerErrors(conn.server.buildTargetScalacOptions(scalacOptionsParams))
        } yield CompileResult(res, opts)
      }

      //conn.writeTrace(layout)
      //conn.writeMessages(layout)
      
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

      result.get
    }
  }

  private[this] def wrapServerErrors[T](f: => CompletableFuture[T]): Try[T] =
    Outcome.rescue[ExecutionException] { e: ExecutionException => BuildServerError(e.getCause) } (f.get)


  def compile(log: Log,
              moduleRef: ModuleRef,
              multiplexer: Multiplexer[ModuleRef, CompileEvent],
              futures: Map[TargetId, Future[CompileResult]] = Map(),
              layout: Layout,
              globalPolicy: Policy,
              args: List[String],
              pipelining: Boolean)
  : Map[TargetId, Future[CompileResult]] = {
    val target = targets(moduleRef)

    val newFutures = subgraphs(target.id).foldLeft(futures) { (futures, dependencyTarget) =>
      if(futures.contains(dependencyTarget)) futures
      else compile(log, dependencyTarget.ref, multiplexer, futures, layout, globalPolicy, args, pipelining)
    }

    val dependencyFutures = Future.sequence(subgraphs(target.id).map(newFutures))

    val future = dependencyFutures.map(CompileResult.merge).flatMap { required =>
      if(!required.isSuccessful) {
        multiplexer(target.ref) = SkipCompile(target.ref)
        multiplexer.close(target.ref)
        Future.successful(required)
      } else {
        val noCompilation = target.sourcePaths.isEmpty

        if(noCompilation) deepDependencies(target.id).foreach { targetId =>
          multiplexer(targetId.ref) = NoCompile(targetId.ref)
        }

        compileModule(log, target, layout, target.kind == Application, multiplexer, pipelining).map {
          case compileResult if compileResult.isSuccessful && target.kind.needsExecution =>
            val classDirectories = compileResult.classDirectories
            val runSuccess = run(target, classDirectories, multiplexer, layout, globalPolicy, args) == 0
            if(runSuccess) compileResult else compileResult.failed
          case otherResult =>
            otherResult
        }
      }
    }

    newFutures.updated(target.id, future)
  }

  private def run(target: Target, classDirectories: Set[Path], multiplexer: Multiplexer[ModuleRef, CompileEvent],
                  layout: Layout, globalPolicy: Policy, args: List[String]): Int = {
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
      if (target.kind == Benchmarks) "org.openjdk.jmh.Main" else target.main.getOrElse(""),
      securePolicy = target.kind == Application,
      env = target.environment,
      properties = target.properties,
      policy = globalPolicy.forContext(layout, target.ref.projectId),
      layout = layout,
      args
    ) { ln =>
      multiplexer(target.ref) = Print(target.ref, ln)
    }.await()

    deepDependencies(target.id).foreach { targetId =>
      multiplexer(targetId.ref) = NoCompile(targetId.ref)
    }

    multiplexer.close(target.ref)
    multiplexer(target.ref) = StopRun(target.ref)

    exitCode
  }

}
