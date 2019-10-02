package fury.core

import java.io._
import java.net.URI
import java.nio.charset.StandardCharsets
import java.time.LocalDateTime
import java.util.concurrent.{CompletableFuture, Executors}

import ch.epfl.scala.bsp4j.{CompileResult => _, _}
import com.google.gson.{Gson, JsonElement}
import fury.core.Graph.CompilerDiagnostic
import fury.strings._
import fury.io.{Path, Zipper}
import fury.model._
import fury.utils._
import gastronomy.Digest
import mercator._
import org.eclipse.lsp4j.jsonrpc.Launcher

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.collection.mutable.HashMap
import scala.collection.JavaConverters._
import scala.concurrent._
import scala.util._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.reflect.{ClassTag, classTag}


trait FuryBspServer extends BuildServer with ScalaBuildServer

case class BspConnection(future: java.util.concurrent.Future[Void],
                         client: BuildingClient,
                         server: FuryBspServer,
                         traceBuffer: CharArrayWriter,
                         messageBuffer: CharArrayWriter) {

  def shutdown(): Unit = {
    server.buildShutdown()
    future.cancel(true)
  }

  def provision[T](currentCompilation: Compilation,
                   targetId: TargetId,
                   layout: Layout,
                   currentMultiplexer: Option[Multiplexer[ModuleRef, CompileEvent]])
                  (action: FuryBspServer => T)
  : T = {
    client.compilation = currentCompilation
    client.targetId = targetId
    client.layout = layout
    client.multiplexer = currentMultiplexer
    action(server)
  }

  def writeTrace(layout: Layout): Try[Unit] = layout.traceLogfile.appendSync(traceBuffer.toString)
  def writeMessages(layout: Layout): Try[Unit] = layout.messagesLogfile.appendSync(messageBuffer.toString)
}

object BspConnectionManager {
  case class Handle(in: OutputStream,
                    out: InputStream,
                    err: InputStream,
                    broken: Promise[Unit],
                    launcher: Future[Unit])
    extends AutoCloseable {

    lazy val errReader = new BufferedReader(new InputStreamReader(err))

    override def close(): Unit = {
      broken.success(())
      in.close()
      out.close()
      err.close()
    }
  }

  object HandleHandler {
    private val handles: scala.collection.mutable.Map[Handle, PrintWriter] = TrieMap()

    private val ec: ExecutionContext =
      ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor(), throw _)

    def handle(handle: Handle, sink: PrintWriter): Unit = handles(handle) = sink

    Future {
      while(true) {
        handles.foreach {
          case (handle, sink) if !handle.broken.isCompleted =>
            try {
              val line = handle.errReader.readLine()
              if(line != null) sink.println(line)
            } catch {
              case e: IOException =>
                sink.println("Broken handle!")
                e.printStackTrace(sink)
                handles -= handle
                handle.broken.failure(e)
            }
        }
        Thread.sleep(100)
      }
    } (ec)

  }

  import bloop.launcher.LauncherMain
  import bloop.launcher.LauncherStatus._

  private val bloopVersion = "1.3.2"

  def bloopLauncher: Handle = {

    val bloopIn = new PipedInputStream
    val in = new PipedOutputStream
    in.connect(bloopIn)

    val bloopOut = new PipedOutputStream
    val out = new PipedInputStream
    out.connect(bloopOut)

    val bloopErr = new PipedOutputStream
    val err = new PipedInputStream
    err.connect(bloopErr)

    val launcher = new LauncherMain(
      clientIn = bloopIn,
      clientOut = bloopOut,
      out = new PrintStream(bloopErr),
      charset = StandardCharsets.UTF_8,
      shell = bloop.launcher.core.Shell.default,
      nailgunPort = None,
      startedServer = Promise[Unit](),
      generateBloopInstallerURL = bloop.launcher.core.Installer.defaultWebsiteURL
    )

    val future = Future(blocking {
      launcher.runLauncher(
        bloopVersionToInstall = bloopVersion,
        bloopInstallerURL = bloop.launcher.core.Installer.defaultWebsiteURL(bloopVersion),
        skipBspConnection = false,
        serverJvmOptions = Nil
      )
    }).map {
      case SuccessfulRun => ()
      case failure       => throw new Exception(s"Launcher failed: $failure")
    }

    Handle(in, out, err, Promise[Unit], future)
  }
}

object Compilation {
  private val compilationThreadPool = Executors.newCachedThreadPool()

  val bspPool: Pool[Path, BspConnection] = new Pool[Path, BspConnection](60000L) {

    def destroy(value: BspConnection): Unit = value.shutdown()
    def isBad(value: BspConnection): Boolean = value.future.isDone

    def create(dir: Path): BspConnection = {
      val bspMessageBuffer = new CharArrayWriter()
      val bspTraceBuffer = new CharArrayWriter()
      val log = new java.io.PrintWriter(bspTraceBuffer, true)
      val handle = BspConnectionManager.bloopLauncher
      BspConnectionManager.HandleHandler.handle(handle, log)
      val client = new BuildingClient(messageSink = new PrintWriter(bspMessageBuffer))

      val launcher = new Launcher.Builder[FuryBspServer]()
        .traceMessages(log)
        .setRemoteInterface(classOf[FuryBspServer])
        .setExecutorService(compilationThreadPool)
        .setInput(handle.out)
        .setOutput(handle.in)
        .setLocalService(client)
        .create()

      val future = launcher.startListening()
      val server = launcher.getRemoteProxy
      val capabilities = new BuildClientCapabilities(List("scala").asJava)

      val initializeParams = new InitializeBuildParams("fury", Version.current, "2.0.0-M4", dir.uriString,
        capabilities)

      server.buildInitialize(initializeParams).get
      server.onBuildInitialized()
      val bspConn = BspConnection(future, client, server, bspTraceBuffer, bspMessageBuffer)

      handle.broken.future.andThen {
        case Success(_) =>
          log.println(msg"Connection for $dir has been closed")
          log.flush()
          bspConn.future.cancel(true)
        case Failure(e) =>
          log.println(msg"Connection for $dir is broken. Cause: ${e.getMessage}")
          e.printStackTrace(log)
          log.flush()
          bspConn.future.cancel(true)
      }
      bspConn
    }
  }

  private val compilationCache: collection.mutable.Map[Path, Future[Try[Compilation]]] = TrieMap()

  private[this] def mkCompilation(io: Io,
                    schema: Schema,
                    ref: ModuleRef,
                    layout: Layout,
                    globalLayout: GlobalLayout,
                    https: Boolean)
  : Try[Compilation] = for {

    hierarchy   <- schema.hierarchy(io, layout.base, layout, https)
    universe    <- hierarchy.universe
    policy      <- Policy.read(io, globalLayout)
    compilation <- universe.compilation(io, ref, policy, layout)
    _           <- compilation.generateFiles(io, layout)

    _           <- compilation.bspUpdate(io, compilation.targets(ref).id, layout)

  } yield compilation

  def asyncCompilation(io: Io,
                       schema: Schema,
                       ref: ModuleRef,
                       layout: Layout,
                       globalLayout: GlobalLayout,
                       https: Boolean)
  : Future[Try[Compilation]] = {

    def fn: Future[Try[Compilation]] = Future(mkCompilation(io, schema, ref, layout, globalLayout, https))

    compilationCache(layout.furyDir) = compilationCache.get(layout.furyDir) match {
      case Some(future) => future.transformWith(fn.waive)
      case None         => fn
    }

    compilationCache(layout.furyDir)
  }

  def syncCompilation(io: Io,
                      schema: Schema,
                      ref: ModuleRef,
                      layout: Layout,
                      globalLayout: GlobalLayout,
                      https: Boolean): Try[Compilation] = {
    val compilation = mkCompilation(io, schema, ref, layout, globalLayout, https)
    compilationCache(layout.furyDir) = Future.successful(compilation)
    compilation
  }
}

class BuildingClient(messageSink: PrintWriter) extends BuildClient {
  var compilation: Compilation = _
  var targetId: TargetId = _
  var layout: Layout = _
  var multiplexer: Option[Multiplexer[ModuleRef, CompileEvent]] = None

  override def onBuildShowMessage(params: ShowMessageParams): Unit = {
    multiplexer.foreach(_(targetId.ref) = Print(targetId.ref, params.getMessage))
    messageSink.println(s"${LocalDateTime.now} showMessage: ${params.getMessage}")
  }

  override def onBuildLogMessage(params: LogMessageParams): Unit = {
    multiplexer.foreach(_(targetId.ref) = Print(targetId.ref, params.getMessage))
    messageSink.println(s"${LocalDateTime.now}  logMessage: ${params.getMessage}")
  }

  override def onBuildPublishDiagnostics(params: PublishDiagnosticsParams): Unit = {
    val targetId: TargetId = getTargetId(params.getBuildTarget.getUri)
    val fileName = new java.net.URI(params.getTextDocument.getUri).getRawPath
    val repos = compilation.checkouts.map { checkout => (checkout.path(layout).value, checkout.repoId)}.toMap

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
      }.getOrElse((RepoId("local"), Path(fileName.drop(layout.base.value.length + 1))))

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

      multiplexer.foreach { mp =>
        mp(targetId.ref) = DiagnosticMsg(
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
  }

  override def onBuildTargetDidChange(params: DidChangeBuildTarget): Unit = {}

  def convertDataTo[A: ClassTag](data: Object): A = {
    val gson = new Gson()
    val json = data.asInstanceOf[JsonElement]
    val report =
      gson.fromJson[A](json, classTag[A].runtimeClass)
    report
  }

  // FIXME: We should implement this using a regular expression
  def getTargetId(bspUri: String): TargetId = {
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
    multiplexer.foreach {
      _(targetId.ref) = CompilationProgress(targetId.ref, params.getProgress.toDouble / params.getTotal)
    }
  }

  override def onBuildTaskStart(params: TaskStartParams): Unit = {
    val report   = convertDataTo[CompileTask](params.getData)
    val targetId: TargetId = getTargetId(report.getTarget.getUri)
    multiplexer.foreach { mp => mp(targetId.ref) = StartCompile(targetId.ref) }
    compilation.deepDependencies(targetId).foreach { dependencyTargetId =>
      multiplexer.foreach(_(dependencyTargetId.ref) = NoCompile(dependencyTargetId.ref))
    }
  }
  override def onBuildTaskFinish(params: TaskFinishParams): Unit = params.getDataKind match {
    case TaskDataKind.COMPILE_REPORT =>
      val report = convertDataTo[CompileReport](params.getData)
      val targetId: TargetId = getTargetId(report.getTarget.getUri)
      val ref = targetId.ref
      multiplexer.foreach { mp =>
        mp(ref) = StopCompile(ref, params.getStatus == StatusCode.OK)
        if(!compilation.targets(ref).kind.needsExecution) mp(ref) = StopRun(ref)
        else mp(ref) = StartRun(ref)
      }
  }
}

case class Compilation(graph: Map[TargetId, List[TargetId]],
                       subgraphs: Map[TargetId, List[TargetId]],
                       checkouts: Set[Checkout],
                       targets: Map[ModuleRef, Target],
                       universe: Universe) {

  private[this] val hashes: HashMap[ModuleRef, Digest] = new HashMap()
  lazy val allDependencies: Set[Target] = targets.values.to[Set]

  def bspUpdate(io: Io, targetId: TargetId, layout: Layout): Try[Unit] =
    Await.result(Compilation.bspPool.borrow(layout.base) { conn =>
      conn.provision(this, targetId, layout, None) { server =>
        Try(server.workspaceBuildTargets.get)
      }
    }, Duration.Inf)

  def apply(ref: ModuleRef): Try[Target] = targets.get(ref).ascribe(ItemNotFound(ref.moduleId))

  def checkoutAll(io: Io, layout: Layout, https: Boolean): Try[Unit] =
    checkouts.traverse(_.get(io, layout, https)).map{ _ => ()}

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

  def generateFiles(io: Io, layout: Layout): Try[Iterable[Path]] = synchronized {
    Bloop.clean(layout).flatMap(Bloop.generateFiles(io, this, layout).waive)
  }

  def classpath(ref: ModuleRef, layout: Layout): Set[Path] = allDependencies.flatMap { target =>
    Set(layout.classesDir(target.id), layout.resourcesDir(target.id))
  } ++ allDependencies.flatMap(_.binaries) ++ targets(ref).binaries

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

  def saveNative(io: Io, ref: ModuleRef, dest: Path, layout: Layout, main: String): Try[Unit] =
    for {
      dest <- dest.directory
      cp   = runtimeClasspath(io, ref, layout).to[List].map(_.value)
      _    <- Shell(layout.env).native(dest, cp, main)
    } yield ()

  def saveJars(io: Io,
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
      _                 = io.println(msg"Saving JAR file ${path.relativizeTo(layout.base)}")
      stagingDirectory <- aggregateCompileResults(ref, srcs, layout)
      _                <- if(fatJar) bins.traverse { bin => Zipper.unpack(bin, stagingDirectory) }
      else Success()
      _                <- Shell(layout.env).jar(path, stagingDirectory.children.map(stagingDirectory / _).to[Set],
        manifest)
      _                <- if(!fatJar) bins.traverse { bin => bin.copyTo(dest / bin.name) } else Success()
    } yield ()
  }

  private[this] def aggregateCompileResults(ref: ModuleRef,
                                            compileResults: Set[Path],
                                            layout: Layout): Try[Path] = {
    val stagingDirectory = layout.workDir(targets(ref).id) / "staging"
    for(_ <- compileResults.filter(_.exists()).traverse(_.copyTo(stagingDirectory))) yield stagingDirectory
  }

  def allParams(io: Io, ref: ModuleRef, layout: Layout): List[String] =
    (targets(ref).params ++ allDependencies.filter(_.kind == Plugin).map { pluginTarget =>
      Parameter(str"Xplugin:${layout.classesDir(pluginTarget.id)}")
    }).map(_.parameter)

  def jmhRuntimeClasspath(io: Io, ref: ModuleRef, classesDirs: Set[Path], layout: Layout): Set[Path] =
    classesDirs ++ targets(ref).compiler.to[Set].map { compilerTarget =>
      layout.resourcesDir(compilerTarget.id)
    } ++ classpath(ref, layout)

  def runtimeClasspath(io: Io, ref: ModuleRef, layout: Layout): Set[Path] =
    targets(ref).compiler.to[Set].flatMap { compilerTarget =>
      Set(layout.classesDir(compilerTarget.id), layout.resourcesDir(compilerTarget.id))
    } ++ classpath(ref, layout) + layout.classesDir(targets(ref).id) + layout.resourcesDir(targets(ref).id)

  private[this] case object CompilationFailed extends Exception

  def compileModule(io: Io,
                    target: Target,
                    layout: Layout,
                    application: Boolean,
                    multiplexer: Multiplexer[ModuleRef, CompileEvent])
  : Future[CompileResult] = {

    val targetIdentifiers = deepDependencies(target.id).map { dep =>
      new BuildTargetIdentifier(str"file://${layout.workDir(dep).value}?id=${dep.key}")
    }

    def wrapServerErrors[T](f: => CompletableFuture[T]): Try[T] =
      Outcome.rescue[ExecutionException] { e: ExecutionException => BuildServerError(e.getCause) } (f.get)

    Compilation.bspPool.borrow(layout.base) { conn =>
      val result: Try[CompileResult] = conn.provision(this, target.id, layout, Some(multiplexer)) { server =>
        val uri: String = str"file://${layout.workDir(target.id).value}?id=${target.id.key}"

        val params = new CompileParams(List(new BuildTargetIdentifier(uri)).asJava)
        val scalacOptionsParams = new ScalacOptionsParams(targetIdentifiers.toList.asJava)

        (for {
          compileResult     <- wrapServerErrors(server.buildTargetCompile(params))
          statusCode         = compileResult.getStatusCode
          _                 <- if(statusCode == StatusCode.OK) ~() else Failure(CompilationFailed)
          scalacOptions     <- wrapServerErrors(server.buildTargetScalacOptions(scalacOptionsParams))
          outputDirectories  = scalacOptions.getItems.asScala.toSet.map { x: ScalacOptionsItem =>
            Path(new URI(x.getClassDirectory))
          }
        } yield CompileSuccess(outputDirectories)).recover { case CompilationFailed => CompileFailure }
      }

      conn.writeTrace(layout)
      conn.writeMessages(layout)

      result.get
    }
  }

  def compile(io: Io,
              moduleRef: ModuleRef,
              multiplexer: Multiplexer[ModuleRef, CompileEvent],
              futures: Map[TargetId, Future[CompileResult]] = Map(),
              layout: Layout,
              globalPolicy: Policy,
              args: List[String])
  : Map[TargetId, Future[CompileResult]] = {
    val target = targets(moduleRef)

    val newFutures = subgraphs(target.id).foldLeft(futures) { (futures, dependencyTarget) =>
      if(futures.contains(dependencyTarget)) futures
      else compile(io, dependencyTarget.ref, multiplexer, futures, layout, globalPolicy, args)
    }

    val dependencyFutures = Future.sequence(subgraphs(target.id).map(newFutures))

    val future = dependencyFutures.flatMap { inputs =>
      if(inputs.exists(!_.isSuccessful)) {
        multiplexer(target.ref) = SkipCompile(target.ref)
        multiplexer.close(target.ref)
        Future.successful(CompileFailure)
      } else {
        val noCompilation = target.sourcePaths.isEmpty

        if(noCompilation) deepDependencies(target.id).foreach { targetId =>
          multiplexer(targetId.ref) = NoCompile(targetId.ref)
        }

        compileModule(io, target, layout, target.kind == Application, multiplexer).map {
          case CompileSuccess(classDirectories) if target.kind.needsExecution =>
            if(target.kind == Benchmarks) {
              classDirectories.foreach { classDirectory =>
                Jmh.instrument(classDirectory, layout.benchmarksDir(target.id), layout.resourcesDir(target.id))
                val javaSources = layout.benchmarksDir(target.id).findChildren(_.endsWith(".java"))

                Shell(layout.env).javac(
                  classpath(target.ref, layout).to[List].map(_.value),
                  classDirectory.value,
                  javaSources.map(_.value).to[List])
              }
            }

            val res = Shell(layout.env).runJava(
              jmhRuntimeClasspath(io, target.ref, classDirectories, layout).to[List].map(_.value),
              if(target.kind == Benchmarks) "org.openjdk.jmh.Main" else target.main.getOrElse(""),
              securePolicy = target.kind == Application,
              env = target.environment,
              properties = target.properties,
              policy = globalPolicy.forContext(layout, target.ref.projectId),
              layout = layout,
              args
            ) { ln =>
              multiplexer(target.ref) = Print(target.ref, ln)
            }.await() == 0

            deepDependencies(target.id).foreach { targetId =>
              multiplexer(targetId.ref) = NoCompile(targetId.ref)
            }

            multiplexer.close(target.ref)
            multiplexer(target.ref) = StopRun(target.ref)

            if(res) CompileSuccess(classDirectories) else CompileFailure
          case compileResult =>
            compileResult
        }
      }
    }

    newFutures.updated(target.id, future)
  }

}
