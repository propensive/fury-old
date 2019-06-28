/*
  Fury, version 0.4.0. Copyright 2018-19 Jon Pretty, Propensive Ltd.

  The primary distribution site is: https://propensive.com/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required  by applicable  law or  agreed to  in writing,  software  distributed  under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
  express  or  implied.  See  the  License for  the specific  language  governing  permissions and
  limitations under the License.
 */
package fury.core

import fury._, io._, strings._, ogdl._

import Graph.{Compiling, CompilerDiagnostic, OtherMessage, DiagnosticMessage}

import exoskeleton._
import gastronomy._
import kaleidoscope._
import mercator._

import org.eclipse.lsp4j.jsonrpc.Launcher
import ch.epfl.scala.bsp4j.{CompileResult => _, _}
import com.google.gson.{Gson, JsonElement}

import scala.collection.immutable.{SortedSet, TreeSet}
import scala.collection.mutable.HashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.JavaConverters._
import scala.concurrent._
import scala.util._
import scala.concurrent.duration._
import scala.reflect.{ClassTag, classTag}

import java.io._
import java.net.URI
import java.nio.charset.StandardCharsets
import java.time.LocalDateTime
import java.util.concurrent.Executors

import language.higherKinds

object ManifestEntry {
  implicit val stringShow: StringShow[ManifestEntry] = _.key
  implicit val msgShow: MsgShow[ManifestEntry] = v =>
    UserMsg { t =>
      v.key
    }

  implicit val diff: Diff[ManifestEntry] =
    (l, r) => Diff.stringDiff.diff(l.pairString, r.pairString)
}

case class ManifestEntry(key: String, value: String) {
  def pairString: String = str"$key=$value"
}

object Kind {
  implicit val msgShow: MsgShow[Kind] = v =>
    UserMsg { t =>
      v.name
    }
  implicit val stringShow: StringShow[Kind] = _.name

  val all: List[Kind] = List(Library, Compiler, Plugin, Application, Benchmarks)

  def unapply(str: String): Option[Kind] = all.find(_.name == str)
}

sealed abstract class Kind(val name: String)
case object Library     extends Kind("library")
case object Compiler    extends Kind("compiler")
case object Plugin      extends Kind("plugin")
case object Application extends Kind("application")
case object Benchmarks  extends Kind("benchmarks")

object Module {
  implicit val msgShow: MsgShow[Module]       = v => UserMsg(_.module(v.id.key))
  implicit val stringShow: StringShow[Module] = _.id.key

  implicitly[StringShow[Path]]
  implicitly[MsgShow[Path]]
  implicitly[EntityName[Path]]
  implicitly[Diff[Path]]

  Diff.traversableDiff[Path]

  implicit val diff: Diff[Module] = Diff.gen[Module]

  def available(id: ModuleId, project: Project): Try[ModuleId] =
    project.modules
      .find(_.id == id)
      .map { module =>
        Failure(ModuleAlreadyExists(module.id))
      }
      .getOrElse(Success(id))
}

object Binary {
  implicit val msgShow: MsgShow[Binary]       = v => UserMsg(_.binary(v.spec))
  implicit val stringShow: StringShow[Binary] = _.spec
  implicit def diff: Diff[Binary]             = Diff.gen[Binary]

  def unapply(service: BinRepoId, string: String): Try[Binary] =
    string match {
      case r"$g@([\.\-_a-zA-Z0-9]*)\:$a@([\.\-_a-zA-Z0-9]*)\:$v@([\.\-\+_a-zA-Z0-9]*)" =>
        Success(Binary(service, g, a, v))
      case _ =>
        Failure(InvalidArgValue("binary", string))
    }

  private val compilerVersionCache: HashMap[Binary, Try[String]] =
    HashMap()
}

case class Binary(binRepo: BinRepoId, group: String, artifact: String, version: String) {
  def spec = str"$group:$artifact:$version"

  def paths(io: Io): Future[List[Path]] = Coursier.fetch(io, this)
}

case class Module(
    id: ModuleId,
    kind: Kind = Library,
    main: Option[String] = None,
    plugin: Option[String] = None,
    manifest: List[ManifestEntry] = List(),
    compiler: ModuleRef = ModuleRef.JavaRef,
    after: SortedSet[ModuleRef] = TreeSet(),
    params: SortedSet[Parameter] = TreeSet(),
    sources: SortedSet[Source] = TreeSet(),
    binaries: SortedSet[Binary] = TreeSet(),
    resources: SortedSet[Path] = TreeSet(),
    bloopSpec: Option[BloopSpec] = None) {

  def allBinaries: SortedSet[Binary] =
    if (kind == Benchmarks)
      binaries + Binary(BinRepoId.Central, "org.openjdk.jmh", "jmh-core", "1.21")
    else binaries

  def compilerDependencies: Set[ModuleRef] =
    Set(compiler).filter(_ != ModuleRef.JavaRef).map(_.hide)

  def ref(project: Project): ModuleRef = ModuleRef(project.id, id)

  def externalSources: SortedSet[ExternalSource] = sources.collect {
    case src: ExternalSource => src
  }

  def sharedSources: SortedSet[SharedSource] = sources.collect {
    case src: SharedSource => src
  }

  def localSources: SortedSet[Path] = sources.collect { case src: LocalSource => src.path }
}

object BloopSpec {
  implicit val msgShow: MsgShow[BloopSpec]       = v => msg"${v.org}:${v.name}"
  implicit val stringShow: StringShow[BloopSpec] = bs => str"${bs.org}:${bs.name}"
  implicit def diff: Diff[BloopSpec]             = Diff.gen[BloopSpec]

  def parse(str: String): Try[BloopSpec] = str match {
    case r"$org@([a-z][a-z0-9_\-\.]*):$id@([a-z][a-z0-9_\-\.]*):$version@([0-9a-z][A-Za-z0-9_\-\.]*)" =>
      Success(BloopSpec(org, id, version))
    case _ =>
      Failure(InvalidValue(str))
  }
}

case class BloopSpec(org: String, name: String, version: String)

trait FuryBspServer extends BuildServer with ScalaBuildServer

case class BspConnection(
    future: java.util.concurrent.Future[Void],
    client: BuildingClient,
    server: FuryBspServer,
    traceBuffer: CharArrayWriter,
    messageBuffer: CharArrayWriter
  ) {

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

  def writeTrace(layout: Layout): Try[Unit] = {
    layout.traceLogfile.appendSync(traceBuffer.toString)
  }

  def writeMessages(layout: Layout): Try[Unit] = {
    layout.messagesLogfile.appendSync(messageBuffer.toString)
  }
}



object BspConnectionManager {

  case class Handle(in: OutputStream, out: InputStream, err: InputStream, broken: Promise[Unit]) extends AutoCloseable {

    lazy val errReader = new BufferedReader(new InputStreamReader(err))

    override def close(): Unit = {
      broken.success(())
      in.close()
      out.close()
      err.close()
    }
  }

  object HandleHandler {
    private val handles: scala.collection.mutable.Map[Handle, PrintWriter] = scala.collection.concurrent.TrieMap()

    private val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor(), throw _)

    def handle(handle: Handle, sink: PrintWriter): Unit = {
      handles(handle) = sink
    }

    Future{
      while(true){
        handles.foreach{
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
    }(ec)

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

    val handle = Handle(in, out, err, Promise[Unit])

    Future(blocking{
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
      launcher.runLauncher(
        bloopVersionToInstall = bloopVersion,
        bloopInstallerURL = bloop.launcher.core.Installer.defaultWebsiteURL(bloopVersion),
        skipBspConnection = false,
        serverJvmOptions = Nil
      )
    }).map{ status =>
      System.out.println(s"Launcher status: $status")
      status match {
        case SuccessfulRun => ()
        case x => throw new Exception(s"Launcher failed: $x")
      }
    }

    handle
  }

}

object Compilation {

  private val compilationThreadPool = Executors.newCachedThreadPool()

  val bspPool: Pool[Path, BspConnection] = new RetryingPool[Path, BspConnection, java.util.concurrent.ExecutionException](60000L) {

    def destroy(value: BspConnection): Unit = value.shutdown()

    def isBad(value: BspConnection): Boolean = {
      value.future.isDone
    }

    def create(dir: Path): BspConnection = {
      val bspMessageBuffer = new CharArrayWriter()
      val bspTraceBuffer = new CharArrayWriter()
      val log = new java.io.PrintWriter(bspTraceBuffer, true)
      log.println(s"----------- ${LocalDateTime.now} --- Compilation log for ${dir.value}")
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
      val initializeParams = new InitializeBuildParams(
          "fury",
          Version.current,
          "2.0.0-M4",
          dir.uriString,
          new BuildClientCapabilities(List("scala").asJava)
      )
      server.buildInitialize(initializeParams).get
      server.onBuildInitialized()
      val x = BspConnection(future, client, server, bspTraceBuffer, bspMessageBuffer)
      handle.broken.future.andThen{
        case Success(_) =>
          log.println(s"Connection for ${dir.value} has been closed.")
          log.flush()
          x.future.cancel(true)
        case Failure(e) =>
          log.println(s"Connection for ${dir.value} is broken! Cause: ${e.getMessage}")
          e.printStackTrace(log)
          log.flush()
          x.future.cancel(true)
      }
      x
    }
  }

  private val compilationCache: collection.mutable.Map[Path, Future[Try[Compilation]]] = collection.concurrent.TrieMap()

  def mkCompilation(io: Io, schema: Schema, ref: ModuleRef, layout: Layout): Try[Compilation] = for {
    hierarchy   <- schema.hierarchy(io, layout.base, layout)
    universe    <- hierarchy.universe
    compilation <- universe.compilation(io, ref, layout)
    _           <- compilation.generateFiles(io, layout)
    _           <- compilation.bspUpdate(io, compilation.targets(ref).id, layout).recover{case x: Throwable => io.println(str"$schema --- ${x.getMessage}")}
  } yield compilation

  def asyncCompilation(io: Io, schema: Schema, ref: ModuleRef, layout: Layout): Future[Try[Compilation]] = {
    def fn: Future[Try[Compilation]] = Future(mkCompilation(io, schema, ref, layout))
    compilationCache(layout.furyDir) = compilationCache.get(layout.furyDir) match {
      case Some(future) => future.transformWith { case _ : Try[Compilation] => fn }
      case None => fn
    }
    compilationCache(layout.furyDir)
  }

  def syncCompilation(io: Io, schema: Schema, ref: ModuleRef, layout: Layout): Try[Compilation] =
    Await.result(asyncCompilation(io, schema, ref, layout), Duration.Inf)
}

object LineNo {
  implicit val msgShow: MsgShow[LineNo] = v => UserMsg(_.lineNo(v.line.toString))
}
case class LineNo(line: Int) extends AnyVal

class BuildingClient(messageSink: PrintWriter) extends BuildClient {
  var compilation: Compilation                          = _
  var targetId: TargetId                                = _
  var layout: Layout                                    = _
  var multiplexer: Option[Multiplexer[ModuleRef, CompileEvent]] = None

  override def onBuildShowMessage(params: ShowMessageParams): Unit = {
    multiplexer.foreach(_(targetId.ref) = Print(params.getMessage))
    messageSink.println(s"${LocalDateTime.now} showMessage: ${params.getMessage}")
  }

  override def onBuildLogMessage(params: LogMessageParams): Unit = {
    multiplexer.foreach(_(targetId.ref) = Print(params.getMessage))
    messageSink.println(s"${LocalDateTime.now}  logMessage: ${params.getMessage}")
  }

  override def onBuildPublishDiagnostics(params: PublishDiagnosticsParams): Unit = {
    val targetId: TargetId = getTargetId(params.getBuildTarget.getUri)
    val fileName = new java.net.URI(params.getTextDocument.getUri).getRawPath

    val repos = compilation.checkouts.map { checkout =>
      (checkout.path(layout).value, checkout.repoId)
    }.toMap

    params.getDiagnostics.asScala.foreach { diag =>
      val lineNo  = LineNo(diag.getRange.getStart.getLine + 1)
      val charNum = diag.getRange.getStart.getCharacter
      // FIXME: This reads the same file potentially many times
      val codeLine = scala.io.Source.fromFile(fileName).getLines.toList(lineNo.line - 1)

      def isSymbolic(ch: Char)     = (ch == '_' || !ch.isLetterOrDigit) && ch != ' '
      def isAlphanumeric(ch: Char) = ch == '_' || ch.isLetterOrDigit

      def takeSame(str: String): (String, String) = {
        val ch = str.find(_ != '_').getOrElse('_')
        val matching =
          if (isSymbolic(ch)) str.takeWhile(isSymbolic) else str.takeWhile(isAlphanumeric)
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
      .to[List]
      .map(_.split("=", 2))
      .map { x =>
        x(0) -> x(1)
      }
      .toMap

    TargetId(uriQuery("id"))
  }

  override def onBuildTaskProgress(params: TaskProgressParams): Unit = {
    val report   = convertDataTo[CompileTask](params.getData)
    val targetId = getTargetId(report.getTarget.getUri)
    multiplexer.foreach { mp =>
      mp(targetId.ref) = CompilationProgress(targetId.ref, params.getProgress.toDouble / params.getTotal)
    }
  }

  override def onBuildTaskStart(params: TaskStartParams): Unit = {
    val report   = convertDataTo[CompileTask](params.getData)
    val targetId = getTargetId(report.getTarget.getUri)
    multiplexer.foreach { mp => mp(targetId.ref) = StartCompile(targetId.ref) }
    compilation.deepDependencies(targetId).foreach { dependencyTargetId =>
      multiplexer.foreach { mp => mp(dependencyTargetId.ref) = NoCompile(dependencyTargetId.ref) }
    }
  }
  override def onBuildTaskFinish(params: TaskFinishParams): Unit = {
    params.getDataKind match {
      case TaskDataKind.COMPILE_REPORT =>
        val report = convertDataTo[CompileReport](params.getData)
        val targetId = getTargetId(report.getTarget.getUri)
        params.getStatus match {
          case StatusCode.OK =>
            multiplexer.foreach { mp => mp(targetId.ref) = StopCompile(targetId.ref, true) }
          case StatusCode.ERROR =>
            multiplexer.foreach { mp => mp(targetId.ref) = StopCompile(targetId.ref, false) }
          case StatusCode.CANCELLED =>
            multiplexer.foreach { mp => mp(targetId.ref) = StopCompile(targetId.ref, false) }
        }
    }
  }
}

case class Compilation(
    graph: Map[TargetId, List[TargetId]],
    subgraphs: Map[TargetId, List[TargetId]],
    checkouts: Set[Checkout],
    targets: Map[ModuleRef, Target],
    universe: Universe) {

  private[this] val hashes: HashMap[ModuleRef, Digest] = new HashMap()

  lazy val allDependencies: Set[Target] = targets.values.to[Set]

  def bspUpdate(io: Io, targetId: TargetId, layout: Layout): Try[Unit] =
    Compilation.bspPool.borrow(layout.furyDir) { conn =>
      conn.provision(this, targetId, layout, None) { server =>
        Try{ server.workspaceBuildTargets.get }.map(_.getTargets.asScala.toString)
      }
    }

  def apply(ref: ModuleRef): Try[Target] =
    targets.get(ref).ascribe(ItemNotFound(ref.moduleId))

  def deepDependencies(targetId: TargetId): Set[TargetId] =
    Set(targetId) ++ graph(targetId).to[Set].flatMap(deepDependencies(_))

  def checkoutAll(io: Io, layout: Layout): Unit =
    checkouts.foreach(_.get(io, layout).unit)

  def generateFiles(io: Io, layout: Layout): Try[Iterable[Path]] = {
    Bloop.clean(layout).flatMap{ _ => Bloop.generateFiles(io, this, layout) }
  }

  def classpath(ref: ModuleRef, layout: Layout): Set[Path] =
    allDependencies.flatMap { target =>
      Set(layout.classesDir(target.id), layout.resourcesDir(target.id))
    } ++ allDependencies.flatMap(_.binaries) ++ targets(ref).binaries

  def writePlugin(ref: ModuleRef, layout: Layout): Unit = {
    val target = targets(ref)
    if (target.kind == Plugin) {
      val file = layout.classesDir(target.id) / "scalac-plugin.xml"

      target.main.foreach { main =>
        file.writeSync(
            str"<plugin><name>${target.plugin.getOrElse("plugin"): String}</name><classname>${main}</classname></plugin>")
      }
    }
  }

  def saveNative(io: Io, ref: ModuleRef, dest: Path, layout: Layout, main: String): Try[Unit] =
    for {
      dest <- dest.directory
      cp   = runtimeClasspath(io, ref, layout).to[List].map(_.value)
      _    <- layout.shell.native(dest, cp, main)
    } yield ()

  def saveJars(io: Io, ref: ModuleRef, srcs: Set[Path], dest: Path, layout: Layout): Try[Unit] =
    for {
      dest <- dest.directory
      bins         <- ~allDependencies.flatMap(_.binaries)
      manifestFile <- Manifest.file(layout.manifestFile(targets(ref).id), bins.map(_.name), None)
      path         <- ~(dest / str"${ref.projectId.key}-${ref.moduleId.key}.jar")
      _            <- ~io.println(msg"Saving JAR file ${path.relativizeTo(layout.base)}")
      stagingDirectory <- aggregateCompileResults(ref, srcs, layout)
      _            <- layout.shell.jar(path, Set(stagingDirectory -> stagingDirectory.children), manifestFile)
      _            <- bins.traverse { bin => bin.copyTo(dest / bin.name) }
    } yield ()

  private def aggregateCompileResults(ref: ModuleRef, compileResults: Set[Path], layout: Layout): Try[Path] = {
    val stagingDirectory = layout.workDir(targets(ref).id) / "staging"
    for{
      _ <- compileResults.filter(_.exists()).traverse{case x => x.copyTo(stagingDirectory)}
    } yield stagingDirectory
  }

  def allParams(io: Io, ref: ModuleRef, layout: Layout): List[String] =
    (targets(ref).params ++ allDependencies.filter(_.kind == Plugin).map { pluginTarget =>
      Parameter(str"Xplugin:${layout.classesDir(pluginTarget.id)}")
    }).map(_.parameter)

  def jmhRuntimeClasspath(io: Io, ref: ModuleRef, layout: Layout): Set[Path] =
    targets(ref).compiler.to[Set].flatMap { compilerTarget =>
      Set(layout.classesDir(compilerTarget.id), layout.resourcesDir(compilerTarget.id))
    } ++ classpath(ref, layout)

  def runtimeClasspath(io: Io, ref: ModuleRef, layout: Layout): Set[Path] =
    targets(ref).compiler.to[Set].flatMap { compilerTarget =>
      Set(layout.classesDir(compilerTarget.id), layout.resourcesDir(compilerTarget.id))
    } ++ classpath(ref, layout) + layout.classesDir(targets(ref).id) + layout.resourcesDir(targets(ref).id)

  def compileModule(io: Io,
                    target: Target,
                    layout: Layout,
                    application: Boolean,
                    multiplexer: Multiplexer[ModuleRef, CompileEvent])
                    : Future[CompileResult] = {
    val targetIdentifiers = deepDependencies(target.id)
      .map{ dep => new BuildTargetIdentifier(s"file://${layout.workDir(dep).value}?id=${dep.key}")}
    Future (blocking {
      Compilation.bspPool.borrow(layout.furyDir) { conn =>
        conn.provision(this, target.id, layout, Some(multiplexer)) { server =>
          val uri: String = s"file://${layout.workDir(target.id).value}?id=${target.id.key}"
          val result = try {
            val statusCode = if (application)
              server.buildTargetRun(new RunParams(new BuildTargetIdentifier(uri))).get.getStatusCode
            else
              server.buildTargetCompile(new CompileParams(List(new BuildTargetIdentifier(uri)).asJava)).get.getStatusCode
            if(statusCode == StatusCode.OK){
              val outputDirectories = server.buildTargetScalacOptions(new ScalacOptionsParams(targetIdentifiers.toList.asJava)).get
                .getItems.asScala.toSet.map{x: ScalacOptionsItem => Path(new URI(x.getClassDirectory))}
              CompileSuccess(outputDirectories)
            } else CompileFailure
          } catch {
            case e: java.util.concurrent.ExecutionException => throw BuildServerError(e.getCause)
          }
          conn.writeTrace(layout)
          conn.writeMessages(layout)
          result
        }
      }
    })
  }


  def compile(io: Io,
              moduleRef: ModuleRef,
              multiplexer: Multiplexer[ModuleRef, CompileEvent],
              futures: Map[TargetId, Future[CompileResult]] = Map(),
              layout: Layout)
              : Map[TargetId, Future[CompileResult]] = {
    val target = targets(moduleRef)
    val newFutures = subgraphs(target.id).foldLeft(futures) { (futures, dependencyTarget) =>
      io.println(str"Scheduling ${dependencyTarget}")
      if (futures.contains(dependencyTarget)) futures else compile(io, dependencyTarget.ref, multiplexer, futures, layout)
    }

    val dependencyFutures = Future.sequence(subgraphs(target.id).map(newFutures))
    val future =
      dependencyFutures.flatMap { inputs =>
        if (inputs.exists(!_.isSuccessful)) {
          multiplexer(target.ref) = SkipCompile(target.ref)
          multiplexer.close(target.ref)
          Future.successful(CompileFailure)
        } else {
          val noCompilation = target.sourcePaths.isEmpty
          if (noCompilation) deepDependencies(target.id).foreach { targetId =>
            multiplexer(targetId.ref) = NoCompile(targetId.ref)
          }
          compileModule(io, target, layout, target.kind == Application, multiplexer).map {
            case CompileSuccess(classDirectories) if target.kind == Benchmarks =>
              classDirectories.foreach { classDirectory =>
                Jmh.instrument(
                  classDirectory,
                  layout.benchmarksDir(target.id),
                  layout.resourcesDir(target.id))
                multiplexer(target.ref) = StartStreaming
                val javaSources =
                  layout.benchmarksDir(target.id).findChildren(_.endsWith(".java"))
                layout.shell.javac(
                  classpath(target.ref, layout).to[List].map(_.value),
                  classDirectory.value,
                  javaSources.map(_.value).to[List])
              }
              val out = new StringBuilder()
              val res = layout.shell
                .runJava(
                    jmhRuntimeClasspath(io, target.ref, layout).to[List].map(_.value),
                    if (target.kind == Benchmarks) "org.openjdk.jmh.Main"
                    else target.main.getOrElse(""),
                    securePolicy = target.kind == Application,
                    layout
                ) { ln =>
                  if (target.kind == Benchmarks) multiplexer(target.ref) = Print(ln)
                  else {
                    out.append(ln)
                    out.append("\n")
                  }
                }.await() == 0
              multiplexer(target.ref) = DiagnosticMsg(target.ref, OtherMessage(out.mkString))
              deepDependencies(target.id).foreach { targetId =>
                multiplexer(targetId.ref) = NoCompile(targetId.ref)
              }
              multiplexer(target.ref) = StopCompile(target.ref, res)
              multiplexer.close(target.ref)
              multiplexer(target.ref) = StopStreaming

              if(res) CompileSuccess(classDirectories) else CompileFailure
            case compileResult => compileResult
          }
        }
      }
    newFutures.updated(target.id, future)
  }
}

case class Entity(project: Project, schema: Schema, path: Path)

/** A Universe represents a the fully-resolved set of projects available in the layer */
case class Universe(entities: Map[ProjectId, Entity] = Map()) {
  //projects: Map[ProjectId, Project] = Map(),
  //schemas: Map[ProjectId, Schema] = Map(),
  //dirs: Map[ProjectId, Path] = Map()) {
  def ids: Set[ProjectId] = entities.keySet

  def entity(id: ProjectId): Try[Entity] =
    entities.get(id).ascribe(ItemNotFound(id))

  def makeTarget(io: Io, ref: ModuleRef, layout: Layout): Try[Target] =
    for {
      entity <- entity(ref.projectId)
      module <- entity.project(ref.moduleId)
      compiler <- if (module.compiler == ModuleRef.JavaRef) Success(None)
                 else makeTarget(io, module.compiler, layout).map(Some(_))
      binaries <- ~Await.result(
                     module.allBinaries.map(_.paths(io)).sequence.map(_.flatten),
                     duration.Duration.Inf)
      checkouts <- checkout(ref, layout)
    } yield
      Target(
          ref,
          entity.schema.id,
          module.kind,
          module.main,
          module.plugin,
          entity.schema.repos.map(_.repo).to[List],
          checkouts.to[List],
          binaries.to[List],
          module.after.map(TargetId(entity.schema.id, _)).to[List],
          compiler,
          module.bloopSpec,
          module.params.to[List],
          ref.intransitive,
          module.localSources.map(_ in entity.path).to[List] ++ module.sharedSources
            .map(_.path in layout.sharedDir)
            .to[List] ++ checkouts.flatMap { c =>
            c.local match {
              case Some(p) => c.sources.map(_ in p)
              case None    => c.sources.map(_ in c.path(layout))
            }
          }
      )

  def checkout(ref: ModuleRef, layout: Layout): Try[Set[Checkout]] =
    for {
      entity <- entity(ref.projectId)
      module <- entity.project(ref.moduleId)
      repos <- module.externalSources
                .groupBy(_.repoId)
                .map { case (k, v) => entity.schema.repo(k, layout).map(_ -> v) }
                .sequence
    } yield
      repos.map {
        case (repo, paths) =>
          Checkout(
              repo.id,
              repo.repo,
              repo.local,
              repo.commit,
              repo.track,
              paths.map(_.path).to[List])
      }.to[Set]

  def ++(that: Universe): Universe =
    Universe(entities ++ that.entities)

  private[fury] def dependencies(io: Io, ref: ModuleRef, layout: Layout): Try[Set[ModuleRef]] =
    for {
      entity <- entity(ref.projectId)
      module <- entity.project(ref.moduleId)
      deps    = module.after ++ module.compilerDependencies
      tDeps  <- deps.map(dependencies(io, _, layout)).sequence
    } yield deps ++ tDeps.flatten

  def clean(ref: ModuleRef, layout: Layout): Unit =
    layout.classesDir.delete().unit

  def getMod(ref: ModuleRef): Try[Module] =
    for {
      entity <- entity(ref.projectId)
      module <- entity.project(ref.moduleId)
    } yield module

  def compilation(io: Io, ref: ModuleRef, layout: Layout): Try[Compilation] =
    for {
      target <- makeTarget(io, ref, layout)
      entity <- entity(ref.projectId)
      graph  <- dependencies(io, ref, layout).map(_.map(makeTarget(io, _, layout)).map { a =>
                  a.map { dependencyTarget =>
                    (dependencyTarget.id, dependencyTarget.dependencies ++ dependencyTarget.compiler.map(_.id))
                  }
                }.sequence.map(_.toMap.updated(target.id, target.dependencies ++ target.compiler.map(_.id)))).flatten
      targets   <- graph.keys.map { targetId => makeTarget(io, targetId.ref, layout).map(targetId.ref -> _) }.sequence.map(_.toMap)
      appModules = targets.filter(_._2.executed)
      subgraphs = DirectedGraph(graph.mapValues(_.to[Set])).subgraph(appModules.map(_._2.id).to[Set] + TargetId(entity.schema.id, ref)).connections.mapValues(_.to[List])
      checkouts <- graph.keys.map { targetId => checkout(targetId.ref, layout) }.sequence
    } yield
      Compilation(
          graph,
          subgraphs,
          checkouts.foldLeft(Set[Checkout]())(_ ++ _),
          targets ++ (target.compiler.map { compilerTarget =>
            compilerTarget.ref -> compilerTarget
          }),
          this)
}

case class Hierarchy(schema: Schema, dir: Path, inherited: Set[Hierarchy]) {

  lazy val universe: Try[Universe] = {
    val localProjectIds = schema.projects.map(_.id)
    def merge(universe: Try[Universe], hierarchy: Hierarchy) = for {
      projects <- universe
      nextProjects <- hierarchy.universe
      potentialConflictIds = (projects.ids -- localProjectIds).intersect(nextProjects.ids)
      conflictIds = potentialConflictIds.filter { id =>
        projects.entity(id).map(_.project) != nextProjects.entity(id).map(_.project)
      }
      allProjects <- conflictIds match {
        case x if x.isEmpty => Success(projects ++ nextProjects)
        case _ =>
          Failure(ProjectConflict(conflictIds, h1 = this, h2 = hierarchy))
      }
    } yield allProjects

    val empty: Try[Universe] = Success(Universe())
    for{
      allInherited <- inherited.foldLeft(empty)(merge)
    } yield {
      val schemaEntities = schema.projects.map { project =>
        project.id -> Entity(project, schema, dir)
      }
      allInherited ++ Universe(schemaEntities.toMap)
    }

  }
}

object Schema {
  implicit val msgShow: MsgShow[Schema]       = v => UserMsg(_.schema(v.id.key))
  implicit val stringShow: StringShow[Schema] = _.id.key
  implicit def diff: Diff[Schema]             = Diff.gen[Schema]
}

case class Schema(
    id: SchemaId,
    projects: SortedSet[Project] = TreeSet(),
    repos: SortedSet[SourceRepo] = TreeSet(),
    imports: SortedSet[SchemaRef] = TreeSet(),
    main: Option[ProjectId] = None) {

  def apply(id: ProjectId) = projects.findBy(id)

  def repo(repoId: RepoId, layout: Layout): Try[SourceRepo] =
    repos.findBy(repoId)

  def moduleRefs: SortedSet[ModuleRef] = projects.flatMap(_.moduleRefs)

  def compilerRefs(io: Io, layout: Layout): List[ModuleRef] =
    allProjects(io, layout).toOption.to[List].flatMap(_.flatMap(_.compilerRefs))

  def mainProject: Try[Option[Project]] =
    main.map(projects.findBy(_)).to[List].sequence.map(_.headOption)

  def importCandidates(io: Io, layout: Layout): List[String] =
    repos.to[List].flatMap(_.importCandidates(io, this, layout).toOption.to[List].flatten)

  def hierarchy(io: Io, dir: Path, layout: Layout): Try[Hierarchy] =
    for {
      imps <- imports.map { ref =>
               for {
                 repo         <- repos.findBy(ref.repo)
                 repoDir      <- repo.fullCheckout.get(io, layout)
                 nestedLayout <- ~Layout(layout.home, repoDir, layout.env, repoDir)
                 layer        <- Layer.read(io, nestedLayout.furyConfig, nestedLayout)
                 resolved     <- layer.schemas.findBy(ref.schema)
                 tree         <- resolved.hierarchy(io, repoDir, layout)
               } yield tree
             }.sequence
    } yield Hierarchy(this, dir, imps)

  def importedSchemas(io: Io, layout: Layout): Try[List[Schema]] =
    imports.to[List].map(_.resolve(io, this, layout)).sequence

  def sourceRepoIds: SortedSet[RepoId] = repos.map(_.id)

  def allProjects(io: Io, layout: Layout): Try[List[Project]] =
    importedSchemas(io, layout)
      .flatMap(_.map(_.allProjects(io, layout)).sequence.map(_.flatten))
      .map(_ ++ projects.to[List])

  def unused(projectId: ProjectId): Try[ProjectId] = projects.find(_.id == projectId) match {
    case None    => Success(projectId)
    case Some(m) => Failure(ProjectAlreadyExists(m.id))
  }

  def duplicate(id: String) = copy(id = SchemaId(id))
}

object AliasCmd {
  implicit val msgShow: MsgShow[AliasCmd]       = v => UserMsg(_.module(v.key))
  implicit val stringShow: StringShow[AliasCmd] = _.key
}

case class AliasCmd(key: String)

object Alias {
  implicit val msgShow: MsgShow[Alias]       = v => UserMsg(_.module(v.cmd.key))
  implicit val stringShow: StringShow[Alias] = _.cmd.key
}

case class Alias(cmd: AliasCmd, description: String, schema: Option[SchemaId], module: ModuleRef)

case class Layer(
    version: Int = Layer.CurrentVersion,
    schemas: SortedSet[Schema] = TreeSet(Schema(SchemaId.default)),
    main: SchemaId = SchemaId.default,
    aliases: SortedSet[Alias] = TreeSet()) { layer =>

  def mainSchema: Try[Schema] = schemas.findBy(main)

  def showSchema: Boolean = schemas.size > 1

  def apply(schemaId: SchemaId): Try[Schema] =
    schemas.find(_.id == schemaId).ascribe(ItemNotFound(schemaId))

  def projects: Try[SortedSet[Project]] = mainSchema.map(_.projects)
}

object Layer {
  val CurrentVersion = 3

  def read(io: Io, string: String, layout: Layout): Try[Layer] =
    Success(Ogdl.read[Layer](string, upgrade(io, layout, _)))

  def read(io: Io, file: Path, layout: Layout): Try[Layer] =
    Success(Ogdl.read[Layer](file, upgrade(io, layout, _)).toOption.getOrElse(Layer()))

  def save(io: Io, layer: Layer, layout: Layout): Try[Unit] =
    for {
      layerRepo <- LayerRepository(layout).update(io, layer, layout)
      _         <- Bsp.createConfig(layout)
    } yield ()

  private def upgrade(io: Io, layout: Layout, ogdl: Ogdl): Ogdl =
    Try(ogdl.version().toInt).getOrElse(1) match {
      case 1 =>
        io.println("Migrating layer.fury from file format v1")
        upgrade(
            io,
            layout,
            ogdl.set(
                schemas = ogdl.schemas.map { schema =>
                  schema.set(
                      repos = schema.repos.map { repo =>
                        io.println(s"Checking commit hash for ${repo.repo()}")
                        val commit =
                          layout.shell.git.lsRemoteRefSpec(repo.repo(), repo.refSpec()).toOption.get
                        repo.set(commit = Ogdl(Commit(commit)), track = repo.refSpec)
                      }
                  )
                },
                version = Ogdl(2)
            )
        )
      case 2 =>
        io.println("Migrating layer.fury from file format v2")
        upgrade(
            io,
            layout,
            ogdl.set(
                schemas = ogdl.schemas.map { schema =>
                  schema.set(
                      projects = schema.projects.map { project =>
                        project.set(
                            modules = project.modules.map { module =>
                              module.set(kind = Ogdl(module.kind().capitalize))
                            }
                        )
                      }
                  )
                },
                version = Ogdl(3)
            )
        )
      case CurrentVersion => ogdl
    }

}

object ModuleRef {

  implicit val stringShow: StringShow[ModuleRef] = ref => str"${ref.projectId}/${ref.moduleId}"
  implicit val entityName: EntityName[ModuleRef] = EntityName(msg"dependency")

  implicit val msgShow: MsgShow[ModuleRef] =
    ref =>
      UserMsg { theme =>
        msg"${theme.project(ref.projectId.key)}${theme.gray("/")}${theme.module(ref.moduleId.key)}"
          .string(theme)
      }

  val JavaRef = ModuleRef(ProjectId("java"), ModuleId("compiler"), false)

  def parseFull(string: String, intransitive: Boolean): Try[ModuleRef] = string match {
    case r"$projectId@([a-z][a-z0-9\-]*[a-z0-9])\/$moduleId@([a-z][a-z0-9\-]*[a-z0-9])" =>
      Success(ModuleRef(ProjectId(projectId), ModuleId(moduleId), intransitive))
    case _ =>
      Failure(ItemNotFound(ModuleId(string)))
  }

  def parse(project: Project, string: String, intransitive: Boolean): Try[ModuleRef] =
    string match {
      // `dummy1` and `dummy2` are not used, but binding them to the inner capturing groups
      // works around a bug in Kaleidoscope
      case r"$projectId@([a-z]$dummy1@(-?[a-z0-9]+)*)\/$moduleId@([a-z]$dummy2@(-?[a-z0-9]+)*)" =>
        Success(ModuleRef(ProjectId(projectId), ModuleId(moduleId), intransitive))
      case r"[a-z](-?[a-z0-9]+)*" =>
        Success(ModuleRef(project.id, ModuleId(string), intransitive))
      case _ =>
        Failure(ItemNotFound(ModuleId(string)))
    }
  }

case class ModuleRef(
    projectId: ProjectId,
    moduleId: ModuleId,
    intransitive: Boolean = false,
    hidden: Boolean = false) {
  override def equals(that: Any): Boolean = that match {
    case ModuleRef(p, m, _, _) => projectId == p && moduleId == m
    case _                     => false
  }

  def hide = copy(hidden = true)

  override def hashCode: Int = projectId.hashCode + moduleId.hashCode

  override def toString: String = str"$projectId/$moduleId"
}

object SchemaId {
  implicit val msgShow: MsgShow[SchemaId]       = v => UserMsg(_.schema(v.key))
  implicit val stringShow: StringShow[SchemaId] = _.key

  implicit val diff: Diff[SchemaId] =
    (l, r) => Diff.stringDiff.diff(l.key, r.key)

  final val default = SchemaId("default")

  def parse(name: String): Try[SchemaId] = name match {
    case r"[a-z](-?[a-zA-Z0-9]+)*" => Success(SchemaId(name))
    case _                         => Failure(InvalidValue(name))
  }
}

case class SchemaId(key: String) extends Key(msg"schema")

object ProjectId {
  implicit val msgShow: MsgShow[ProjectId]       = p => UserMsg(_.project(p.key))
  implicit val stringShow: StringShow[ProjectId] = _.key
  implicit def diff: Diff[ProjectId]             = (l, r) => Diff.stringDiff.diff(l.key, r.key)

  def parse(name: String): Try[ProjectId] = name match {
    case r"[a-z](-?[a-z0-9]+)*" => Success(ProjectId(name))
    case _                      => Failure(InvalidValue(name))
  }
}

case class ProjectId(key: String) extends Key(msg"project")

object ModuleId {
  implicit val msgShow: MsgShow[ModuleId]       = m => UserMsg(_.module(m.key))
  implicit val stringShow: StringShow[ModuleId] = _.key
  implicit def diff: Diff[ModuleId]             = (l, r) => Diff.stringDiff.diff(l.key, r.key)
  final val Core: ModuleId                      = ModuleId("core")

  def parse(name: String): Try[ModuleId] = name match {
    case r"[a-z](-?[a-z0-9]+)*" => Success(ModuleId(name))
    case _                      => Failure(InvalidValue(name))
  }
}

case class ModuleId(key: String) extends Key(msg"module")

object Repo {
  implicit val msgShow: MsgShow[Repo]       = r => UserMsg(_.url(r.simplified))
  implicit val stringShow: StringShow[Repo] = _.simplified

  case class ExistingLocalFileAsAbspath(absPath: String)

  object ExistingLocalFileAsAbspath {

    def unapply(path: String): Option[String] = Path(path).absolutePath().toOption match {
      case Some(absPath) => absPath.ifExists().map(_.value)
      case None          => None
    }
  }

  def fromString(str: String): Repo = str match {
    case "." => Repo("")
    case r"gh:$group@([A-Za-z0-9_\-\.]+)/$project@([A-Za-z0-9\._\-]+)" =>
      Repo(str"git@github.com:$group/$project.git")
    case r"gl:$group@([A-Za-z0-9_\-\.]+)/$project@([A-Za-z0-9\._\-]+)" =>
      Repo(str"git@gitlab.com:$group/$project.git")
    case r"bb:$group@([A-Za-z0-9_\-\.]+)/$project@([A-Za-z0-9\._\-]+)" =>
      Repo(str"git@bitbucket.com:$group/$project.git")
    case ExistingLocalFileAsAbspath(abspath) => Repo(abspath)
    case other                               => Repo(other)
  }
}

case class Checkout(
    repoId: RepoId,
    repo: Repo,
    local: Option[Path],
    commit: Commit,
    refSpec: RefSpec,
    sources: List[Path]) {

  def hash: Digest               = this.digest[Md5]
  def path(layout: Layout): Path = layout.srcsDir / hash.encoded

  def get(io: Io, layout: Layout): Try[Path] =
    for {
      repoDir    <- repo.fetch(io, layout)
      workingDir <- checkout(io, layout)
    } yield workingDir

  private def checkout(io: Io, layout: Layout): Try[Path] =
    local.map(Success(_)).getOrElse {
      if (!(path(layout) / ".done").exists) {

        if (path(layout).exists()) {
          io.println(s"Found incomplete checkout of ${if (sources.isEmpty) "all sources"
          else sources.map(_.value).mkString("[", ", ", "]")}.")
          path(layout).delete()
        }

        io.println(msg"Checking out ${if (sources.isEmpty) msg"all sources from repository ${repoId}"
        else sources.map(_.value).mkString("[", ", ", "]")}.")
        path(layout).mkdir()
        layout.shell.git
          .sparseCheckout(
              repo.path(layout),
              path(layout),
              sources,
              refSpec = refSpec.id,
              commit = commit.id)
          .map { _ =>
            path(layout)
          }
      } else Success(path(layout))
    }
}

object SourceRepo {
  implicit val msgShow: MsgShow[SourceRepo]       = r => UserMsg(_.repo(r.id.key))
  implicit val stringShow: StringShow[SourceRepo] = _.id.key
  implicit def diff: Diff[SourceRepo]             = Diff.gen[SourceRepo]
}

case class SourceRepo(id: RepoId, repo: Repo, track: RefSpec, commit: Commit, local: Option[Path]) { // TODO: change Option[String] to RefSpec

  def listFiles(io: Io, layout: Layout): Try[List[Path]] =
    for {
      dir <- local.map(Success(_)).getOrElse(repo.fetch(io, layout))
      commit <- ~layout.shell.git
                 .getTag(dir, track.id)
                 .toOption
                 .orElse(layout.shell.git.getBranchHead(dir, track.id).toOption)
                 .getOrElse(track.id)
      files <- local.map { _ =>
                Success(dir.children.map(Path(_)))
              }.getOrElse(layout.shell.git.lsTree(dir, commit))
    } yield files

  def fullCheckout: Checkout = Checkout(id, repo, local, commit, track, List())

  def importCandidates(io: Io, schema: Schema, layout: Layout): Try[List[String]] =
    for {
      repoDir     <- repo.fetch(io, layout)
      layerString <- layout.shell.git.showFile(repoDir, "layer.fury")
      layer       <- Layer.read(io, layerString, layout)
      schemas     <- ~layer.schemas.to[List]
    } yield
      schemas.map { schema =>
        str"${id.key}:${schema.id.key}"
      }

  def current(io: Io, layout: Layout): Try[RefSpec] =
    for {
      dir    <- local.map(Success(_)).getOrElse(repo.fetch(io, layout))
      commit <- layout.shell.git.getCommit(dir)
    } yield RefSpec(commit)

  def sourceCandidates(io: Io, layout: Layout)(pred: String => Boolean): Try[Set[Source]] =
    listFiles(io, layout).map { files =>
      files.filter { f =>
        pred(f.filename)
      }.map { p =>
        ExternalSource(id, p.parent): Source
      }.to[Set]
    }
}

case class BinRepoId(id: String)

object BinRepoId {
  implicit val msgShow: MsgShow[BinRepoId]       = v => UserMsg(_.repo(v.id))
  implicit val stringShow: StringShow[BinRepoId] = _.id
  final val Central: BinRepoId                   = BinRepoId("central")
}

case class Repo(url: String) {
  def hash: Digest               = url.digest[Md5]
  def path(layout: Layout): Path = layout.reposDir / hash.encoded

  def update(layout: Layout): Try[UserMsg] =
    for {
      oldCommit <- layout.shell.git.getCommit(path(layout))
      _         <- layout.shell.git.fetch(path(layout), None)
      newCommit <- layout.shell.git.getCommit(path(layout))
      msg <- ~(if (oldCommit != newCommit) msg"Repository ${url} updated to new commit $newCommit"
               else msg"Repository ${url} is unchanged")
    } yield msg

  def getCommitFromTag(layout: Layout, tag: RefSpec): Try[String] =
    for {
      commit <- layout.shell.git.getCommitFromTag(path(layout), tag.id)
    } yield commit

  def fetch(io: Io, layout: Layout): Try[Path] =
    if (!(path(layout) / ".done").exists) {
      if (path(layout).exists()) {
        io.println(s"Found incomplete clone of $url.")
        path(layout).delete()
      }

      io.println(s"Cloning Git repository $url.")
      path(layout).mkdir()
      layout.shell.git.cloneBare(url, path(layout)).map { _ =>
        path(layout)
      }
    } else Success(path(layout))

  def simplified: String = url match {
    case r"git@github.com:$group@(.*)/$project@(.*)\.git"    => str"gh:$group/$project"
    case r"git@bitbucket.com:$group@(.*)/$project@(.*)\.git" => str"bb:$group/$project"
    case r"git@gitlab.com:$group@(.*)/$project@(.*)\.git"    => str"gl:$group/$project"
    case other                                               => other
  }

  def projectName: Try[RepoId] = url match {
    case r".*/$project@([^\/]*).git" => Success(RepoId(project))
    case value                       => Failure(InvalidValue(value))
  }
}

object SchemaRef {

  implicit val msgShow: MsgShow[SchemaRef] =
    v =>
      UserMsg { theme =>
        msg"${v.repo}${theme.gray(":")}${v.schema}".string(theme)
      }

  implicit val stringShow: StringShow[SchemaRef] = sr => str"${sr.repo}:${sr.schema}"
  implicit def diff: Diff[SchemaRef]             = Diff.gen[SchemaRef]

  def unapply(value: String): Option[SchemaRef] = value match {
    case r"$repo@([a-z0-9\.\-]*[a-z0-9]):$schema@([a-zA-Z0-9\-\.]*[a-zA-Z0-9])$$" =>
      Some(SchemaRef(RepoId(repo), SchemaId(schema)))
    case _ =>
      None
  }
}

case class SchemaRef(repo: RepoId, schema: SchemaId) {

  def resolve(io: Io, base: Schema, layout: Layout): Try[Schema] =
    for {
      repo     <- base.repos.findBy(repo)
      dir      <- repo.fullCheckout.get(io, layout)
      layer    <- Layer.read(io, Layout(layout.home, dir, layout.env, dir).furyConfig, layout)
      resolved <- layer.schemas.findBy(schema)
    } yield resolved
}

sealed trait CompileEvent
case object Tick                                                 extends CompileEvent
case class StartCompile(ref: ModuleRef)                          extends CompileEvent
case class CompilationProgress(ref: ModuleRef, progress: Double) extends CompileEvent
case class StopCompile(ref: ModuleRef, success: Boolean)         extends CompileEvent
case class NoCompile(ref: ModuleRef)                             extends CompileEvent
case class SkipCompile(ref: ModuleRef)                           extends CompileEvent
case class Print(line: String)                                   extends CompileEvent
object StartStreaming                                            extends CompileEvent
object StopStreaming                                             extends CompileEvent
case class DiagnosticMsg(ref: ModuleRef, msg: DiagnosticMessage) extends CompileEvent

sealed trait CompileResult {
  def isSuccessful: Boolean
  def asTry: Try[CompileSuccess]
}

case class CompileSuccess(outputDirectories: Set[Path]) extends CompileResult{
  override def isSuccessful: Boolean = true
  override def asTry: Try[CompileSuccess] = Success(this)
}

case object CompileFailure extends CompileResult{
  override def isSuccessful: Boolean = false
  override def asTry: Try[CompileSuccess] = Failure(CompilationFailure())
}

object TargetId {
  implicit val stringShow: StringShow[TargetId] = _.key
  
  def apply(schemaId: SchemaId, projectId: ProjectId, moduleId: ModuleId): TargetId =
    TargetId(str"${schemaId}_${projectId}_${moduleId}")
  
    def apply(schemaId: SchemaId, ref: ModuleRef): TargetId =
      TargetId(schemaId, ref.projectId, ref.moduleId)
}

case class TargetId(key: String) extends AnyVal {
  def moduleId: ModuleId = ModuleId(key.split("_")(2))
  def projectId: ProjectId = ProjectId(key.split("_")(1))
  def schemaId: SchemaId = SchemaId(key.split("_")(0))
  def ref: ModuleRef = ModuleRef(projectId, moduleId)
}

case class Target(
    ref: ModuleRef,
    schemaId: SchemaId,
    kind: Kind,
    main: Option[String],
    plugin: Option[String],
    repos: List[Repo],
    checkouts: List[Checkout],
    binaries: List[Path],
    dependencies: List[TargetId],
    compiler: Option[Target],
    bloopSpec: Option[BloopSpec],
    params: List[Parameter],
    intransitive: Boolean,
    sourcePaths: List[Path]) {

  def id: TargetId = TargetId(schemaId, ref.projectId, ref.moduleId)

  def executed = kind == Application || kind == Benchmarks
}

object Project {
  implicit val msgShow: MsgShow[Project]       = v => UserMsg(_.project(v.id.key))
  implicit val stringShow: StringShow[Project] = _.id.key
  implicit def diff: Diff[Project]             = Diff.gen[Project]

  def available(projectId: ProjectId, layer: Layer): Boolean =
    !layer.projects.toOption.to[List].flatten.findBy(projectId).isSuccess
}

case class Project(
    id: ProjectId,
    modules: SortedSet[Module] = TreeSet(),
    main: Option[ModuleId] = None,
    license: LicenseId = License.unknown,
    description: String = "",
    compiler: Option[ModuleRef] = None) {
  def moduleRefs: List[ModuleRef] = modules.to[List].map(_.ref(this))

  def mainModule: Try[Option[Module]] =
    main.map(modules.findBy(_)).to[List].sequence.map(_.headOption)

  def compilerRefs: List[ModuleRef] =
    modules.to[List].collect {
      case m @ Module(_, Compiler, _, _, _, _, _, _, _, _, _, _) => m.ref(this)
    }

  def unused(moduleId: ModuleId) =
    modules.findBy(moduleId) match {
      case Success(_) =>
        Failure(ModuleAlreadyExists(moduleId))
      case _ =>
        Success(moduleId)
    }

  def apply(module: ModuleId): Try[Module] = modules.findBy(module)
}

object License {
  implicit val msgShow: MsgShow[License]       = v => UserMsg(_.license(v.id.key))
  implicit val stringShow: StringShow[License] = _.id.key

  val unknown = LicenseId("unknown")

  val standardLicenses = List(
      License(LicenseId("afl-3.0"), "Academic Free License v3.0"),
      License(LicenseId("apache-2.0"), "Apache license 2.0"),
      License(LicenseId("artistic-2.0"), "Artistic license 2.0"),
      License(LicenseId("bsd-2-clause"), "BSD 2-clause \"Simplified\" license"),
      License(LicenseId("bsd-3-clause"), "BSD 3-clause \"New\" or \"Revised\" license"),
      License(LicenseId("bsl-1.0"), "Boost Software License 1.0"),
      License(LicenseId("bsd-3-clause-clear"), "BSD 3-clause Clear license"),
      License(LicenseId("cc"), "Creative Commons license family"),
      License(LicenseId("cc0-1.0"), "Creative Commons Zero v1.0 Universal"),
      License(LicenseId("cc-by-4.0"), "Creative Commons Attribution 4.0"),
      License(LicenseId("cc-by-sa-4.0"), "Creative Commons Attribution Share Alike 4.0"),
      License(LicenseId("wtfpl"), "Do What The F*ck You Want To Public License"),
      License(LicenseId("ecl-2.0"), "Educational Community License v2.0"),
      License(LicenseId("epl-1.0"), "Eclipse Public License 1.0"),
      License(LicenseId("epl-1.1"), "European Union Public License 1.1"),
      License(LicenseId("agpl-3.0"), "GNU Affero General Public License v3.0"),
      License(LicenseId("gpl"), "GNU General Public License family"),
      License(LicenseId("gpl-2.0"), "GNU General Public License v2.0"),
      License(LicenseId("gpl-3.0"), "GNU General Public License v3.0"),
      License(LicenseId("lgpl"), "GNU Lesser General Public License family"),
      License(LicenseId("lgpl-2.1"), "GNU Lesser General Public License v2.1"),
      License(LicenseId("lgpl-3.0"), "GNU Lesser General Public License v3.0"),
      License(LicenseId("isc"), "ISC"),
      License(LicenseId("lppl-1.3c"), "LaTeX Project Public License v1.3c"),
      License(LicenseId("ms-pl"), "Microsoft Public License"),
      License(LicenseId("mit"), "MIT"),
      License(LicenseId("mpl-2.0"), "Mozilla Public License 2.0"),
      License(LicenseId("osl-3.0"), "Open Software License 3.0"),
      License(LicenseId("postgresql"), "PostgreSQL License"),
      License(LicenseId("ofl-1.1"), "SIL Open Font License 1.1"),
      License(LicenseId("ncsa"), "University of Illinois/NCSA Open Source License"),
      License(LicenseId("unlicense"), "The Unlicense"),
      License(LicenseId("zlib"), "zLib License")
  )
}

object LicenseId {
  implicit val msgShow: MsgShow[LicenseId]       = v => UserMsg(_.license(v.key))
  implicit val stringShow: StringShow[LicenseId] = _.key
}
case class LicenseId(key: String) extends Key(msg"license")

case class License(id: LicenseId, name: String)

object RefSpec {
  implicit val msgShow: MsgShow[RefSpec]       = v => UserMsg(_.version(v.id))
  implicit val stringShow: StringShow[RefSpec] = _.id

  val master = RefSpec("master")
}
case class RefSpec(id: String)

object Commit {
  implicit val stringShow: StringShow[Commit] = _.id
  implicit val msgShow: MsgShow[Commit]       = r => UserMsg(_.version(r.id.take(7)))
}
case class Commit(id: String)

object Source {
  implicit val stringShow: StringShow[Source] = _.description

  implicit val msgShow: MsgShow[Source] = v =>
    UserMsg { theme =>
      v match {
        case ExternalSource(repoId, path) =>
          msg"${theme.repo(repoId.key)}${theme.gray(":")}${theme.path(path.value)}".string(theme)
        case SharedSource(path) =>
          msg"${theme.repo("shared")}${theme.gray(":")}${theme.path(path.value)}".string(theme)
        case LocalSource(path) => msg"${theme.path(path.value)}".string(theme)
      }
    }

  def unapply(string: String): Option[Source] = string match {
    case r"shared:$path@(.*)" =>
      Some(SharedSource(Path(path)))
    case r"$repo@([a-z][a-z0-9\.\-]*[a-z0-9]):$path@(.*)" =>
      Some(ExternalSource(RepoId(repo), Path(path)))
    case r"$path@(.*)" => Some(LocalSource(Path(path)))
    case _             => None
  }

  implicit val ogdlReader: OgdlReader[Source] = src => unapply(src()).get // FIXME
  implicit val ogdlWriter: OgdlWriter[Source] = src => Ogdl(src.description)

  def repoId(src: Source): Option[RepoId] = src match {
    case ExternalSource(repoId, _) => Some(repoId)
    case _                         => None
  }
}

trait Source {
  def description: String

  def hash(schema: Schema, layout: Layout): Try[Digest]
  def path: Path
  def repoIdentifier: RepoId
}

case class ExternalSource(repoId: RepoId, path: Path) extends Source {
  def description: String = str"${repoId}:${path.value}"

  def hash(schema: Schema, layout: Layout): Try[Digest] =
    schema.repo(repoId, layout).map((path, _).digest[Md5])

  def repoIdentifier: RepoId = repoId
}

case class SharedSource(path: Path) extends Source {
  def description: String = str"shared:${path.value}"

  def hash(schema: Schema, layout: Layout): Try[Digest] =
    Success((-2, path).digest[Md5])

  def repoIdentifier: RepoId = RepoId("-")
}

case class LocalSource(path: Path) extends Source {
  def description: String = str"${path.value}"

  def hash(schema: Schema, layout: Layout): Try[Digest] =
    Success((-1, path).digest[Md5])

  def repoIdentifier: RepoId = RepoId("-")
}

object RepoId {
  implicit val msgShow: MsgShow[RepoId]       = r => UserMsg(_.repo(r.key))
  implicit val stringShow: StringShow[RepoId] = _.key
}

case class RepoId(key: String) extends Key(msg"repository")

object Parameter {
  implicit val stringShow: StringShow[Parameter] = _.name
  implicit val msgShow: MsgShow[Parameter]       = v => UserMsg(_.param(v.name))
}

case class Parameter(name: String) { def parameter = str"-$name" }

abstract class Key(val kind: UserMsg) { def key: String }
