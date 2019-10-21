/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.6.7. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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

import fury._, io._, strings._, ogdl._, model._, utils._
import Graph.{CompilerDiagnostic, DiagnosticMessage, OtherMessage}
import exoskeleton._
import gastronomy._
import kaleidoscope._
import mercator._

import org.eclipse.lsp4j.jsonrpc.Launcher
import ch.epfl.scala.bsp4j.{CompileResult => BspCompileResult, _}
import com.google.gson.{Gson, JsonElement}

import scala.collection.immutable.{SortedSet, TreeSet}
import scala.collection.mutable.HashMap
import scala.collection.concurrent.TrieMap
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
import java.util.concurrent.{CompletableFuture, Executors, ExecutionException, TimeUnit}

import language.higherKinds
import scala.annotation.tailrec
import scala.util.control.NonFatal

object Binary {
  implicit val msgShow: MsgShow[Binary] = v => UserMsg(_.binary(v.spec))
  implicit val stringShow: StringShow[Binary] = _.spec
  implicit def diff: Diff[Binary] = Diff.gen[Binary]

  def unapply(service: BinRepoId, string: String): Try[Binary] =
    string match {
      case r"$g@([\.\-_a-zA-Z0-9]*)\:$a@([\.\-_a-zA-Z0-9]*)\:$v@([\.\-\+_a-zA-Z0-9]*)" =>
        Success(Binary(service, g, a, v))
      case _ =>
        Failure(InvalidArgValue("binary", string))
    }

  private val compilerVersionCache: HashMap[Binary, Try[String]] = HashMap()

  val Jmh = Binary(BinRepoId.Central, "org.openjdk.jmh", "jmh-core", "1.21")
}

case class Binary(binRepo: BinRepoId, group: String, artifact: String, version: String) {
  def spec = str"$group:$artifact:$version"
  def paths(io: Io): Try[List[Path]] = Coursier.fetch(io, this)
}

object Policy {
  def read(io: Io, installation: Installation): Try[Policy] =
    Success(Ogdl.read[Policy](installation.policyFile,
        upgrade(io, installation, _)).toOption.getOrElse(Policy(SortedSet.empty[Grant])))

  def save(io: Io, installation: Installation, policy: Policy): Try[Unit] =
    installation.policyFile.writeSync(Ogdl.serialize(Ogdl(policy)))

  private def upgrade(io: Io, installation: Installation, ogdl: Ogdl): Ogdl = ogdl
}

case class Policy(policy: SortedSet[Grant] = TreeSet()) {
  def forContext(layout: Layout, projectId: ProjectId/*, layer: Layer*/): Policy =
    Policy(policy.filter {
      case Grant(DirectoryScope(dir), _) => dir == layout.base.value
      case Grant(ProjectScope(id), _)    => projectId == id
      //case Grant(LayerScope(hash), _)    => hash == layer.hash
    })

  def grant(scope: Scope, permissions: List[Permission]): Policy =
    copy(policy = policy ++ permissions.map(Grant(scope, _)))

  def obviate(scope: Scope, permissions: List[Permission]): Policy =
    copy(policy = policy.filterNot(permissions.contains))

  def checkAll(permissions: Iterable[Permission]): Try[Unit] = {
    val missing = permissions.to[Set] -- policy.map(_.permission)
    if(missing.isEmpty) Success(()) else Failure(NoPermissions(missing))
  }

  def save(file: Path): Try[Unit] = file.writeSync {
    val sb = new StringBuilder()
    sb.append("grant {\n")
    policy.foreach { grant =>
      val p = grant.permission
      val actionAddendum = p.action.fold("") { a => s""", "$a"""" }
      sb.append(str""" permission ${p.className} "${p.target}"${actionAddendum};""")
      sb.append('\n')
    }
    sb.append("};\n")
    sb.toString
  }
}

object Module {
  implicit val msgShow: MsgShow[Module]       = v => UserMsg(_.module(v.id.key))
  implicit val stringShow: StringShow[Module] = _.id.key
  implicit val diff: Diff[Module] = Diff.gen[Module]

  def available(id: ModuleId, project: Project): Try[ModuleId] =
    project.modules.find(_.id == id).fold(Try(id)) { module => Failure(ModuleAlreadyExists(module.id)) }
}

case class Module(id: ModuleId,
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
                  bloopSpec: Option[BloopSpec] = None,
                  environment: SortedSet[EnvVar] = TreeSet(),
                  properties: SortedSet[JavaProperty] = TreeSet(),
                  policy: SortedSet[Permission] = TreeSet()) {

  def allBinaries: SortedSet[Binary] = if(kind == Benchmarks) binaries + Binary.Jmh else binaries
  def compilerDependencies: Set[ModuleRef] = Set(compiler).filter(_ != ModuleRef.JavaRef).map(_.hide)
  def ref(project: Project): ModuleRef = ModuleRef(project.id, id)
  def externalSources: SortedSet[ExternalSource] = sources.collect { case src: ExternalSource => src }
  def sharedSources: SortedSet[SharedSource] = sources.collect { case src: SharedSource => src }
  def localSources: SortedSet[Path] = sources.collect { case src: LocalSource => src.path }

  def permission(hashPrefix: PermissionHash): Option[Permission] = {
    val allMatches = policy.filter(_.hash.startsWith(hashPrefix.key))
    if (allMatches.size == 1) Some(allMatches.head) else None
  }

  def policyEntries: Set[PermissionEntry] = {
    val prefixLength = Compare.uniquePrefixLength(policy.map(_.hash)).max(3)
    policy.map { p => PermissionEntry(p, PermissionHash(p.hash.take(prefixLength))) }
  }
}

trait FuryBspServer extends BuildServer with ScalaBuildServer

class BspConnection(val future: java.util.concurrent.Future[Void],
                         client: FuryBuildClient,
                         server: FuryBspServer,
                         traceBuffer: CharArrayWriter,
                         messageBuffer: CharArrayWriter) {

  //TODO wrap into a trait
  var lastUsed: Long = System.currentTimeMillis
  var lastUser: String = "none"

  def shutdown(): Unit = {
    writeTrace(client.layout)
    writeMessages(client.layout)
    client.multiplexer.foreach(_(client.targetId.ref) = Print(client.targetId.ref, s"Shutting down connection, last used: ${System.currentTimeMillis() - lastUsed} ms ago by ${lastUser}"))
    try {
      server.buildShutdown().get(5, TimeUnit.SECONDS)
      server.onBuildExit()
    } catch {
      case NonFatal(e) =>
        client.multiplexer.foreach(_(client.targetId.ref) = Print(client.targetId.ref, s"Connection broke: ${e.getMessage}, last used: ${System.currentTimeMillis() - lastUsed} ms ago by ${lastUser}"))
    }
    client.onBeingUsed = { user: String => throw new Exception("Attempt to use a connection that has been shut down")}
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
    client.onBeingUsed = { user: String =>
      val curr = System.currentTimeMillis
      //currentMultiplexer.foreach(_(targetId.ref) = Print(targetId.ref, s"Last used: ${curr - lastUsed} ms ago by ${lastUser}"))
      lastUsed = curr
      lastUser = user
    }
    action(server)
  }

  def writeTrace(layout: Layout): Try[Unit] = for {
    _ <- layout.traceLogfile.appendSync(traceBuffer.toString)
  } yield traceBuffer.reset()

  def writeMessages(layout: Layout): Try[Unit] = for {
    _ <- layout.messagesLogfile.appendSync(messageBuffer.toString)
  } yield messageBuffer.reset()

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
      ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor(Threads.factory("handle-handler", daemon = true)), throw _)

    def handle(handle: Handle, sink: PrintWriter): Unit = handles(handle) = sink

    Future {
      while(true) {
        handles.foreach {
          case (handle, sink) =>
            if (handle.broken.isCompleted) {
              handles -= handle
            } else {
              try {
                val line = handle.errReader.readLine()
                if (line != null) sink.println(line)
              } catch {
                case e: IOException =>
                  sink.println("Broken handle!")
                  e.printStackTrace(sink)
                  handle.broken.failure(e)
              }
            }
        }
        Thread.sleep(100)
      }
    } (ec)
  }

  import bloop.launcher.LauncherMain
  import bloop.launcher.LauncherStatus._

  private val bloopVersion = "1.3.4"

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
  private val compilationThreadPool = Executors.newCachedThreadPool(Threads.factory("bsp-launcher", daemon = true))

  //FIXME
  var receiverClient: Option[BuildClient] = None

  val bspPool: Pool[Path, BspConnection] = new Pool[Path, BspConnection](10000L) {

    def destroy(value: BspConnection): Unit = value.shutdown()
    def isBad(value: BspConnection): Boolean = value.future.isDone

    def isIdle(value: BspConnection): Boolean = {
      value.lastUsed + timeout < System.currentTimeMillis()
    }

    private val pec: ExecutionContext =
      ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor(Threads.factory("pool-cleaner", daemon = true)), throw _)

    Future {
      while (true) {
        pool.keySet.foreach { key =>
          pool(key).foreach{ conn =>
            if(isIdle(conn)) {
              destroy(conn)
              pool -= key
            }
          }
        }
        Thread.sleep(100)
      }
    }(pec)

    def create(dir: Path): BspConnection = {
      val bspMessageBuffer = new CharArrayWriter()
      val bspTraceBuffer = new CharArrayWriter()
      val log = new java.io.PrintWriter(bspTraceBuffer, true)
      val handle = BspConnectionManager.bloopLauncher
      BspConnectionManager.HandleHandler.handle(handle, log)
      val client = receiverClient.fold[FuryBuildClient](
        new DisplayingClient(messageSink = new PrintWriter(bspMessageBuffer))
      ){
        rec => new ForwardingClient(rec)
      }
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
      val bspConn = new BspConnection(future, client, server, bspTraceBuffer, bspMessageBuffer)

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

  def mkCompilation(io: Io,
                    schema: Schema,
                    ref: ModuleRef,
                    layout: Layout,
                    installation: Installation,
                    https: Boolean)
                   : Try[Compilation] = for {

    hierarchy   <- schema.hierarchy(io, layout.base, layout, https)
    universe    <- hierarchy.universe
    policy      <- Policy.read(io, installation)
    compilation <- universe.compilation(io, ref, policy, layout)
    _           <- compilation.generateFiles(io, layout)

    _           <- compilation.bspUpdate(io, compilation.targets(ref).id, layout)

  } yield compilation

  def asyncCompilation(io: Io,
                       schema: Schema,
                       ref: ModuleRef,
                       layout: Layout,
                       installation: Installation,
                       https: Boolean)
                      : Future[Try[Compilation]] = {

    def fn: Future[Try[Compilation]] = Future(mkCompilation(io, schema, ref, layout, installation, https))

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
                      installation: Installation,
                      https: Boolean): Try[Compilation] = {
    val compilation = mkCompilation(io, schema, ref, layout, installation, https)
    compilationCache(layout.furyDir) = Future.successful(compilation)
    compilation
  }
}

sealed abstract class FuryBuildClient extends BuildClient {
  var compilation: Compilation = _
  var targetId: TargetId = _
  var layout: Layout = _
  var onBeingUsed: String => Unit = _
  //TODO move to DisplayingClient
  var multiplexer: Option[Multiplexer[ModuleRef, CompileEvent]] = None
}

class DisplayingClient(messageSink: PrintWriter) extends FuryBuildClient {

  override def onBuildShowMessage(params: ShowMessageParams): Unit = {
    onBeingUsed("onBuildShowMessage")
    multiplexer.foreach(_(targetId.ref) = Print(targetId.ref, params.getMessage))
    messageSink.println(s"${LocalDateTime.now} showMessage: ${params.getMessage}")
  }

  override def onBuildLogMessage(params: LogMessageParams): Unit = {
    onBeingUsed("onBuildLogMessage")
    multiplexer.foreach(_(targetId.ref) = Print(targetId.ref, params.getMessage))
    messageSink.println(s"${LocalDateTime.now}  logMessage: ${params.getMessage}")
  }

  override def onBuildPublishDiagnostics(params: PublishDiagnosticsParams): Unit = {
    onBeingUsed("onBuildPublishDiagnostics")
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

  override def onBuildTargetDidChange(params: DidChangeBuildTarget): Unit = {
    onBeingUsed("onBuildTargetDidChange")
  }

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
    onBeingUsed("onBuildTaskProgress")
    val report   = convertDataTo[CompileTask](params.getData)
    val targetId = getTargetId(report.getTarget.getUri)
    multiplexer.foreach {
      _(targetId.ref) = CompilationProgress(targetId.ref, params.getProgress.toDouble / params.getTotal)
    }
  }

  override def onBuildTaskStart(params: TaskStartParams): Unit = {
    onBeingUsed("onBuildTaskStart")
    val report   = convertDataTo[CompileTask](params.getData)
    val targetId: TargetId = getTargetId(report.getTarget.getUri)
    multiplexer.foreach { mp => mp(targetId.ref) = StartCompile(targetId.ref) }
    compilation.deepDependencies(targetId).foreach { dependencyTargetId =>
      multiplexer.foreach(_(dependencyTargetId.ref) = NoCompile(dependencyTargetId.ref))
    }
  }

  override def onBuildTaskFinish(params: TaskFinishParams): Unit = {
    onBeingUsed("onBuildTaskFinish")
    params.getDataKind match {
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
}

class ForwardingClient(receiver: BuildClient) extends FuryBuildClient {

  //TODO check if messages have to be transformed, e. g. the target IDs
  override def onBuildShowMessage(showMessageParams: ShowMessageParams): Unit = {
    onBeingUsed("forwarding client")
    receiver.onBuildShowMessage(showMessageParams)
  }

  override def onBuildLogMessage(logMessageParams: LogMessageParams): Unit = {
    onBeingUsed("forwarding client")
    receiver.onBuildLogMessage(logMessageParams)
  }

  override def onBuildTaskStart(taskStartParams: TaskStartParams): Unit = {
    onBeingUsed("forwarding client")
    receiver.onBuildTaskStart(taskStartParams)
  }

  override def onBuildTaskProgress(taskProgressParams: TaskProgressParams): Unit = {
    onBeingUsed("forwarding client")
    receiver.onBuildTaskProgress(taskProgressParams)
  }

  override def onBuildTaskFinish(taskFinishParams: TaskFinishParams): Unit = {
    onBeingUsed("forwarding client")
    receiver.onBuildTaskFinish(taskFinishParams)
  }

  override def onBuildPublishDiagnostics(publishDiagnosticsParams: PublishDiagnosticsParams): Unit = {
    onBeingUsed("forwarding client")
    receiver.onBuildPublishDiagnostics(publishDiagnosticsParams)
  }

  override def onBuildTargetDidChange(didChangeBuildTarget: DidChangeBuildTarget): Unit = {
    onBeingUsed("forwarding client")
    receiver.onBuildTargetDidChange(didChangeBuildTarget)
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

  def saveNative(io: Io, ref: ModuleRef, dest: Path, layout: Layout, main: String): Try[Unit] =
    for {
      dest <- dest.directory
      cp   = runtimeClasspath(ref, layout).to[List].map(_.value)
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

  def allParams(io: Io, ref: ModuleRef, layout: Layout): List[String] = {
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

  def compileModule(io: Io,
                    target: Target,
                    layout: Layout,
                    application: Boolean,
                    multiplexer: Multiplexer[ModuleRef, CompileEvent],
                    pipelining: Boolean)
                   : Future[CompileResult] = {

    val uri: String = str"file://${layout.workDir(target.id).value}?id=${target.id.key}"
    val params = new CompileParams(List(new BuildTargetIdentifier(uri)).asJava)
    if(pipelining) params.setArguments(List("--pipeline").asJava)
    val bspTargetIds = deepDependencies(target.id).map { dep =>
      new BuildTargetIdentifier(str"file://${layout.workDir(dep).value}?id=${dep.key}")
    }
    val scalacOptionsParams = new ScalacOptionsParams(bspTargetIds.toList.asJava)
    Compilation.bspPool.borrow(layout.base) { conn =>
      val bspCompileResult: Try[BspCompileResult] = conn.provision(this, target.id, layout, Some(multiplexer)) { server =>
        wrapServerErrors(server.buildTargetCompile(params))
      }
      val scalacOptions: Try[ScalacOptionsResult] = conn.provision(this, target.id, layout, None) { server =>
        wrapServerErrors(server.buildTargetScalacOptions(scalacOptionsParams))
      }
      conn.writeTrace(layout)
      conn.writeMessages(layout)
      CompileResult(bspCompileResult.get, scalacOptions.get)
    }
  }

  private[this] def wrapServerErrors[T](f: => CompletableFuture[T]): Try[T] =
    Outcome.rescue[ExecutionException] { e: ExecutionException => BuildServerError(e.getCause) } (f.get)


  def compile(io: Io,
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
      else compile(io, dependencyTarget.ref, multiplexer, futures, layout, globalPolicy, args, pipelining)
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

        compileModule(io, target, layout, target.kind == Application, multiplexer, pipelining).map {
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

case class ProjectSpec(project: Project, repos: Map[RepoId, SourceRepo])

case class Entity(project: Project, schema: Schema, path: Path) {
  def spec: ProjectSpec = {
    val repoIds = project.allRepoIds
    ProjectSpec(project, schema.repos.to[List].filter(repoIds contains _.id).map { r => (r.id, r) }.toMap)
  }
}

/** A Universe represents a the fully-resolved set of projects available in the layer */
case class Universe(entities: Map[ProjectId, Entity] = Map()) {
  //projects: Map[ProjectId, Project] = Map(),
  //schemas: Map[ProjectId, Schema] = Map(),
  //dirs: Map[ProjectId, Path] = Map()) {

  def ids: Set[ProjectId] = entities.keySet
  def entity(id: ProjectId): Try[Entity] = entities.get(id).ascribe(ItemNotFound(id))

  def makeTarget(io: Io, ref: ModuleRef, layout: Layout): Try[Target] =
    for {
      resolvedProject <- entity(ref.projectId)
      module          <- resolvedProject.project(ref.moduleId)
      compiler        <- if(module.compiler == ModuleRef.JavaRef) Success(None)
                         else makeTarget(io, module.compiler, layout).map(Some(_))
      binaries        <- module.allBinaries.map(_.paths(io)).sequence.map(_.flatten)
      dependencies    <- module.after.traverse { dep => for{
                           origin <- entity(dep.projectId)
                         } yield TargetId(origin.schema.id, dep)}
      checkouts       <- checkout(ref, layout)
    } yield {
      val sourcePaths = module.localSources.map(_ in resolvedProject.path).to[List] ++
        module.sharedSources.map(_.path in layout.sharedDir).to[List] ++
        checkouts.flatMap { c =>
          c.local match {
            case Some(p) => c.sources.map(_ in p)
            case None    => c.sources.map(_ in c.path(layout))
          }
        }
      Target(
        ref,
        resolvedProject.schema.id,
        module.kind,
        module.main,
        module.plugin,
        resolvedProject.schema.repos.map(_.repo).to[List],
        checkouts.to[List],
        binaries.to[List],
        dependencies.to[List],
        compiler,
        module.bloopSpec,
        module.params.to[List],
        module.policy.to[List],
        ref.intransitive,
        sourcePaths,
        module.environment.map { e => (e.key, e.value) }.toMap,
        module.properties.map { p => (p.key, p.value) }.toMap
      )
    }

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

  private[fury] def dependencies(ref: ModuleRef, layout: Layout): Try[Set[ModuleRef]] =
    resolveTransitiveDependencies(forbidden = Set.empty, ref, layout)

  private[this] def resolveTransitiveDependencies(forbidden: Set[ModuleRef], ref: ModuleRef, layout: Layout): Try[Set[ModuleRef]] =
    for {
      entity   <- entity(ref.projectId)
      module   <- entity.project(ref.moduleId)
      deps     =  module.after ++ module.compilerDependencies
      repeated =  deps.intersect(forbidden)
      _        <- if (repeated.isEmpty) ~() else Failure(CyclesInDependencies(repeated))
      tDeps    <- deps.map(resolveTransitiveDependencies(forbidden + ref, _, layout).filter(!_.contains(ref))).sequence
    } yield deps ++ tDeps.flatten

  def clean(ref: ModuleRef, layout: Layout): Unit = layout.classesDir.delete().unit

  def getMod(ref: ModuleRef): Try[Module] = for {
    entity <- entity(ref.projectId)
    module <- entity.project(ref.moduleId)
  } yield module

  def compilation(io: Io, ref: ModuleRef, policy: Policy, layout: Layout): Try[Compilation] = for {
    target    <- makeTarget(io, ref, layout)
    entity    <- entity(ref.projectId)
    graph     <- dependencies(ref, layout).map(_.map(makeTarget(io, _, layout)).map { a =>
                   a.map { dependencyTarget =>
                     (dependencyTarget.id, dependencyTarget.dependencies ++ dependencyTarget.compiler.map(_.id))
                   }
                 }.sequence.map(_.toMap.updated(target.id, target.dependencies ++
                     target.compiler.map(_.id)))).flatten
    targets   <- graph.keys.map { targetId =>
                  makeTarget(io, targetId.ref, layout).map(targetId.ref -> _)
                }.sequence.map(_.toMap)
    permissions = targets.flatMap(_._2.permissions)
    _         <- policy.checkAll(permissions)
    appModules = targets.filter(_._2.executed)
    subgraphs  = DirectedGraph(graph.mapValues(_.to[Set])).subgraph(appModules.map(_._2.id).to[Set] +
                     TargetId(entity.schema.id, ref)).connections.mapValues(_.to[List])
    checkouts <- graph.keys.map { targetId => checkout(targetId.ref, layout) }.sequence
  } yield
    Compilation(graph, subgraphs, checkouts.foldLeft(Set[Checkout]())(_ ++ _),
        targets ++ (target.compiler.map { compilerTarget => compilerTarget.ref -> compilerTarget }), this)
}

case class Hierarchy(schema: Schema, dir: Path, inherited: Set[Hierarchy]) {
  lazy val universe: Try[Universe] = {
    val localProjectIds = schema.projects.map(_.id)

    def merge(universe: Try[Universe], hierarchy: Hierarchy) = for {
      projects             <- universe
      nextProjects         <- hierarchy.universe
      potentialConflictIds  = (projects.ids -- localProjectIds).intersect(nextProjects.ids)

      conflictIds           = potentialConflictIds.filter { id =>
                                projects.entity(id).map(_.spec) != nextProjects.entity(id).map(_.spec)
                              }

      allProjects          <- conflictIds match {
                                case x if x.isEmpty => Success(projects ++ nextProjects)
                                case _ => Failure(ProjectConflict(conflictIds/*, h1 = this, h2 = hierarchy*/))
                              }
    } yield allProjects

    val empty: Try[Universe] = Success(Universe())

    for(allInherited <- inherited.foldLeft(empty)(merge)) yield {
      val schemaEntities = schema.projects.map { project => project.id -> Entity(project, schema, dir) }
      allInherited ++ Universe(schemaEntities.toMap)
    }
  }
}

object Schema {
  implicit val msgShow: MsgShow[Schema] = v => UserMsg(_.schema(v.id.key))
  implicit val stringShow: StringShow[Schema] = _.id.key
  implicit def diff: Diff[Schema] = Diff.gen[Schema]
}

case class Schema(id: SchemaId,
                  projects: SortedSet[Project] = TreeSet(),
                  repos: SortedSet[SourceRepo] = TreeSet(),
                  imports: SortedSet[SchemaRef] = TreeSet(),
                  main: Option[ProjectId] = None) {

  def apply(id: ProjectId) = projects.findBy(id)
  def repo(repoId: RepoId, layout: Layout): Try[SourceRepo] = repos.findBy(repoId)
  def moduleRefs: SortedSet[ModuleRef] = projects.flatMap(_.moduleRefs)
  def mainProject: Try[Option[Project]] = main.map(projects.findBy(_)).to[List].sequence.map(_.headOption)
  def sourceRepoIds: SortedSet[RepoId] = repos.map(_.id)
  def duplicate(id: String) = copy(id = SchemaId(id))

  def compilerRefs(io: Io, layout: Layout, https: Boolean): List[ModuleRef] =
    allProjects(io, layout, https).toOption.to[List].flatMap(_.flatMap(_.compilerRefs))

  def importCandidates(io: Io, layout: Layout, https: Boolean): List[String] =
    repos.to[List].flatMap(_.importCandidates(io, this, layout, https).toOption.to[List].flatten)

  def hierarchy(io: Io, dir: Path, layout: Layout, https: Boolean): Try[Hierarchy] = for {
    imps <- imports.map { ref => for {
      repo         <- repos.findBy(ref.repo)
      repoDir      <- repo.fullCheckout.get(io, layout, https)
      nestedLayout <- ~Layout(layout.home, repoDir, layout.env, repoDir)
      layer        <- Layer.read(io, nestedLayout.furyConfig, nestedLayout)
      resolved     <- layer.schemas.findBy(ref.schema)
      tree         <- resolved.hierarchy(io, repoDir, layout, https)
    } yield tree }.sequence
  } yield Hierarchy(this, dir, imps)

  def importedSchemas(io: Io, layout: Layout, https: Boolean): Try[List[Schema]] =
    imports.to[List].map(resolve(_, io, layout, https)).sequence

  def allProjects(io: Io, layout: Layout, https: Boolean): Try[List[Project]] = {
    @tailrec
    def flatten[T](treeNodes: List[T])(aggregated: List[T], getChildren: T => Try[List[T]]): Try[List[T]] = {
      treeNodes match{
        case Nil => ~aggregated
        case head :: tail =>
          getChildren(head) match {
            case Success(ch) => flatten(ch ::: tail)(head :: aggregated, getChildren)
            case fail => fail
          }
      }
    }

    for {
      allSchemas <- flatten(List(this))(Nil, _.importedSchemas(io, layout, https))
    } yield allSchemas.flatMap(_.projects)
  }

  def unused(projectId: ProjectId): Try[ProjectId] = projects.find(_.id == projectId) match {
    case None    => Success(projectId)
    case Some(m) => Failure(ProjectAlreadyExists(m.id))
  }

  def resolve(ref: SchemaRef, io: Io, layout: Layout, https: Boolean): Try[Schema] = for {
    repo     <- repos.findBy(ref.repo)
    dir      <- repo.fullCheckout.get(io, layout, https)
    layer    <- Layer.read(io, Layout(layout.home, dir, layout.env, dir).furyConfig, layout)
    resolved <- layer.schemas.findBy(ref.schema)
  } yield resolved
}

case class Layer(version: Int = Layer.CurrentVersion,
                 schemas: SortedSet[Schema] = TreeSet(Schema(SchemaId.default)),
                 main: SchemaId = SchemaId.default,
                 aliases: SortedSet[Alias] = TreeSet()) { layer =>

  def mainSchema: Try[Schema] = schemas.findBy(main)
  def showSchema: Boolean = schemas.size > 1
  def apply(schemaId: SchemaId): Try[Schema] = schemas.find(_.id == schemaId).ascribe(ItemNotFound(schemaId))
  def projects: Try[SortedSet[Project]] = mainSchema.map(_.projects)
}

object Layer {
  val CurrentVersion = 3

  def read(io: Io, string: String, layout: Layout): Try[Layer] =
    Success(Ogdl.read[Layer](string, upgrade(io, layout, _)))

  def read(io: Io, file: Path, layout: Layout): Try[Layer] =
    Success(Ogdl.read[Layer](file, upgrade(io, layout, _)).toOption.getOrElse(Layer()))

  def save(io: Io, layer: Layer, layout: Layout): Try[Unit] = for {
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
                        io.println(msg"Checking commit hash for ${repo.repo()}")
                        val commit =
                          Shell(layout.env).git.lsRemoteRefSpec(repo.repo(), repo.refSpec()).toOption.get
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

  def fromString(str: String, https: Boolean): String = str match {
    case "." => ""
    case r"gh:$group@([A-Za-z0-9_\-\.]+)/$project@([A-Za-z0-9\._\-]+)" =>
      if(https) str"https://github.com/$group/$project.git"
      else str"git@github.com:$group/$project.git"
    case r"gl:$group@([A-Za-z0-9_\-\.]+)/$project@([A-Za-z0-9\._\-]+)" =>
      if(https) str"https://gitlab.com/$group/$project.git"
      else str"git@gitlab.com:$group/$project.git"
    case r"bb:$group@([A-Za-z0-9_\-\.]+)/$project@([A-Za-z0-9\._\-]+)" =>
      if(https) str"https://bitbucket.com/$group/$project.git"
      str"git@bitbucket.com:$group/$project.git"
    case ExistingLocalFileAsAbspath(abspath) =>
      abspath
    case other =>
      other
  }
}

case class Checkout(repoId: RepoId,
                    repo: Repo,
                    local: Option[Path],
                    commit: Commit,
                    refSpec: RefSpec,
                    sources: List[Path]) {

  def hash: Digest = this.digest[Md5]
  def path(layout: Layout): Path = layout.srcsDir / hash.encoded

  def get(io: Io, layout: Layout, https: Boolean): Try[Path] = for {
    repoDir    <- repo.fetch(io, layout, https)
    workingDir <- checkout(io, layout)
  } yield workingDir

  private def checkout(io: Io, layout: Layout): Try[Path] =
    local.map(Success(_)).getOrElse {
      if(!(path(layout) / ".done").exists) {
        if(path(layout).exists()) {
          val sourceText = if(sources.isEmpty) "all sources" else sources.map(_.value).mkString("[", ", ", "]")
          io.println(msg"Found incomplete checkout of $sourceText")
          path(layout).delete()
        }

        val sourceDesc: UserMsg = sources match {
          case List() =>
            UserMsg { theme => theme.path("*") }
          case head :: Nil =>
            msg"$head"
          case head :: tail =>
            val init = tail.foldLeft(msg"${'{'}$head") { case (str, next) => msg"$str${','} $next" }
            msg"$init${'}'}"
        }

        io.println(msg"Checking out $sourceDesc from repository $repoId")
        path(layout).mkdir()
        Shell(layout.env).git
          .sparseCheckout(repo.path(layout), path(layout), sources, refSpec = refSpec.id, commit = commit.id)
          .map(path(layout).waive)
      } else Success(path(layout))
    }
}

object SourceRepo {
  implicit val msgShow: MsgShow[SourceRepo] = r => UserMsg(_.repo(r.id.key))
  implicit val stringShow: StringShow[SourceRepo] = _.id.key
  implicit def diff: Diff[SourceRepo] = Diff.gen[SourceRepo]
}

case class SourceRepo(id: RepoId, repo: Repo, track: RefSpec, commit: Commit, local: Option[Path]) {
  def listFiles(io: Io, layout: Layout, https: Boolean): Try[List[Path]] = for {
    dir    <- local.map(Success(_)).getOrElse(repo.fetch(io, layout, https))
    commit <- ~Shell(layout.env).git.getTag(dir, track.id).toOption.orElse(Shell(layout.env).git.getBranchHead(dir,
                  track.id).toOption).getOrElse(track.id)
    files  <- local.map(Success(dir.children.map(Path(_))).waive).getOrElse(Shell(layout.env).git.lsTree(dir,
                  commit))
  } yield files

  def fullCheckout: Checkout = Checkout(id, repo, local, commit, track, List())

  def importCandidates(io: Io, schema: Schema, layout: Layout, https: Boolean): Try[List[String]] = for {
    repoDir     <- repo.fetch(io, layout, https)
    layerString <- Shell(layout.env).git.showFile(repoDir, "layer.fury")
    layer       <- Layer.read(io, layerString, layout)
    schemas     <- ~layer.schemas.to[List]
  } yield schemas.map { schema => str"${id.key}:${schema.id.key}" }

  def current(io: Io, layout: Layout, https: Boolean): Try[RefSpec] = for {
    dir    <- local.map(Success(_)).getOrElse(repo.fetch(io, layout, https))
    commit <- Shell(layout.env).git.getCommit(dir)
  } yield RefSpec(commit)

  def sourceCandidates(io: Io, layout: Layout, https: Boolean)(pred: String => Boolean): Try[Set[Source]] =
    listFiles(io, layout, https).map(_.filter { f => pred(f.filename) }.map { p =>
        ExternalSource(id, p.parent): Source }.to[Set])
}

case class Repo(ref: String) {
  def hash: Digest               = ref.digest[Md5]
  def path(layout: Layout): Path = layout.reposDir / hash.encoded

  def update(layout: Layout): Try[UserMsg] = for {
    oldCommit <- Shell(layout.env).git.getCommit(path(layout)).map(Commit(_))
    _         <- Shell(layout.env).git.fetch(path(layout), None)
    newCommit <- Shell(layout.env).git.getCommit(path(layout)).map(Commit(_))
    msg <- ~(if(oldCommit != newCommit) msg"Repository $this updated to new commit $newCommit"
              else msg"Repository $this is unchanged")
  } yield msg

  def getCommitFromTag(layout: Layout, tag: RefSpec): Try[Commit] =
    for(commit <- Shell(layout.env).git.getCommitFromTag(path(layout), tag.id)) yield Commit(commit)

  def fetch(io: Io, layout: Layout, https: Boolean): Try[Path] =
    if(!(path(layout) / ".done").exists) {
      if(path(layout).exists()) {
        io.println(msg"Found incomplete clone of $this at ${path(layout)}")
        path(layout).delete()
      }

      io.println(msg"Cloning repository at $this")
      path(layout).mkdir()
      Shell(layout.env).git.cloneBare(Repo.fromString(ref, https), path(layout)).map(path(layout).waive)
    } else Success(path(layout))

  def simplified: String = ref match {
    case r"git@github.com:$group@(.*)/$project@(.*)\.git"    => str"gh:$group/$project"
    case r"git@bitbucket.com:$group@(.*)/$project@(.*)\.git" => str"bb:$group/$project"
    case r"git@gitlab.com:$group@(.*)/$project@(.*)\.git"    => str"gl:$group/$project"
    case other                                               => other
  }

  def projectName: Try[RepoId] = ref match {
    case r".*/$project@([^\/]*).git" => Success(RepoId(project))
    case value                       => Failure(InvalidValue(value))
  }
}

sealed trait CompileEvent
case object Tick                                                 extends CompileEvent
case class StartCompile(ref: ModuleRef)                          extends CompileEvent
case class CompilationProgress(ref: ModuleRef, progress: Double) extends CompileEvent
case class StopCompile(ref: ModuleRef, success: Boolean)         extends CompileEvent
case class NoCompile(ref: ModuleRef)                             extends CompileEvent
case class SkipCompile(ref: ModuleRef)                           extends CompileEvent
case class Print(ref: ModuleRef, line: String)                   extends CompileEvent
case class StartRun(ref: ModuleRef)                              extends CompileEvent
case class StopRun(ref: ModuleRef)                               extends CompileEvent
case class DiagnosticMsg(ref: ModuleRef, msg: DiagnosticMessage) extends CompileEvent

case class CompileResult(bspCompileResult: BspCompileResult, scalacOptions: ScalacOptionsResult) {
  def isSuccessful: Boolean = bspCompileResult.getStatusCode == StatusCode.OK
  def classDirectories: Set[Path] = scalacOptions.getItems.asScala.toSet.map { x: ScalacOptionsItem =>
    Path(new URI(x.getClassDirectory))
  }
  def asTry: Try[CompileResult] = if(isSuccessful) Success(this) else Failure(CompilationFailure())
  def failed: CompileResult = {
    val updatedResult = new BspCompileResult(StatusCode.ERROR)
    updatedResult.setOriginId(bspCompileResult.getOriginId)
    updatedResult.setDataKind(bspCompileResult.getDataKind)
    updatedResult.setData(bspCompileResult.getData)
    copy(bspCompileResult = updatedResult)
  }
}

object CompileResult {
  def merge(results: List[CompileResult]): CompileResult = {
    CompileResult(merge(results.map(_.bspCompileResult)), merge(results.map(_.scalacOptions)))
  }

  private def merge(results: List[BspCompileResult]): BspCompileResult = {
    val distinctStatuses = results.map(_.getStatusCode).toSet
    val aggregatedStatus = List(StatusCode.CANCELLED, StatusCode.ERROR, StatusCode.OK).find(distinctStatuses.contains)
    val mergedResult = new BspCompileResult(aggregatedStatus.getOrElse(StatusCode.OK))
    results.headOption.foreach { res =>
      //TODO think of a better way to merge those fields
      mergedResult.setOriginId(res.getOriginId)
      mergedResult.setDataKind(res.getDataKind)
      mergedResult.setData(res.getData)
    }
    mergedResult
  }

  private def merge(results: List[ScalacOptionsResult]): ScalacOptionsResult = {
    new ScalacOptionsResult(results.flatMap(_.getItems.asScala).asJava)
  }
}

case class Target(ref: ModuleRef,
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
                  permissions: List[Permission],
                  intransitive: Boolean,
                  sourcePaths: List[Path],
                  environment: Map[String, String],
                  properties: Map[String, String]) {

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

case class Project(id: ProjectId,
                   modules: SortedSet[Module] = TreeSet(),
                   main: Option[ModuleId] = None,
                   license: LicenseId = License.unknown,
                   description: String = "",
                   compiler: Option[ModuleRef] = None) {

  def apply(module: ModuleId): Try[Module] = modules.findBy(module)
  def moduleRefs: List[ModuleRef] = modules.to[List].map(_.ref(this))
  def mainModule: Try[Option[Module]] = main.map(modules.findBy(_)).to[List].sequence.map(_.headOption)

  def compilerRefs: List[ModuleRef] =
    modules.to[List].collect { case m if m.kind == Compiler => m.ref(this) }

  def unused(moduleId: ModuleId) = modules.findBy(moduleId) match {
    case Success(_) => Failure(ModuleAlreadyExists(moduleId))
    case _          => Success(moduleId)
  }

  def allRepoIds: Set[RepoId] = modules.flatMap(_.sources).collect { case ExternalSource(repoId, _) => repoId }
}

object Source {
  implicit val stringShow: StringShow[Source] = _.description
  implicit val ogdlReader: OgdlReader[Source] = src => unapply(src()).get // FIXME
  implicit val ogdlWriter: OgdlWriter[Source] = src => Ogdl(src.description)

  implicit val sourceDiff: Diff[Source] =
    (l, r) => if(l == r) Nil else List(Difference(msg"source", msg"", msg"$l", msg"$r"))

  implicit val msgShow: MsgShow[Source] = v => UserMsg { theme =>
    v match {
      case ExternalSource(repoId, path) =>
        msg"${theme.repo(repoId.key)}${theme.gray(":")}${theme.path(path.value)}".string(theme)
      case SharedSource(path) =>
        msg"${theme.repo("shared")}${theme.gray(":")}${theme.path(path.value)}".string(theme)
      case LocalSource(path) =>
        msg"${theme.path(path.value)}".string(theme)
    }
  }

  def unapply(string: String): Option[Source] = string match {
    case r"shared:$path@(.*)" =>
      Some(SharedSource(Path(path)))
    case r"$repo@([a-z][a-z0-9\.\-]*[a-z0-9]):$path@(.*)" =>
      Some(ExternalSource(RepoId(repo), Path(path)))
    case r"$path@(.*)" =>
      Some(LocalSource(Path(path)))
    case _ =>
      None
  }

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
  def hash(schema: Schema, layout: Layout): Try[Digest] = schema.repo(repoId, layout).map((path, _).digest[Md5])
  def repoIdentifier: RepoId = repoId
}

case class SharedSource(path: Path) extends Source {
  def description: String = str"shared:${path.value}"
  def hash(schema: Schema, layout: Layout): Try[Digest] = Success((-2, path).digest[Md5])
  def repoIdentifier: RepoId = RepoId("-")
}

case class LocalSource(path: Path) extends Source {
  def description: String = str"${path.value}"
  def hash(schema: Schema, layout: Layout): Try[Digest] = Success((-1, path).digest[Md5])
  def repoIdentifier: RepoId = RepoId("-")
}
