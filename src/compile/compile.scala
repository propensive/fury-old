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
package fury.core

import fury.core.UiGraph.CompileIssue, fury.core.Lifecycle.Session, fury.io._, fury.model._, fury.text._,
    fury.utils._


import gastronomy._
import kaleidoscope._
import mercator._
import jovian._

import org.eclipse.lsp4j.jsonrpc.Launcher
import bloop.launcher.LauncherMain
import bloop.launcher.LauncherStatus._
import ch.epfl.scala.bsp4j.{CompileResult => _, TaskId => _, _}
import com.google.gson.{Gson, JsonElement}

import scala.collection.JavaConverters._
import scala.concurrent._, ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.higherKinds
import scala.reflect.{ClassTag, classTag}
import scala.util._
import scala.util.control.NonFatal

import java.io._
import java.nio.charset.StandardCharsets
import java.nio.channels._

trait FuryBspServer extends BuildServer with ScalaBuildServer

object BloopServer extends Lifecycle.Shutdown with Lifecycle.ResourceHolder {
  override type Resource = Connection

  Lifecycle.bloopServer.complete(Success(this))
 
  private var lock: Promise[Unit] = Promise.successful(())
  private var connections: Map[Path, Connection] = Map.empty
  private var usages: Map[Session, Connection] = Map.empty
  private val bloopVersion = "1.4.8"

  def subscribers(bspClient: FuryBuildClient): List[Session] = usages.synchronized {
    usages.collect { case (session, conn) if conn.client == bspClient => session }.to[List]
  }

  override def acquire(session: Session, connection: Connection): Unit =
    usages.synchronized { usages += session -> connection }

  override def release(session: Session): Unit = usages.synchronized { usages -= session }

  case class Connection(server: FuryBspServer, client: FuryBuildClient, thread: Thread)
  
  private def connect(dir: Path, build: Build, ref: ModuleRef, layout: Layout, trace: Option[Path] = None)
                     (implicit log: Log): Connection = BloopServer.synchronized {

      val serverIoPipe: Pipe = Pipe.open()
      val serverIn: InputStream = Channels.newInputStream(serverIoPipe.source())
      val clientOut: OutputStream = Channels.newOutputStream(serverIoPipe.sink())
      val clientIoPipe: Pipe = Pipe.open()
      val clientIn: InputStream = Channels.newInputStream(clientIoPipe.source())
      val serverOut: OutputStream = Channels.newOutputStream(clientIoPipe.sink())

      val logging: PrintStream = log.stream { str =>
        (if(str.indexOf("[D]") == 9) str.drop(17) else str) match {
          case r"Starting the bsp launcher.*" =>
            log.note(msg"Starting the BSP launcher...")
          case r"Opening a bsp server connection with.*" =>
            log.note(msg"Opening a BSP server connection")
          case r"Waiting for a connection at" =>
            log.info(msg"Waiting for a socket connection")
          case r"Loading project from .*" |
              r"Loading previous analysis for .*" |
              r".*detected in file:.*" |
              r"  => .*" |
              r"Waiting.*" |
              r"Starting thread.*" |
              r"Command: .*" |
              r"Deduplicating compilation of .*" |
              "\n" =>
            None
          case r"Creating a scala instance from Bloop" =>
            log.info("Instantiating a new instance of the Scala compiler")
          case r"The server is listening for incoming connections.*" =>
            log.note(msg"BSP server is listening for incoming connections")
          case r"bad option '-Ysemanticdb' was ignored" =>
            log.note(msg"Tried to use -Ysemanticdb")
            None
          case r"No server running at .*" =>
            log.info("Could not detect a BSP server running locally")
          case r"A bloop installation has been detected.*" =>
            log.info("Detected an existing Bloop installation")
          case other =>
            log.note(msg"${'['}bsp${']'} ${other}")
        }
      }

      val launcher: LauncherMain = new LauncherMain(serverIn, serverOut, logging,
          StandardCharsets.UTF_8, bloop.bloopgun.core.Shell.default, None, None, Promise())
      
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

  def borrow[T](dir: Path, build: Build, ref: ModuleRef, layout: Layout, cancellation: Option[Future[Unit]])
               (block: Connection => T)(implicit log: Log): Try[T] = {

    val conn = BloopServer.synchronized(connections.get(dir)).getOrElse {
      log.note(msg"Opening a new BSP connection at $dir")
      val tracePath: Option[Path] = if(ManagedConfig().trace) {
        val path = layout.logsDir / s"${java.time.LocalDateTime.now().toString}.log"
        log.note(str"BSP trace log is at $path")
        Some(path)
      } else None

      val newConnection = connect(dir, build, ref, layout, trace = tracePath)
      
      connections += dir -> newConnection
      newConnection
    }

    cancellation.foreach(_.andThen { case _ =>
      BloopServer.synchronized {
        conn.server.buildShutdown()
        connections -= dir
      }
    })

    Try {
      acquire(Lifecycle.currentSession, conn)
      conn.synchronized(block(conn))
    }
  }
  
  def workDirectories: Set[Path] = connections.keySet

  override def shutdown(): Unit = {
    BloopServer.synchronized {
      connections.foreach { case(dir, conn) =>
        conn.synchronized { try {
          conn.server.buildShutdown().thenAccept { _ => conn.server.onBuildExit() }.get()
        } catch {
          case NonFatal(e) => println(s"Error while closing the connection for $dir. Cause: ${e.getMessage}")
        } }
      }
      connections = Map.empty
    }
  }
}

class FuryBuildClient(layout: Layout) extends BuildClient {

  def broadcast(event: CompileEvent): Unit = event match {
    case e: ModuleCompileEvent =>
      BloopServer.subscribers(this).map(_.multiplexer).filter(_.contains(e.ref)).foreach(_.fire(e.ref, event))
    case Warning(_) => ()
    case Tick =>
      BloopServer.subscribers(this).map(_.multiplexer).foreach(_.updateAll(Tick))
  }

  override def onBuildShowMessage(params: ShowMessageParams): Unit = for {
    idString <- Option(params.getOriginId)
    originId <- RequestOriginId.unapply(idString)
    build    <- Build.findOrigin(originId)
  } yield {
    Bus.put(LogMessage(params.getMessage))
    broadcast(Print(build.goal, params.getMessage))
  }

  override def onBuildLogMessage(params: LogMessageParams): Unit = for {
    idString <- Option(params.getOriginId)
    originId <- RequestOriginId.unapply(idString)
    build    <- Build.findOrigin(originId)
  } yield {
    Bus.put(LogMessage(params.getMessage))
    broadcast(Print(build.goal, params.getMessage))
  }

  override def onBuildPublishDiagnostics(params: PublishDiagnosticsParams): Unit = {
    val ref = ModuleRef.fromUri(params.getBuildTarget.getUri)
    val fileName = new java.net.URI(params.getTextDocument.getUri).getRawPath
    
    val build = for {
      idString <- Option(params.getOriginId)
      originId <- RequestOriginId.unapply(idString)
      build    <- Build.findOrigin(originId)
    } yield build

    val repos = build.map(_.target.snapshot.stashes.map { case (_, stash) =>
        (stash.path.value, stash.repoId) }.toMap).getOrElse(Map())

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
      
      val highlightedLine = Message { theme =>
        msg"$linePrefix${theme.failure(theme.underline(matching))}$remainder".string(theme).dropWhile(_ == ' ')
      }

      val (repo, filePath) = repos.find { case (k, v) => fileName.startsWith(k) }.map {
        case (k, v) => (v, Path(fileName.drop(k.length + 1)))
      }.getOrElse((RepoId("local"), Path(fileName.drop(layout.baseDir.value.length + 1))))

      val (isWarning, severity) = diag.getSeverity.toString.toLowerCase match {
        case "error"       => (false, Message { theme => msg"${'['}${theme.failure("E")}${']'}".string(theme) })
        case "warning"     => (true,  Message { theme => msg"${'['}${theme.ongoing("W")}${']'}".string(theme) })
        case "information" => (false, Message { theme => msg"${'['}${theme.info("I")}${']'}".string(theme) })
        case _             => (false, Message { theme => msg"${'['}${theme.info("H")}${']'}".string(theme) })
      }

      // FIXME: This may be less than 100% reliable
      val missingMatch = codeLine.only {
        case r".*..port +$pkg@([a-z_\.\-]*)\.[^a-z\.\-]*.*" if diag.getMessage.contains("ot found:") || diag.getMessage.contains("is not a member of") => Pkg(pkg)
      }
      
      missingMatch.map { pkg => broadcast(MissingPkg(ref, pkg)) }.getOrElse {
        broadcast(DiagnosticMsg(
          ref,
          CompileIssue(
            msg"""$severity ${ref}${'>'}${repo}${':'}${filePath}${':'}${lineNo}${':'}${(charNum + 1).toString}
  ${'|'} ${Message(
              theme =>
                diag.getMessage.split("\n").to[List].map(theme.gray(_)).join(msg"""
  ${'|'} """.string(theme)))}
  ${'|'} ${highlightedLine}
  """,
            repo,
            filePath,
            lineNo,
            charNum,
            isWarning
          )
        ))
      }
    }
  }

  override def onBuildTargetDidChange(params: DidChangeBuildTarget): Unit = ()

  private[this] def convertDataTo[A: ClassTag](data: Object): A = {
    val gson = new Gson()
    val json = data.asInstanceOf[JsonElement]
    
    gson.fromJson[A](json, classTag[A].runtimeClass)
  }

  private[this] def getCompileRef(taskNotificationData: AnyRef): ModuleRef = {
    Log().info(msg"Got compile ref: ${convertDataTo[CompileTask](taskNotificationData).getTarget.getUri}")
    val r = ModuleRef.fromUri(convertDataTo[CompileTask](taskNotificationData).getTarget.getUri)
    r
  }

  def getTaskId(taskNotificationData: AnyRef): TaskId = {
    val uri = new java.net.URI(convertDataTo[CompileTask](taskNotificationData).getTarget.getUri)
    val paramMap = uri.getRawQuery.split("&", 2).map(_.split("=", 2)).map { case Array(k, v) => k -> v }.toMap
    TaskId(paramMap("id"))
  }

  override def onBuildTaskProgress(params: TaskProgressParams): Unit = {
    val ref = getCompileRef(params.getData) 
    //Bus.put(TaskProgress())
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
