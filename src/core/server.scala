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

  override def shutdown(): Unit = BloopServer.synchronized {
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