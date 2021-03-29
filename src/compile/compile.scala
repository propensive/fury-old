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

import fury.core.UiGraph.CompileIssue, fury.io._, fury.model._, fury.text._,
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
import java.util.concurrent.CompletableFuture

trait FuryBspServer extends BuildServer with ScalaBuildServer

object BloopServer {
  private var connections: Map[Path, Connection] = Map()
  private val bloopVersion = "1.4.8"

  case class Connection(server: FuryBspServer, client: BspClient) {
    private[this] def wrapServerErrors[T](f: => CompletableFuture[T]): Try[T] =
      Try(f.get).recoverWith { case e: ExecutionException => Failure(BuildServerError(e.getCause)) }
    
    def compile() = {

    }
  }
  
  private def printStream(): PrintStream = Log.printStream { str =>
    (if(str.indexOf("[D]") == 9) str.drop(17) else str) match {
      case r"Starting the bsp launcher.*"              => log.fine(msg"Starting the BSP launcher...")
      case r"Opening a bsp server connection with.*"   => log.fine(msg"Opening a BSP server connection")
      case r"Waiting for a connection at"              => log.info(msg"Waiting for a socket connection")
      case r"Loading project from .*" |
          r"Loading previous analysis for .*" |
          r".*detected in file:.*" |
          r"  => .*" |
          r"Waiting.*" |
          r"Starting thread.*" |
          r"Command: .*" |
          r"Deduplicating compilation of .*" |
          "\n"                                         => log.fine(str)
      case r"Creating a scala instance from Bloop"     => log.info("Instantiating a new Scala compiler")
      case r"The server is listening.*"                => log.fine(msg"BSP server is listening")
      case r"bad option '-Ysemanticdb' was ignored"    => log.fine(msg"Tried to use -Ysemanticdb")
      case r"No server running at .*"                  => log.info(msg"Could not detect a local BSP server")
      case r"A bloop installation has been detected.*" => log.info(msg"Detected an existing Bloop installation")
      case other                                       => log.fine(msg"${'['}bsp${']'} ${other}")
    }
  }

  private def connect(layout: Layout, job: Job): Connection = synchronized {
    val serverIoPipe: Pipe = Pipe.open()
    val serverIn: InputStream = Channels.newInputStream(serverIoPipe.source())
    val clientOut: OutputStream = Channels.newOutputStream(serverIoPipe.sink())
    val clientIoPipe: Pipe = Pipe.open()
    val clientIn: InputStream = Channels.newInputStream(clientIoPipe.source())
    val serverOut: OutputStream = Channels.newOutputStream(clientIoPipe.sink())

    val promise: Promise[Unit] = Promise()

    val main = new LauncherMain(serverIn, serverOut, printStream(), StandardCharsets.UTF_8,
        bloop.bloopgun.core.Shell.default, None, None, promise)
    
    Run("launchbloop") {
      main.runLauncher(bloopVersion, false, Nil) match {
        case FailedToConnectToServer   => log.info("Failed to connect to BSP server")
        case FailedToInstallBloop      => log.info("Failed to install Bloop")
        case FailedToOpenBspConnection => log.info("Failed to open a BSP connection")
        case FailedToParseArguments    => log.info("Failed to parse Bloop arguments")
        case SuccessfulRun             => log.info("Successfully launched Bloop")
      }
    }
    
    Await.result(promise.future, Duration.Inf)

    val client = new BspClient(layout, job)
      
    val bspServer = new Launcher.Builder[FuryBspServer]().setRemoteInterface(classOf[FuryBspServer])
      .setExecutorService(Threads.bsp).setInput(clientIn).setOutput(clientOut).setLocalService(client).create()

    bspServer.startListening()
      
    val server = bspServer.getRemoteProxy
    val capabilities = new BuildClientCapabilities(List("scala").asJava)
    
    val initParams = new InitializeBuildParams("fury", FuryVersion.current, "2.0.0-M4",
        layout.baseDir.uriString, capabilities)

    server.buildInitialize(initParams).get
    server.onBuildInitialized()
    
    Connection(server, client)
  }

  def borrow[T](layout: Layout, job: Job)(block: Connection => T): T = {
    val conn = BloopServer.synchronized {
      log.fine(msg"Opening a new BSP connection at ${layout.baseDir}")
      val conn = connect(layout, job)
      
      connections += layout.baseDir -> conn
      conn
    }

    try block(conn) finally BloopServer.synchronized {
      try conn.server.buildShutdown().get() finally { connections -= layout.baseDir }
    }
  }
  
  def workDirectories: Set[Path] = connections.keySet
}
