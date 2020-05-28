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
  } yield broadcast(Print(build.ref, params.getMessage))

  override def onBuildLogMessage(params: LogMessageParams): Unit = for {
    idString <- Option(params.getOriginId)
    originId <- RequestOriginId.unapply(idString)
    build    <- Build.findOrigin(originId)
  } yield broadcast(Print(build.ref, params.getMessage))

  override def onBuildPublishDiagnostics(params: PublishDiagnosticsParams): Unit = {
    val ref = extractModuleRef(params.getBuildTarget.getUri)
    val fileName = new java.net.URI(params.getTextDocument.getUri).getRawPath
    val build = for {
      idString <- Option(params.getOriginId)
      originId <- RequestOriginId.unapply(idString)
      build    <- Build.findOrigin(originId)
    } yield build

    val repos = build match {
      case Some(c) => c.checkouts.checkouts.map { checkout => (checkout.path.value, checkout.repoId)}.toMap
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
        val signal = if(success && build(ref).module.kind.needsExec) StartRun(ref) else StopRun(ref)
        broadcast(signal)
      }
  }
}