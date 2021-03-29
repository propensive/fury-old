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

import fury._, io._, model._, text._

import jovian._

import ch.epfl.scala.bsp4j.{CompileResult => _, TaskId => _, _}
import com.google.gson._

import scala.collection.JavaConverters._

class BspClient(layout: Layout, job: Job) extends BuildClient {

  override def onBuildShowMessage(params: ShowMessageParams): Unit =
    Option(params.getOriginId).foreach { originId =>
      val jobId = JobId(originId)
      log.info(msg"$jobId: "+params.getMessage)
    }

  override def onBuildLogMessage(params: LogMessageParams): Unit =
    Option(params.getOriginId).foreach { originId =>
      val jobId = JobId(originId)
      log.fine(msg"$jobId: "+params.getMessage)
    }

  override def onBuildTargetDidChange(params: DidChangeBuildTarget): Unit = ()

  override def onBuildPublishDiagnostics(params: PublishDiagnosticsParams): Unit = {
    val taskId = TaskId(ModuleRef.fromUri(params.getBuildTarget.getUri).urlSafe)
    val uri = params.getTextDocument.getUri
    val sourceFile = Path(uri.replaceAll("file:\\/+", "/"))
    
    val issues = params.getDiagnostics.asScala.to[List].map { diagnostic =>
      val code = Option(diagnostic.getCode)
      val startPos = diagnostic.getRange.getStart
      val endPos = diagnostic.getRange.getEnd
      val line = startPos.getLine.intValue
      val start = startPos.getCharacter.intValue
      val chars = CharRange(start, Some(endPos.getCharacter))
      val highlight = Highlight(line, Some(chars))

      val end = if(endPos.getLine > startPos.getLine) None else Some(endPos.getCharacter -
          startPos.getCharacter)
      
      val severity = diagnostic.getSeverity match {
        case DiagnosticSeverity.ERROR       => Fail
        case DiagnosticSeverity.WARNING     => Warn
        case DiagnosticSeverity.INFORMATION => Info
        case DiagnosticSeverity.HINT        => Fine
      }

      Issue(diagnostic.getMessage, severity, Some(sourceFile), code.map(Code(_, Some(highlight))))
    }

    Bus.enqueue(msg"Added diagnostics", _.addIssues(job.id, taskId, issues))
  }

  private def getTaskId[T](data: T): TaskId = TaskId(ModuleRef.fromUri(data match { case json: JsonElement =>
    json.getAsJsonObject().get("target").getAsJsonObject.get("uri").getAsString
  }).urlSafe)


  override def onBuildTaskProgress(params: TaskProgressParams): Unit = {
    val progress = params.getProgress/params.getTotal.toDouble
    val taskId = getTaskId(params.getData)
    Bus.enqueue(msg"Update progress of ${taskId.key} to ${params.getProgress.toInt}${'/'}${params.getTotal.toInt}",
      _.setProgress(job.id, taskId, progress))
  }

  override def onBuildTaskStart(params: TaskStartParams): Unit = {
    val taskId = getTaskId(params.getData)
    Bus.enqueue(msg"Started building ${taskId.key}", _.setStarted(job.id, taskId))
  }

  override def onBuildTaskFinish(params: TaskFinishParams): Unit = params.getDataKind match {
    case TaskDataKind.COMPILE_REPORT =>
      val taskId = getTaskId(params.getData)
      Bus.enqueue(msg"Finished building ${taskId.key}", _.setFinished(job.id, taskId))
  }
}