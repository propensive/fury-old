/*

    Fury, version 0.18.9. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

import fury.model._, UiGraph.Issue, fury.io._, fury.ogdl._
import fury.text.FuryException

import ch.epfl.scala.bsp4j._

import scala.collection.JavaConverters._
import scala.util._

import java.net.URI

case class Entity(project: Project, imports: Set[Pointer])

sealed trait CompileEvent
case object Tick extends CompileEvent
sealed trait ModuleCompileEvent extends CompileEvent { val ref: ModuleRef }
case class StartCompile(ref: ModuleRef) extends ModuleCompileEvent
case class Progress(ref: ModuleRef, progress: Double) extends ModuleCompileEvent
case class StopCompile(ref: ModuleRef, success: Boolean) extends ModuleCompileEvent
case class NoCompile(ref: ModuleRef) extends ModuleCompileEvent
case class SkipCompile(ref: ModuleRef) extends ModuleCompileEvent
case class Print(ref: ModuleRef, line: String) extends ModuleCompileEvent
case class StartRun(ref: ModuleRef) extends ModuleCompileEvent
case class StopRun(ref: ModuleRef) extends ModuleCompileEvent
case class DiagnosticMsg(ref: ModuleRef, issue: Issue) extends ModuleCompileEvent

case class BuildResult(bspResult: CompileResult, scalacOptions: ScalacOptionsResult, exitCode: Option[Int]) {

  def success: Boolean = bspResult.getStatusCode == StatusCode.OK && exitCode.forall(_ == 0)

  def classDirectories: Set[Path] = scalacOptions.getItems.asScala.to[Set].map { x: ScalacOptionsItem =>
    Path(new URI(x.getClassDirectory))
  }

  def asTry: Try[BuildResult] =
    if(success) Success(this)
    else Failure(exitCode.fold[FuryException](CompilationFailure())(ExecutionFailure(_)))

  def asBsp: CompileResult = {
    val compileResult = new CompileResult(if(exitCode.exists(_ != 0)) StatusCode.ERROR else
        bspResult.getStatusCode)
    
    compileResult.setOriginId(bspResult.getOriginId)
    compileResult.setDataKind(bspResult.getDataKind)
    compileResult.setData(bspResult.getData)

    compileResult
  }
}

object BuildResult {
  def merge(results: Iterable[BuildResult]): BuildResult =
    BuildResult(merge(results.map(_.bspResult)), merge(results.map(_.scalacOptions)),
        merge(results.map(_.exitCode)))

  private def merge(results: Iterable[CompileResult]): CompileResult = {
    val distinctStatuses = results.map(_.getStatusCode).toSet
    
    val aggregatedStatus = List(StatusCode.CANCELLED, StatusCode.ERROR,
        StatusCode.OK).find(distinctStatuses.contains)
    
    val mergedResult = new CompileResult(aggregatedStatus.getOrElse(StatusCode.OK))
    results.headOption.foreach { res =>
      //TODO think of a better way to merge those fields
      mergedResult.setOriginId(res.getOriginId)
      mergedResult.setDataKind(res.getDataKind)
      mergedResult.setData(res.getData)
    }

    mergedResult
  }

  private def merge(results: Iterable[ScalacOptionsResult]): ScalacOptionsResult =
    new ScalacOptionsResult(results.flatMap(_.getItems.asScala).toList.asJava)

  private def merge(exitCodes: Iterable[Option[Int]]): Option[Int] = {
    val allCodes = exitCodes.flatten
    if (allCodes.isEmpty) None else allCodes.find(_ != 0).orElse(Some(0))
  }
}
