/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.8.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.                                         ║
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

import fury.model._, UiGraph.DiagnosticMessage, fury.io._, fury.ogdl._

import ch.epfl.scala.bsp4j.{CompileResult => BspCompileResult, _}

import scala.collection.JavaConverters._
import scala.util._

import java.net.URI


case class ProjectSpec(project: Project, repos: Map[RepoId, SourceRepo])

case class Entity(project: Project, schema: Schema) {
  def spec: ProjectSpec = {
    val repoIds = project.allRepoIds
    ProjectSpec(project, schema.repos.to[List].filter(repoIds contains _.id).map { r => (r.id, r) }.toMap)
  }
}

object Counter {
  private var count: Int = 0
  def next(): Int = {
    count += 1
    count
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
  def merge(results: Iterable[CompileResult]): CompileResult = {
    CompileResult(merge(results.map(_.bspCompileResult)), merge(results.map(_.scalacOptions)))
  }

  private def merge(results: Iterable[BspCompileResult]): BspCompileResult = {
    val distinctStatuses = results.map(_.getStatusCode).toSet
    
    val aggregatedStatus = List(StatusCode.CANCELLED, StatusCode.ERROR,
        StatusCode.OK).find(distinctStatuses.contains)
    
    val mergedResult = new BspCompileResult(aggregatedStatus.getOrElse(StatusCode.OK))
    results.headOption.foreach { res =>
      //TODO think of a better way to merge those fields
      mergedResult.setOriginId(res.getOriginId)
      mergedResult.setDataKind(res.getDataKind)
      mergedResult.setData(res.getData)
    }
    mergedResult
  }

  private def merge(results: Iterable[ScalacOptionsResult]): ScalacOptionsResult = {
    new ScalacOptionsResult(results.flatMap(_.getItems.asScala).toList.asJava)
  }
}

object ManagedConfig {
  private var config: Config =
    Ogdl.read[Config](Installation.userConfig, identity(_)).toOption.getOrElse(Config())

  def write(newConfig: Config) = synchronized {
    config = newConfig
    Ogdl.write(config, Installation.userConfig)
  }

  def apply(): Config = config
}

