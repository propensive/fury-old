/*

    Fury, version 0.18.6. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.utils

import fury.io._

import org.scalajs.ir._
import org.scalajs.logging._
import org.scalajs.linker._, interface._

import scala.concurrent._
import scala.util._

import java.net.URI

object ScalaJs {
  def link(main: Option[String], classpath: List[Path], out: Path)
          (implicit ec: ExecutionContext)
          : Try[Unit] = {
    val config = StandardConfig()
        .withSemantics(Semantics.Defaults)
        .withModuleKind(ModuleKind.NoModule)
        .withESFeatures(ESFeatures.Defaults)
        .withCheckIR(false)
        .withOptimizer(true)
        .withSourceMap(false)
        .withRelativizeSourceMapBase(None)
        .withClosureCompiler(false)
        .withPrettyPrint(false)
        .withBatchMode(true)

    val linker = StandardImpl.linker(config)

    val logger = new ScalaConsoleLogger(Level.Info)

    val sm = out.rename(_+".map")

    val output = LinkerOutput(PathOutputFile(out.javaPath))
        .withSourceMap(PathOutputFile(sm.javaPath))
        .withSourceMapURI(new URI(null, null, sm.javaPath.getFileName.toString, null))
        .withJSFileURI(new URI(null, null, out.javaPath.getFileName.toString, null))

    val cache = StandardImpl.irFileCache.newCache

    val modInit = main.map { m => List(ModuleInitializer.mainMethodWithArgs(m, "main")) }.getOrElse(Nil)

    Try(Await.result(for {
      (c, _) <- PathIRContainer.fromClasspath(classpath.map(_.javaPath))
      c      <- cache.cached(c)
      _      <- linker.link(c, modInit, output, logger)
    } yield (), duration.Duration.Inf))
  }
}