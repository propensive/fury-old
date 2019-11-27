/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.7.13. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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

import fury.io._, fury.strings._, fury.model._

import java.net._

import gastronomy._
import guillotine._
import mercator._
import euphemism._

import scala.concurrent._, duration._, ExecutionContext.Implicits.global
import scala.util._
import scala.util.control.NonFatal

object Bloop {

  def clean(layout: Layout): Try[Boolean] =
    layout.bloopDir.findChildren(_.endsWith(".json")).map(_.delete()).sequence.map(_.contains(true))

  def generateFiles(compilation: Compilation, layout: Layout)(implicit log: Log): Try[Iterable[Path]] =
    new CollOps(compilation.targets.values.map { target =>
      for {
        path       <- layout.bloopConfig(target.id).mkParents()
        jsonString <- makeConfig(target, compilation, layout)
        _          <- ~path.writeSync(jsonString)
      } yield List(path)
    }).sequence.map(_.flatten)

  private def makeConfig(target: Target, compilation: Compilation, layout: Layout)(implicit log: Log): Try[String] = for {
    _         <- ~compilation.writePlugin(target.ref, layout)
    classpath <- ~compilation.classpath(target.ref, layout)
    compilerClasspath <- ~target.compiler.map(_.ref).map(compilation.classpath(_, layout)).getOrElse(classpath)
    bloopSpec = target.compiler
      .map(_.bloopSpec.getOrElse(BloopSpec("org.scala-lang", "scala-compiler", "2.12.8")))
    params <- ~compilation.allParams(target.ref, layout)
    compilerOpt = (bloopSpec.map { spec =>
                    Json.of(
                      organization = spec.org,
                      name = spec.name,
                      version = spec.version,
                      options = params,
                      jars = compilerClasspath.map(_.value)
                    )
                  })
  } yield Json.of(
    version = "1.0.0",
    project = Json.of(
      scala = compilerOpt,
      name = target.id.key,
      directory = layout.workDir(target.id).value,
      sources = target.sourcePaths.map(_.value),
      dependencies = compilation.graph(target.id).map(_.key),
      classpath = (classpath ++ compilerClasspath).map(_.value),
      out = str"${layout.outputDir(target.id).value}",
      classesDir = str"${layout.classesDir(target.id).value}",
      java = Json.of(options = Nil),
      test = Json.of(frameworks = Nil, options = Json.of(excludes = Nil, arguments = Nil)),
      jvmPlatform = Json.of(
        name = "jvm",
        config = Json.of(home = "", options = Nil),
        mainClass = target.main.to[List]
      ),
      resolution = Json.of(modules = Nil)
    )
  ).toString
}
