/*

    Fury, version 0.15.1. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

import fury.io._, fury.strings._, fury.model._

import mercator._
import euphemism._

import scala.util._

object Bloop {
  def clean(layout: Layout): Try[Boolean] =
    layout.bloopDir.ifExists().fold(Try(false))(_.findChildren(_.endsWith(".json")).map(_.delete()
        ).sequence.map(_.contains(true)))

  def generateFiles(compilation: Compilation, layout: Layout)(implicit log: Log): Try[Iterable[Path]] =
    new CollOps(compilation.targets.values.map { target =>
      for {
        path       <- layout.bloopConfig(target.id).mkParents()
        jsonString <- makeConfig(target, compilation, layout)
        _          <- ~path.writeSync(jsonString)
      } yield List(path)
    }).sequence.map(_.flatten)

  private def makeConfig(target: Target, compilation: Compilation, layout: Layout)
                        (implicit log: Log)
                        : Try[String] = {

    compilation.writePlugin(target.ref, layout)
    val classpath = compilation.classpath(target.ref, layout)
    val compiler = target.compiler.fold(ModuleRef.JavaRef)(_.ref)
    
    val optDefs = compilation.aggregatedOptDefs(target.ref).getOrElse(Set()).filter(_.compiler ==
        compiler).map(_.value)
    
    val opts: List[String] =
      compilation.aggregatedOpts(target.ref, layout).map(_.to[List].filter(_.compiler == compiler).flatMap(
          _.value.transform(optDefs))).getOrElse(Nil)
    
      val compilerClasspath = target.compiler.map { _ => compilation.bootClasspath(target.ref, layout) }
    val compilerOpt = target.compiler.map { compilerTarget =>
      val spec = compilerTarget.bloopSpec.getOrElse(BloopSpec("org.scala-lang", "scala-compiler", "2.12.8"))
      Json.of(
        organization = spec.org,
        name = spec.name,
        version = spec.version,
        options = opts,
        jars = compilerClasspath.get.map(_.value)
      )
    }

    val result = Json.of(
      version = "1.0.0",
      project = Json.of(
        scala = compilerOpt,
        name = target.id.key,
        directory = layout.workDir(target.id).value,
        sources = target.sourcePaths.map(_.value),
        dependencies = compilation.graph.dependencies(target.id).map(_.key),
        classpath = (classpath ++ compilerClasspath.getOrElse(Set.empty)).map(_.value),
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
    )
    Success(result.toString)
  }
}
