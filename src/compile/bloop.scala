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

import fury.io._, fury.text._, fury.model._

import mercator._
import euphemism._
import jovian._

import scala.util._

object Bloop {
  def clean(layout: Layout): Try[Boolean] =
    layout.bloopDir.ifExists().fold(Try(false))(_.findChildren(_.endsWith(".json")).map(_.delete()
        ).sequence.map(_.contains(true)))

  def generateFiles(build: Build)(implicit log: Log): Try[Iterable[Path]] =
    new CollOps(build.targets.values.map { target =>
      for {
        path       <- build.layout.bloopConfig(target.ref).mkParents()
        jsonString <- makeConfig(build)(target)
        _          <- ~path.writeSync(jsonString)
      } yield List(path)
    }).sequence.map(_.flatten)

  private def makeConfig(build: Build)(target: build.Target)(implicit log: Log): Try[String] = {

    build.writePlugin(target.ref)
    val classpath = target.classpath
    
    val optDefs = target.aggregatedOptDefs.getOrElse(Set()).filter(_.compiler ==
        target.module.compiler).map(_.value)
    
    val opts: List[String] =
      target.aggregatedOpts.map(_.to[List].filter(_.compiler ==
          target.module.compiler).flatMap(_.value.transform(optDefs))).getOrElse(Nil)
    
    val compilerClasspath = build(target.module.compiler).map { _ => target.bootClasspath }
    
    val compilerOpt: Option[Json] = build(target.module.compiler).toOption.flatMap { compiler =>
      val spec: Option[BloopSpec] = compiler.map(_.module.kind.as[Compiler].fold {
        BloopSpec("org.scala-lang", "scala-compiler", "2.12.8")
      } (_.spec))

      spec.map { spec => Json.of(
        organization = spec.org,
        name = spec.name,
        version = spec.version,
        options = opts,
        jars = compilerClasspath.get.map(_.value)
      ) }
    }

    val result = Json.of(
      version = "1.0.0",
      project = Json.of(
        scala = compilerOpt,
        name = target.ref.urlSafe,
        directory = build.layout.workDir(target.ref).value,
        sources = target.sourcePaths.map(_.value),
        dependencies = build.graph.dependencies(target.ref).map(_.ref.urlSafe),
        classpath = (classpath ++ compilerClasspath.getOrElse(Set.empty)).map(_.value),
        out = str"${build.layout.outputDir(target.ref).value}",
        classesDir = str"${build.layout.classesDir(target.ref).value}",
        java = Json.of(options = Nil),
        test = Json.of(frameworks = Nil, options = Json.of(excludes = Nil, arguments = Nil)),
        jvmPlatform = Json.of(
          name = "jvm",
          config = Json.of(home = "", options = Nil),
          mainClass = Nil
        ),
        resolution = Json.of(modules = Nil)
      )
    )
    Success(result.toString)
  }
}
