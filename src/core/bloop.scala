/*
  Fury, version 0.2.2. Copyright 2019 Jon Pretty, Propensive Ltd.

  The primary distribution site is: https://propensive.com/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required  by applicable  law or  agreed to  in writing,  software  distributed  under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
  express  or  implied.  See  the  License for  the specific  language  governing  permissions and
  limitations under the License.
 */
package fury

import java.net._

import fury.io._
import fury.error._
import gastronomy._
import guillotine._
import mercator._

import scala.util._

object Bloop {

  private[this] var bloopServer: Option[Running] = None

  private[this] def testServer(): Outcome[Unit] =
    Success(new Socket("localhost", 8212).close().unit)

  def server(shell: Shell, io: Io): Outcome[Unit] = synchronized {
    try {
      testServer()
      Success(())
    } catch {
      case e: ConnectException =>
        bloopServer.foreach(_.destroy())
        val io2     = io.print("Starting bloop compile server")
        val running = shell.bloop.startServer()

        def checkStarted(): Unit =
          try {
            Thread.sleep(150)
            if (!testServer().isSuccess) {
              io.print(".")
              checkStarted()
            }
          } catch {
            case e: Exception =>
              io.print(".")
              checkStarted()
          }

        io.println("done")

        try {
          bloopServer = Some(running)
          Success(())
        } catch {
          case e: ConnectException =>
            bloopServer = None
            Failure(InitFailure())
        }
    }
  }

  def generateFiles(
      artifacts: Iterable[Artifact],
      universe: Universe
    )(implicit layout: Layout,
      env: Environment,
      shell: Shell
    ): Outcome[Iterable[Path]] =
    new CollOps(artifacts.map { artifact =>
      for {
        path       <- layout.bloopConfig(artifact).mkParents()
        jsonString <- makeConfig(artifact, universe)
        _          <- ~(if (!path.exists) path.writeSync(jsonString))
      } yield List(path)
    }).sequence.map(_.flatten)

  private def makeConfig(
      artifact: Artifact,
      universe: Universe
    )(implicit layout: Layout,
      shell: Shell
    ): Outcome[String] =
    for {
      deps      <- universe.dependencies(artifact.ref)
      _         = artifact.writePlugin()
      compiler  = artifact.compiler
      classpath <- universe.classpath(artifact.ref)
      compilerClasspath <- compiler.map { c =>
                            universe.classpath(c.ref)
                          }.getOrElse(Success(Set()))
      params <- universe.allParams(artifact.ref)
    } yield
      json(
          name = artifact.hash.encoded[Base64Url],
          scalacOptions = params,
          // FIXME: Don't hardcode this value
          bloopSpec = compiler
            .flatMap(_.bloopSpec)
            .getOrElse(BloopSpec("org.scala-lang", "scala-compiler", "2.12.7")),
          dependencies = deps.map(_.hash.encoded[Base64Url]).to[List],
          fork = false,
          classesDir = str"${layout.classesDir(artifact).value}",
          outDir = str"${layout.outputDir(artifact).value}",
          classpath = classpath.map(_.value).to[List].distinct,
          baseDirectory = layout.pwd.value,
          javaOptions = Nil,
          allScalaJars = compilerClasspath.map(_.value).to[List],
          sourceDirectories = artifact.sourcePaths.map(_.value),
          javacOptions = Nil,
          main = artifact.main
      )

  private def json(
      name: String,
      scalacOptions: List[String],
      bloopSpec: BloopSpec,
      dependencies: List[String],
      fork: Boolean,
      classesDir: String,
      outDir: String,
      classpath: List[String],
      baseDirectory: String,
      javaOptions: List[String],
      allScalaJars: List[String],
      sourceDirectories: List[String],
      javacOptions: List[String],
      main: Option[String]
    ): String =
    JsonObject(
        "version" -> JsonString("1.0.0"),
        "project" -> JsonObject(
            "name"         -> JsonString(name),
            "directory"    -> JsonString(baseDirectory),
            "sources"      -> JsonArray(sourceDirectories.map(JsonString(_)): _*),
            "dependencies" -> JsonArray(),
            "classpath"    -> JsonArray(((classpath ++ allScalaJars).map(JsonString(_))): _*),
            "out"          -> JsonString(outDir),
            "classesDir"   -> JsonString(classesDir),
            "scala" -> JsonObject(
                "organization" -> JsonString(bloopSpec.org),
                "name"         -> JsonString(bloopSpec.name),
                "version"      -> JsonString(bloopSpec.version),
                "options"      -> JsonArray(scalacOptions.map(JsonString(_)): _*),
                "jars"         -> JsonArray(allScalaJars.map(JsonString(_)): _*)
            ),
            "java" -> JsonObject(
                "options" -> JsonArray(javaOptions.map(JsonString(_)): _*)
            ),
            "test" -> JsonObject(
                "frameworks" -> JsonArray(),
                "options" -> JsonObject(
                    "excludes"  -> JsonArray(),
                    "arguments" -> JsonArray()
                )
            ),
            "platform" -> JsonObject(
                "name" -> JsonString("jvm"),
                "config" -> JsonObject(
                    "home"    -> JsonString(""),
                    "options" -> JsonArray()
                ),
                "mainClass" -> JsonArray(main.to[List].map(JsonString(_)): _*)
            ),
            "resolution" -> JsonObject(
                "modules" -> JsonArray()
            )
        )
    ).serialize
}
