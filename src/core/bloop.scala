/*
  Fury, version 0.1.0. Copyright 2018 Jon Pretty, Propensive Ltd.

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

import mitigation._
import guillotine._
import java.net._

object Bloop {

  private[this] var bloopServer: Option[Running] = None

  private[this] def testServer(): Result[Unit, ~] =
    Answer(new Socket("localhost", 8212).close().unit)

  def server(cli: Cli[_])(io: cli.Io)(implicit shell: Shell): Result[cli.Io, ~ | InitFailure] = synchronized {
    try {
      testServer()
      Answer(io)
    } catch {
      case e: ConnectException =>
        bloopServer.foreach(_.destroy())
        val io2 = io.print("Starting bloop compile server")
        val running = shell.bloop.startServer()
        
        def checkStarted(io: cli.Io): cli.Io = try {
          Thread.sleep(50)
          if(!testServer().successful) checkStarted(io.print("."))
          else io
        } catch { case e: Exception => checkStarted(io.print(".")) }
        
        val io3 = checkStarted(io2).println("done")

        try {
          bloopServer = Some(running)
          Answer(io3)
        } catch {
          case e: ConnectException =>
            bloopServer = None
            Result.abort(InitFailure())
        }
    }
  }


  def generateFiles(artifacts: Set[Artifact])
                   (implicit layout: Layout, env: Environment, shell: Shell)
                   : Result[Set[Path], ~ | FileWriteError | ShellFailure | FileNotFound |
                       UnknownCompiler | ItemNotFound | InvalidValue] = {
    artifacts.map { artifact => for {
      path       <- layout.bloopConfig(artifact).mkParents()
      jsonString <- makeConfig(artifact)
      _          <- path.writeSync(jsonString)
    } yield List(path) }.sequence.map(_.flatten)
  }

  private def makeConfig(artifact: Artifact)
                        (implicit layout: Layout, shell: Shell)
                        : Result[String, ~ | FileNotFound | FileWriteError | ShellFailure |
                            UnknownCompiler | ItemNotFound | InvalidValue] =
    for {
      deps                 <- artifact.dependencies
      _                     = artifact.writePlugin()
      optCompiler          <- artifact.compiler
      classpath            <- artifact.classpath()
      optCompilerClasspath <- optCompiler.map(_.classpath()).getOrElse(Answer(Nil))
      params               <- artifact.allParams
      sourceDirs           <- artifact.module.sources.to[List].map(_.path(artifact.schema)).distinct.sequence
    } yield json(
      name = artifact.encoded,
      scalacOptions = params.map(_.parameter),
      // FIXME: Don't hardcode this value
      bloopSpec = optCompiler.map(_.module.bloopSpec.get).getOrElse(BloopSpec("org.scala-lang", "scala-compiler", "2.12.7")),
      dependencies = deps.map(_.encoded),
      fork = false,
      classesDir = str"${layout.classesDir(artifact, true).value}",
      outDir = str"${layout.outputDir(artifact, true).value}",
      classpath = classpath.map(_.value),
      baseDirectory = layout.pwd.value,
      javaOptions = Nil,
      allScalaJars = optCompilerClasspath.map(_.value),
      sourceDirectories = sourceDirs.map(_.value),
      javacOptions = Nil,
      main = artifact.module.main
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
  ): String = JsonObject(
    "version" -> JsonString("1.0.0"),
    "project" -> JsonObject(
      "name" -> JsonString(name),
      "directory" -> JsonString(baseDirectory),
      "sources" -> JsonArray(sourceDirectories.map(JsonString(_)): _*),
      "dependencies" -> JsonArray(),
      "classpath" -> JsonArray(((classpath ++ allScalaJars).map(JsonString(_))): _*),
      "out" -> JsonString(outDir),
      "classesDir" -> JsonString(classesDir),
      "scala" -> JsonObject(
        "organization" -> JsonString(bloopSpec.org),
        "name" -> JsonString(bloopSpec.name),
        "version" -> JsonString(bloopSpec.version),
        "options" -> JsonArray(scalacOptions.map(JsonString(_)): _*),
        "jars" -> JsonArray(allScalaJars.map(JsonString(_)): _*),
        "setup" -> JsonObject(
          "order" -> JsonString("mixed"),
          "addLibraryToBootClasspath" -> JsonBoolean(true),
          "addCompilerToClasspath" -> JsonBoolean(false),
          "addExtraJarsToClasspath" -> JsonBoolean(false),
          "manageBootClasspath" -> JsonBoolean(true),
          "filterLibraryFromClasspath" -> JsonBoolean(true),
        ),
      ),
      "java" -> JsonObject(
        "options" -> JsonArray(javaOptions.map(JsonString(_)): _*),
      ),
      "test" -> JsonObject(
        "frameworks" -> JsonArray(),
        "options" -> JsonObject(
          "excludes" -> JsonArray(),
          "arguments" -> JsonArray(),
        ),
      ),
      "platform" -> JsonObject(
        "name" -> JsonString("jvm"),
        "config" -> JsonObject(
          "home" -> JsonString(""),
          "options" -> JsonArray(),
        ),
        "mainClass" -> JsonArray(main.to[List].map(JsonString(_)): _*),
      ),
      "resolution" -> JsonObject(
        "modules" -> JsonArray(),
      ),
    ),
  ).serialize
}
