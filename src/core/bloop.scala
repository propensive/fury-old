/*
  Fury, version 0.4.0. Copyright 2018-19 Jon Pretty, Propensive Ltd.

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

import gastronomy._
import guillotine._
import mercator._

import scala.util._
import scala.concurrent.duration._

object Bloop {

  private[this] var bloopServer: Option[Running] = None

  private[this] def testServer(): Try[Unit] =
    Success(new Socket("localhost", 8212).close().unit)

  def server(shell: Shell, io: Io): Try[Unit] = synchronized {
    try {
      testServer()
      Success(())
    } catch {
      case e: ConnectException =>
        bloopServer.foreach(_.destroy())
        val io2     = io.print("Starting bloop compile server...")
        val running = shell.bloop.startServer()

        def checkStarted(n: Duration): Boolean = {
          val interval = 150.millis
          if (n < interval) {
            false
          } else {
            try {
              if (!testServer().isSuccess) {
                io.print(".")
                Thread.sleep((interval / 1.millisecond).toInt)
                checkStarted(n - interval)
              } else {
                true
              }
            } catch {
              case e: Exception =>
                io.print(".")
                Thread.sleep((interval / 1.millisecond).toInt)
                checkStarted(n - interval)
            }
          }
        }
        if (checkStarted(5 seconds)) {
          io.println("done")
          try {
            bloopServer = Some(running)
            Success(())
          } catch {
            case e: ConnectException =>
              bloopServer = None
              Failure(InitFailure())
          }
        } else {
          Failure(InitFailure())
        }
    }
  }

  def generateFiles(io: Io, compilation: Compilation, layout: Layout): Try[Iterable[Path]] =
    new CollOps(compilation.artifacts.values.map { artifact =>
      for {
        path       <- layout.bloopConfig(compilation.hash(artifact.ref)).mkParents()
        jsonString <- makeConfig(io, artifact, compilation, layout)
        _          <- ~path.writeSync(jsonString)
      } yield List(path)
    }).sequence.map(_.flatten)

  private def makeConfig(
      io: Io,
      artifact: Artifact,
      compilation: Compilation,
      layout: Layout
    ): Try[String] =
    for {
      _         <- ~compilation.writePlugin(artifact.ref, layout)
      classpath <- ~compilation.classpath(artifact.ref, layout)
      compilerClasspath <- ~artifact.compiler.map { c =>
                            compilation.classpath(c.ref, layout)
                          }.getOrElse(Set())
      bloopSpec = artifact.compiler
        .flatMap(_.bloopSpec)
        .getOrElse(BloopSpec("org.scala-lang", "scala-compiler", "2.12.7"))
      params <- ~compilation.allParams(io, artifact.ref, layout)
    } yield
      Json(
          version = "1.0.0",
          project = Json(
              name = compilation.hash(artifact.ref).encoded[Base64Url],
              directory = layout.pwd.value,
              sources = artifact.sourcePaths.map(_.value),
              dependencies = compilation
                .allDependenciesGraph(artifact.ref)
                .map(compilation.hash(_).encoded[Base64Url]),
              classpath = (classpath ++ compilerClasspath).map(_.value),
              out = str"${layout.outputDir(compilation.hash(artifact.ref)).value}",
              classesDir = str"${layout.classesDir(compilation.hash(artifact.ref)).value}",
              scala = Json(
                  organization = bloopSpec.org,
                  name = bloopSpec.name,
                  version = bloopSpec.version,
                  options = params,
                  jars = compilerClasspath.map(_.value)
              ),
              java = Json(options = Nil),
              test = Json(frameworks = Nil, options = Json(excludes = Nil, arguments = Nil)),
              platform = Json(
                  name = "jvm",
                  config = Json(home = "", options = Nil),
                  mainClass = artifact.main.to[List]
              ),
              resolution = Json(modules = Nil)
          )
      ).serialize

}
