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
package fury.core

import fury.io._

import gastronomy._
import guillotine._
import java.util._
import java.text._

import scala.util.Try

object Layout {
  final val dateFormat = new SimpleDateFormat("yyyy-MM-dd-HH:mm:ss.SSS")

  private def findBase(dir: Path): Try[Path] = {
    import java.nio.file.Files.{getFileStore, isRegularFile}
    val here       = dir.javaPath
    val fileSystem = getFileStore(here)
    Stream
      .iterate(here.toAbsolutePath)(_.getParent)
      .takeWhile {
        Option(_).exists(getFileStore(_) == fileSystem)
      }
      .find { path =>
        isRegularFile(path.resolve("layer.fury"))
      }
      .map(Path(_))
      .ascribe(UnspecifiedProject())
  }

  def find(home: Path, pwd: Path, env: Environment): Try[Layout] =
    findBase(pwd).map { Layout(home, pwd, env, _) }
}

case class Layout(home: Path, pwd: Path, env: Environment, base: Path) {

  private val nowString: String = Layout.dateFormat.format(new Date())

  private[this] val uniqueId: String = java.util.UUID.randomUUID().toString
  private[this] val userDir          = (home / ".furyrc").extant()

  lazy val furyDir: Path       = (base / ".fury").extant()
  lazy val bspDir: Path        = (base / ".bsp").extant()
  lazy val historyDir: Path    = (furyDir / "history").extant()
  lazy val bloopDir: Path      = (furyDir / ".bloop").extant()
  lazy val classesDir: Path    = (furyDir / "classes").extant()
  lazy val benchmarksDir: Path = (furyDir / "benchmarks").extant()
  lazy val analysisDir: Path   = (furyDir / "analysis").extant()
  lazy val resourcesDir: Path  = (furyDir / "resources").extant()
  lazy val reposDir: Path      = (userDir / "repos").extant()
  lazy val layersDir: Path     = (userDir / "layers").extant()
  lazy val srcsDir: Path       = (userDir / "sources").extant()
  lazy val logsDir: Path       = (furyDir / "logs").extant()
  lazy val workDir: Path       = (furyDir / "work").extant()
  lazy val sharedDir: Path     = (furyDir / "build" / uniqueId).extant()
  lazy val tmpLayer: Path      = workDir / s"$uniqueId.tar.gz"
  lazy val errorLogfile: Path  = logsDir.extant() / s"$nowString-$uniqueId.log"
  lazy val userConfig: Path    = userDir / "config.fury"
  lazy val aliasesPath: Path   = userDir / "aliases"

  def bloopConfig(digest: Digest): Path =
    bloopDir.extant() / s"${digest.encoded[Base64Url]}.json"

  lazy val furyConfig: Path = base / "layer.fury"

  def outputDir(digest: Digest): Path =
    (analysisDir / digest.encoded[Base64Url]).extant()

  def workDir(digest: Digest): Path =
    (workDir / digest.encoded[Base64Url]).extant()

  def benchmarksDir(digest: Digest): Path =
    (benchmarksDir / digest.encoded[Base64Url]).extant()

  def classesDir(digest: Digest): Path =
    (classesDir / digest.encoded[Base64Url]).extant()

  def resourcesDir(digest: Digest): Path =
    (resourcesDir / digest.encoded[Base64Url]).extant()

  def manifestFile(digest: Digest): Path =
    resourcesDir(digest) / "manifest.mf"

  val shell = Shell(env)

}
