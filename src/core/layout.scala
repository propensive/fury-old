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

import java.nio.file.{Files, Path => JPath}

import gastronomy._
import guillotine._
import java.util._
import java.text._

object Layout {
  final val dateFormat = new SimpleDateFormat("yyyy-MM-dd-HH:mm:ss.SSS")
}

case class Layout(home: Path, pwd: Path, env: Environment) {

  private val nowString: String = Layout.dateFormat.format(new Date())

  private lazy val projectBaseDir = Path(findProjectBaseDir(pwd.javaPath).toString)

  private[this] val uniqueId: String = java.util.UUID.randomUUID().toString
  private[this] val userDir          = (home / ".furyrc").extant()

  lazy val furyDir: Path       = (pwd / ".fury").extant()
  lazy val historyDir: Path    = (furyDir / "history").extant()
  lazy val bloopDir: Path      = (furyDir / "bloop").extant()
  lazy val classesDir: Path    = (furyDir / "classes").extant()
  lazy val benchmarksDir: Path = (furyDir / "benchmarks").extant()
  lazy val analysisDir: Path   = (furyDir / "analysis").extant()
  lazy val resourcesDir: Path  = (furyDir / "resources").extant()
  lazy val reposDir: Path      = (userDir / "repos").extant()
  lazy val srcsDir: Path       = (userDir / "sources").extant()
  lazy val logsDir: Path       = (furyDir / "logs").extant()
  lazy val sharedDir: Path     = (furyDir / "build" / uniqueId).extant()
  lazy val errorLogfile: Path  = logsDir.extant() / s"$nowString-$uniqueId.log"
  lazy val userConfig: Path    = userDir / "config.fury"
  lazy val aliasesPath: Path   = userDir / "aliases"

  def bloopConfig(digest: Digest): Path =
    bloopDir.extant() / s"${digest.encoded[Base64Url]}.json"

  lazy val furyConfig: Path = projectBaseDir / "layer.fury"

  def outputDir(digest: Digest): Path =
    (analysisDir / digest.encoded[Base64Url]).extant()

  def benchmarksDir(digest: Digest): Path =
    (benchmarksDir / digest.encoded[Base64Url]).extant()

  def classesDir(digest: Digest): Path =
    (classesDir / digest.encoded[Base64Url]).extant()

  def resourcesDir(digest: Digest): Path =
    (resourcesDir / digest.encoded[Base64Url]).extant()

  def manifestFile(digest: Digest): Path =
    resourcesDir(digest) / "manifest.mf"

  val shell = Shell(env)

  private def findProjectBaseDir(here: JPath): JPath = {
    import Files.{getFileStore, isRegularFile}
    val fileSystem = getFileStore(here)
    Stream
      .iterate(here.toAbsolutePath)(_.getParent)
      .takeWhile { path =>
        Option(path).exists(getFileStore(_) == fileSystem)
      }
      .find { path =>
        isRegularFile(path.resolve("layer.fury"))
      }
      .ascribe(UnspecifiedProject())
      .get
  }

}
