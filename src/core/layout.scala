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

import fury.io.Path
import gastronomy._
import guillotine._

case class Layout(home: Path, pwd: Path, env: Environment) {

  private[this] val uniqueId: String = java.util.UUID.randomUUID().toString
  private[this] val userDir          = (home / ".furyrc").extant()

  lazy val furyDir: Path      = (pwd / ".fury").extant()
  lazy val historyDir: Path   = (furyDir / "history").extant()
  lazy val bloopDir: Path     = (furyDir / "bloop").extant()
  lazy val classesDir: Path   = (furyDir / "classes").extant()
  lazy val analysisDir: Path  = (furyDir / "analysis").extant()
  lazy val resourcesDir: Path = (furyDir / "resources").extant()
  lazy val reposDir: Path     = (furyDir / "repos").extant()
  lazy val srcsDir: Path      = (furyDir / "sources").extant()
  lazy val logsDir: Path      = (furyDir / "logs").extant()
  lazy val sharedDir: Path    = (furyDir / "build" / uniqueId).extant()
  lazy val errorLogfile: Path = logsDir.extant() / s"$uniqueId.log"
  lazy val userConfig: Path   = userDir / "config.fury"
  lazy val aliasesPath: Path  = userDir / "aliases"

  def bloopConfig(digest: Digest): Path =
    bloopDir.extant() / s"${digest.encoded[Base64Url]}.json"

  lazy val furyConfig: Path = pwd / "layer.fury"

  def outputDir(digest: Digest): Path =
    (analysisDir / digest.encoded[Base64Url]).extant()

  def classesDir(digest: Digest): Path =
    (classesDir / digest.encoded[Base64Url]).extant()

  def manifestFile(digest: Digest): Path =
    (resourcesDir / digest.encoded[Base64Url]).extant() / "manifest.mf"

  val shell = Shell(env)
}
