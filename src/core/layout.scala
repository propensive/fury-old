/*
  Fury, version 0.1.2. Copyright 2018 Jon Pretty, Propensive Ltd.

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

import gastronomy._

case class Layout(home: Path, pwd: Path) {
  lazy val furyDir: Path = pwd / ".fury"
  lazy val bloopDir: Path = furyDir / "bloop"
  lazy val layersDir: Path = furyDir / "layers"
  lazy val classesDir: Path = furyDir / "classes"
  lazy val analysisDir: Path = furyDir / "analysis"
  lazy val resourcesDir: Path = furyDir / "resources"
  lazy val runLogDir: Path = furyDir / "log"
  lazy val reposDir: Path = furyDir / "repos"
  lazy val srcsDir: Path = furyDir / "sources"
  lazy val tmpDir: Path = furyDir / "tmp"
  lazy val errorLogfile: Path = pwd / ".fury.log"
  lazy val userConfig: Path = home / ".fury.conf"
  lazy val logFile: Path = furyDir / "fury.log"

  def bloopConfig(artifact: Artifact): Path = bloopDir / s"${artifact.hash.encoded[Base64Url]}.json"
 
  lazy val furyConfig: Path = pwd / "layer.fury"
  
  def layerDir(layerId: String): Path = layersDir / layerId
  
  def layerFile(layerId: String): Path = layerDir(layerId) / "layer.fury"

  def outputDir(artifact: Artifact): Path = {
    val path = analysisDir / artifact.hash.encoded[Base64Url]
    path.mkdir()
    path
  }

  def runLogFile(artifact: Artifact): Path = {
    runLogDir.mkdir()
    runLogDir / s"${artifact.hash.encoded[Base64Url]}.log"
  }

  def classesDir(artifact: Artifact): Path = {
    val path = classesDir / artifact.hash.encoded[Base64Url]
    path.mkdir()
    path
  }

  def manifestFile(artifact: Artifact): Path = {
    val path = resourcesDir / artifact.hash.encoded[Base64Url]
    path.mkdir()
    path / s"manifest.mf"
  }
}
