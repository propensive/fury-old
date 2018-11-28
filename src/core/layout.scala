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

import gastronomy._

case class Layout(home: Path, pwd: Path) {
  lazy val furyDir: Path = pwd / ".fury"
  lazy val bloopDir: Path = pwd / ".bloop"
  lazy val layersDir: Path = furyDir / "layers"
  lazy val classesDir: Path = furyDir / "classes"
  lazy val runLogDir: Path = furyDir / "log"
  lazy val reposDir: Path = furyDir / "repos"
  lazy val refsDir: Path = furyDir / "refs"
  lazy val tmpDir: Path = furyDir / "tmp"
  lazy val errorLogfile: Path = pwd / ".fury.log"
  lazy val userConfig: Path = home / ".fury.conf"
  lazy val logFile: Path = furyDir / "fury.log"

  def bloopConfig(artifact: Artifact): Path = bloopDir / s"${artifact.hash.encoded[Base64Url]}.json"
 
  lazy val furyConfig: Path = pwd / "layer.fury"
  
  def layerDir(layerId: String): Path = layersDir / layerId
  
  def layerFile(layerId: String): Path = layerDir(layerId) / "layer.fury"

  def outputDir(ref: ModuleRef, create: Boolean): Path = {
    val path = classesDir / ref.projectId.key / ref.moduleId.key / "output"
    if(create) path.mkdir()
    path
  }

  def runLogFile(artifact: Artifact, create: Boolean): Path = {
    if(create) runLogDir.mkdir()
    runLogDir / s"${artifact.hash.encoded[Base64Url]}.log"
  }

  def classesDir(artifact: Artifact, create: Boolean): Path = {
    val path = classesDir / artifact.hash.encoded[Base64Url]
    if(create) path.mkdir()
    path
  }

  def manifestFile(ref: ModuleRef): Path =
    outputDir(ref, true) / s"${ref.projectId.key}-${ref.moduleId.key}.mf"
}
