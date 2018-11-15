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
import eucalyptus._

case class Layout(home: Path, pwd: Path) {
  lazy val furyDir: Path = pwd / ".fury"
  lazy val bloopDir: Path = pwd / ".bloop"
  lazy val workspacesDir: Path = furyDir / "workspaces"
  lazy val classesDir: Path = furyDir / "classes"
  lazy val runLogDir: Path = furyDir / "log"
  lazy val reposDir: Path = furyDir / "repos"
  lazy val refsDir: Path = furyDir / "refs"
  lazy val tmpDir: Path = furyDir / "tmp"
  lazy val errorLogfile: Path = pwd / ".fury.log"
  lazy val userConfig: Path = home / ".fury.conf"
  lazy val logFile: Path = furyDir / "fury.log"

  def bloopConfig(artifact: Artifact): Path = bloopDir / s"${artifact.encoded}.json"
 
  lazy val furyConfig: Path = pwd / "workspace.fury"
  lazy val signedConfig: Path = pwd / "workspace.fury.sig"
  
  def workspaceDir(workspaceId: String): Path = workspacesDir / workspaceId
  
  def workspaceFile(workspaceId: String): Path = workspaceDir(workspaceId) / "workspace.fury"

  def outputDir(artifact: Artifact, create: Boolean): Path = {
    val path = classesDir / artifact.project.id.key / artifact.module.id.key / "output"
    if(create) path.mkdir()
    path
  }

  def runLogFile(artifact: Artifact, create: Boolean): Path = {
    val path = runLogDir / artifact.project.id.key
    if(create) path.mkdir()
    path / s"${artifact.module.id.key}.log"
  }

  def classesDir(artifact: Artifact, create: Boolean): Path = {
    val path = classesDir / artifact.project.id.key / artifact.module.id.key / "classes"
    if(create) path.mkdir()
    path
  }

  def manifestFile(artifact: Artifact): Path =
    outputDir(artifact, true) / s"${artifact.encoded}.mf"
}
