/*

    Fury, version 0.15.1. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

import fury.model._

import scala.util._

case class CleanCli(cli: Cli)(implicit log: Log) {
  def cleanAll: Try[ExitStatus] = for {
    _ <- cleanBloop
    _ <- cleanClasses
    _ <- cleanLogs
    _ <- cleanRepos
    _ <- cleanSources
  } yield Done

  def cleanBloop: Try[ExitStatus] = for {
    layout <- cli.layout
    _      <- layout.bloopDir.delete()
    _      <- layout.analysisDir.delete()
  } yield Done

  def cleanClasses: Try[ExitStatus] = for {
    layout <- cli.layout
    _      <- layout.classesDir.delete()
  } yield Done

  def cleanLogs: Try[ExitStatus] = for {
    layout <- cli.layout
    _      <- layout.logsDir.delete()
  } yield Done
  
  def cleanRepos: Try[ExitStatus] = for {
    _ <- Installation.reposDir.setWritable()
    _ <- Installation.reposDir.delete()
  } yield Done
  
  def cleanSources: Try[ExitStatus] = for {
    _ <- Installation.srcsDir.setWritable()
    _ <- Installation.srcsDir.delete()
  } yield Done
}
