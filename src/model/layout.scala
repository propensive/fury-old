/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.6.6. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
   ║                                                                                                           ║
   ║ The primary distribution site is: https://propensive.com/                                                 ║
   ║                                                                                                           ║
   ║ Licensed under  the Apache License,  Version 2.0 (the  "License"); you  may not use  this file  except in ║
   ║ compliance with the License. You may obtain a copy of the License at                                      ║
   ║                                                                                                           ║
   ║     http://www.apache.org/licenses/LICENSE-2.0                                                            ║
   ║                                                                                                           ║
   ║ Unless required  by applicable law  or agreed to in  writing, software  distributed under the  License is ║
   ║ distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. ║
   ║ See the License for the specific language governing permissions and limitations under the License.        ║
   ╚═══════════════════════════════════════════════════════════════════════════════════════════════════════════╝
*/
package fury.model

import fury._, io._, strings._

import gastronomy._
import guillotine._
import java.util._
import java.text._

import scala.util.Try

object Layout {
  final val dateFormat = new SimpleDateFormat("yyyy-MM-dd-HH:mm:ss.SSS")
  
  def find(home: Path, pwd: Path, env: Environment): Try[Layout] = findBase(pwd).map(Layout(home, pwd, env, _))

  private def findBase(dir: Path): Try[Path] = {
    import java.nio.file.Files.{getFileStore, isRegularFile}
    val here       = dir.javaPath
    val fileSystem = getFileStore(here)
    val parents = Stream.iterate(here.toAbsolutePath)(_.getParent)
    val parentsWithinFs = parents.takeWhile(Option(_).exists(getFileStore(_) == fileSystem))
    val optParent = parentsWithinFs.find { path => isRegularFile(path.resolve("layer.fury")) }.map(Path(_))
    
    optParent.ascribe(UnspecifiedProject())
  }
}

case class GlobalLayout(home: Path) {
  private[this] val userDir = (home / ".furyrc").extant()
  lazy val userConfig: Path = userDir / "config.fury"
  lazy val aliasesPath: Path = userDir / "aliases"
  lazy val policyFile: Path = userDir / "policy.fury"
}

case class Layout(home: Path, pwd: Path, env: Environment, base: Path) {
  private[this] val nowString: String = Layout.dateFormat.format(new Date())
  private[this] val uniqueId: String = java.util.UUID.randomUUID().toString
  private[this] val userDir = (home / ".furyrc").extant()
  
  lazy val furyDir: Path = (base / ".fury").extant()
  lazy val bspDir: Path = (base / ".bsp").extant()
  lazy val historyDir: Path = (furyDir / "history").extant()
  lazy val bloopDir: Path = (furyDir / ".bloop").extant()
  lazy val classesDir: Path = (furyDir / "classes").extant()
  lazy val benchmarksDir: Path = (furyDir / "benchmarks").extant()
  lazy val analysisDir: Path = (furyDir / "analysis").extant()
  lazy val resourcesDir: Path = (furyDir / "resources").extant()
  lazy val reposDir: Path = (userDir / "repos").extant()
  lazy val srcsDir: Path = (userDir / "sources").extant()
  lazy val logsDir: Path = (furyDir / "logs").extant()
  lazy val workDir: Path = (furyDir / "work").extant()
  lazy val policyDir: Path = (furyDir / "policy").extant()
  lazy val sharedDir: Path = (furyDir / "build" / uniqueId).extant()
  lazy val errorLogfile: Path = logsDir.extant() / s"$nowString-$uniqueId.log"
  lazy val messagesLogfile: Path = logsDir.extant() / s"$nowString-$uniqueId.bsp-messages.log"
  lazy val traceLogfile: Path = logsDir.extant() / s"$nowString-$uniqueId.bsp-trace.log"
  lazy val furyConfig: Path = base / "layer.fury"
  
  def bloopConfig(targetId: TargetId): Path = bloopDir.extant() / str"${targetId.key}.json"
  def outputDir(targetId: TargetId): Path = (analysisDir / targetId.key).extant()
  def workDir(targetId: TargetId): Path = (workDir / targetId.key).extant()
  def benchmarksDir(targetId: TargetId): Path = (benchmarksDir / targetId.key).extant()
  def classesDir(targetId: TargetId): Path = (classesDir / targetId.key).extant()
  def resourcesDir(targetId: TargetId): Path = (resourcesDir / targetId.key).extant()
}
