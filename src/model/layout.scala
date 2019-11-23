/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.7.5. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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

import fury._, io._, strings._, ogdl._

import gastronomy._
import guillotine._
import java.util.{List => _, _}
import java.text._

import scala.util.Try

import language.higherKinds

object Layout {
  final val dateFormat = new SimpleDateFormat("yyyy-MM-dd-HH:mm:ss.SSS")
  
  def find(home: Path, pwd: Path, env: Environment): Try[Layout] = findBase(pwd).map(Layout(home, pwd, env, _))

  private def findBase(dir: Path): Try[Path] = {
    import java.nio.file.Files.{getFileStore, isRegularFile}
    val here       = dir.javaPath
    val fileSystem = getFileStore(here)
    val parents = Stream.iterate(here.toAbsolutePath)(_.getParent)
    val parentsWithinFs = parents.takeWhile(Option(_).exists(getFileStore(_) == fileSystem))
    val optParent = parentsWithinFs.find { path => isRegularFile(path.resolve(".focus.fury")) }.map(Path(_))
    
    optParent.ascribe(UnspecifiedProject())
  }
}

object Xdg {
  private case class Var(name: String) {
    private val variable = Option(System.getenv(s"XDG_$name"))
    def path: Option[Path] = variable.map(Path(_))
    def paths: Option[List[Path]] = variable.map(_.split(":").to[List].map(Path(_)))
  }
  
  val home: Path = Option(System.getenv("HOME")).map(Path(_)).getOrElse(Path("/"))
  val cacheHome: Path = Var("CACHE_HOME").path.getOrElse(home / ".cache")
  val dataHome: Path = Var("DATA_HOME").path.getOrElse(home / ".local" / "share")
  val dataDirs: List[Path] = Var("DATA_DIRS").paths.getOrElse(List(Path("/usr/local/share"), Path("/usr/share")))
  val configHome: Path = Var("CONFIG_HOME").path.getOrElse(home / ".config")
  val configDirs: List[Path] = Var("CONFIG_DIRS").paths.getOrElse(List(Path("/etc/xdg")))
  val runtimeDir: Path = Var("RUNTIME_DIR").path.getOrElse(Path("/tmp"))

  def findData(filename: Path): Option[Path] = (dataHome :: dataDirs).map(filename in _).find(_.exists)
  def findConfig(filename: Path): Option[Path] = (configHome :: configDirs).map(filename in _).find(_.exists)

  def data(filename: Path): Path = (filename in dataHome).extantParents()
  def config(filename: Path): Path = (filename in configHome).extantParents()
  def cache(filename: Path): Path = (filename in cacheHome).extantParents()
}

object Installation {
  val userConfig: Path = Xdg.config(Path("fury/config.fury"))
  val aliasesPath: Path = Xdg.config(Path("fury/aliases"))
  val layersPath: Path = Xdg.data(Path("fury/layers"))
  val policyFile: Path = Xdg.config(Path("fury/policy.fury"))
  val srcsDir: Path = Xdg.cache(Path("fury/sources"))
  val reposDir: Path = Xdg.cache(Path("fury/repos"))
  val binsDir: Path = Xdg.cache(Path("fury/bins"))
  val logsDir: Path = Xdg.cache(Path("fury/logs"))
  val upgradeDir: Path = Xdg.cache(Path("fury/upgrade"))
  val policyDir: Path = Xdg.cache(Path("fury/policies"))

  def tmpDir[T](fn: Path => T): T = tmpFile { path =>
    path.mkdir()
    fn(path)
  }

  def tmpFile[T](fn: Path => T): T = {
    val file = Xdg.runtimeDir / java.util.UUID.randomUUID().toString
    file.mkParents()
    val result = fn(file)
    file.delete()
    result
  }

  def config(): Config = Ogdl.read[Config](userConfig, identity(_)).toOption.getOrElse(Config())
}

case class Layout(home: Path, pwd: Path, env: Environment, baseDir: Path) {
  private[this] val nowString: String = Layout.dateFormat.format(new Date())
  private[this] val uniqueId: String = java.util.UUID.randomUUID().toString
  
  lazy val furyDir: Path = (baseDir / ".fury").extant()
  lazy val bspDir: Path = (baseDir / ".bsp").extant()
  lazy val bloopDir: Path = (baseDir / ".bloop").extant()
  lazy val focusFile: Path = baseDir / ".focus.fury"
  lazy val furyConfig: Path = baseDir / "layer.fury"

  lazy val classesDir: Path = (furyDir / "classes").extant()
  lazy val benchmarksDir: Path = (furyDir / "benchmarks").extant()
  lazy val analysisDir: Path = (furyDir / "analysis").extant()
  lazy val resourcesDir: Path = (furyDir / "resources").extant()
  lazy val basesDir: Path = (furyDir / "bases").extant()
  lazy val workDir: Path = (furyDir / "work").extant()
  lazy val sharedDir: Path = (furyDir / "build" / uniqueId).extant()
  lazy val logsDir: Path = (furyDir / "logs").extant()
  
  lazy val errorLogfile: Path = logsDir.extant() / s"$nowString-$uniqueId.log"
  lazy val messagesLogfile: Path = logsDir.extant() / s"$nowString-$uniqueId.bsp-messages.log"
  lazy val traceLogfile: Path = logsDir.extant() / s"$nowString-$uniqueId.bsp-trace.log"
  
  def bloopConfig(targetId: TargetId): Path = bloopDir.extant() / str"${targetId.key}.json"
  def outputDir(targetId: TargetId): Path = (analysisDir / targetId.key).extant()
  def workDir(targetId: TargetId): Path = (workDir / targetId.key).extant()
  def benchmarksDir(targetId: TargetId): Path = (benchmarksDir / targetId.key).extant()
  def classesDir(targetId: TargetId): Path = (classesDir / targetId.key).extant()
  def resourcesDir(targetId: TargetId): Path = (resourcesDir / targetId.key).extant()
}
