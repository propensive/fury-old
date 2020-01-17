/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.8.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.                                         ║
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
import kaleidoscope._
import guillotine._
import java.util.{List => _, _}

import scala.util._

import language.higherKinds

object Layout {
  def find(home: Path, pwd: Path, env: Environment): Try[Layout] = findBase(pwd).map(Layout(home, pwd, env, _))

  private def findBase(dir: Path): Try[Path] = {
    import java.nio.file.Files.{getFileStore, isRegularFile}
    val here       = dir.javaPath
    val fileSystem = getFileStore(here)
    val parents = Stream.iterate(here.toAbsolutePath)(_.getParent)
    val parentsWithinFs = parents.takeWhile(Option(_).exists(getFileStore(_) == fileSystem))
    val optParent = parentsWithinFs.find { path => isRegularFile(path.resolve(".fury.conf")) }.map(Path(_))
    
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

  def data(filename: String): Path = (Path(filename) in dataHome).extantParents()
  def config(filename: String): Path = (Path(filename) in configHome).extantParents()
  def cache(filename: String): Path = (Path(filename) in cacheHome).extantParents()
}

object Os {
  implicit val stringShow: StringShow[Os] = {
    case Windows(m) => str"Windows ($m)"
    case Linux(m)   => str"Linux ($m)"
    case MacOs(m)   => str"Mac OS X ($m)"
  }

  implicit val userMsg: MsgShow[Os] = os => UserMsg { implicit theme =>
    msg"${theme.path(stringShow.show(os))}".string(theme)
  }
}

sealed trait Os
case class Windows(machine: Machine) extends Os
case class Linux(machine: Machine) extends Os
case class MacOs(machine: Machine) extends Os

object Machine {
  implicit val stringShow: StringShow[Machine] = {
    case X64 => "64-bit x86"
    case X86 => "32-bit x86"
  }
}
sealed trait Machine
case object X64 extends Machine
case object X86 extends Machine

object Installation {

  lazy val system: Option[Os] = {
    import environments.enclosing
    val machine: Option[Machine] = sh"uname -m".exec[Try[String]] match {
      case Success("x86_64" | "amd64") => Some(X64)
      case Success("i386" | "i686")    => Some(X86)
      case _                           => None
    }

    sh"uname".exec[Try[String]] match {
      case Success("Darwin")   => machine.map(MacOs(_))
      case Success("Linux")    => machine.map(Linux(_))
      case Success(r"MINGW.*") => machine.map(Windows(_))
      case _                   => None
    }
  }

  val installDir: Path = Path(System.getProperty("fury.home"))
  val usrDir: Path = (installDir / "usr").extant()
  val ipfsInstallDir: Path = installDir / "ipfs"
  val ipfsBin: Path = ipfsInstallDir / "go-ipfs" / "ipfs"

  val cache: Path = Xdg.cache("fury")
  val config: Path = Xdg.config("fury")
  val data: Path = Xdg.data("fury")

  val userConfig: Path = config / "config.fury"
  val aliasesPath: Path = config / "aliases"
  val layersPath: Path = data / "layers"
  val policyFile: Path = config / "policy.conf"
  val srcsDir: Path = cache / "sources"
  val reposDir: Path = cache / "repos"
  val binsDir: Path = cache / "bins"
  val logsDir: Path = cache / "logs"
  val upgradeDir: Path = cache / "upgrade"
  val policyDir: Path = cache / "policies"
  val scriptsDir: Path = Xdg.runtimeDir.extant() / "scripts"

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
}

case class Layout(home: Path, pwd: Path, env: Environment, baseDir: Path) {
  private[this] val uniqueId: String = java.util.UUID.randomUUID().toString
  
  lazy val furyDir: Path = (baseDir / ".fury").extant()
  lazy val bspDir: Path = (baseDir / ".bsp").extant()
  lazy val bloopDir: Path = (baseDir / ".bloop").extant()
  lazy val confFile: Path = baseDir / ".fury.conf"

  lazy val classesDir: Path = (furyDir / "classes").extant()
  lazy val benchmarksDir: Path = (furyDir / "benchmarks").extant()
  lazy val analysisDir: Path = (furyDir / "analysis").extant()
  lazy val resourcesDir: Path = (furyDir / "resources").extant()
  lazy val basesDir: Path = (furyDir / "bases").extant()
  lazy val workDir: Path = (furyDir / "work").extant()
  lazy val sharedDir: Path = (furyDir / "build" / uniqueId).extant()
  lazy val logsDir: Path = (furyDir / "logs").extant()
  lazy val bspLogsDir: Path = (logsDir / "bsp").extant()
  
  def bloopConfig(targetId: TargetId): Path = bloopDir.extant() / str"${targetId.key}.json"
  def outputDir(targetId: TargetId): Path = (analysisDir / targetId.key).extant()
  def workDir(targetId: TargetId): Path = (workDir / targetId.key).extant()
  def benchmarksDir(targetId: TargetId): Path = (benchmarksDir / targetId.key).extant()
  def classesDir(targetId: TargetId): Path = (classesDir / targetId.key).extant()
  def resourcesDir(targetId: TargetId): Path = (resourcesDir / targetId.key).extant()
}
