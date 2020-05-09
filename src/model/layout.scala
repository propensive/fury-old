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
    val optParent = parentsWithinFs.find { path =>
      isRegularFile(path.resolve(".fury/config")) || isRegularFile(path.resolve(".fury.conf"))
    }.map(Path(_))
    
    optParent.ascribe(Unspecified[ProjectId])
  }
}

object Xdg {
  private case class Var(name: String) {
    private val variable = Option(System.getenv(s"XDG_$name"))
    def path: Option[Path] = variable.map(Path(_))
    def paths: Option[List[Path]] = variable.map(_.split(":").to[List].map(Path(_)))
  }
  
  val home: Path = Option(System.getenv("HOME")).map(Path(_)).getOrElse(Path("/"))
  val ipfsRepo: Path = Option(System.getenv("IPFS_PATH")).map(Path(_)).getOrElse(home / ".ipfs")
  val cacheHome: Path = Var("CACHE_HOME").path.getOrElse(home / ".cache")
  val dataHome: Path = Var("DATA_HOME").path.getOrElse(home / ".local" / "share")
  
  val dataDirs: List[Path] = Var("DATA_DIRS").paths.getOrElse(List(Path("/usr/local/share"),
      Path("/usr/share")))
  
  val configHome: Path = Var("CONFIG_HOME").path.getOrElse(home / ".config")
  val configDirs: List[Path] = Var("CONFIG_DIRS").paths.getOrElse(List(Path("/etc/xdg")))
  val runtimeDir: Path = Var("RUNTIME_DIR").path.getOrElse(Path("/tmp"))
  val docsDir: Path = Var("DOCUMENTS_DIR").path.getOrElse(home / "Documents")

  val pathEnv: List[Path] = Option(System.getenv("PATH")).map { str => str.split(":").to[List].map(Path(_))
      }.getOrElse(Nil)

  def findData(filename: Path): Option[Path] = (dataHome :: dataDirs).map(filename in _).find(_.exists)
  def findConfig(filename: Path): Option[Path] = (configHome :: configDirs).map(filename in _).find(_.exists)
  def data(filename: String): Path = (Path(filename) in dataHome).extantParents()
  def config(filename: String): Path = (Path(filename) in configHome).extantParents()
  def cache(filename: String): Path = (Path(filename) in cacheHome).extantParents()
}

object Os {
  case class Unknown(description: String)(machine: Machine) extends Os
  implicit val stringShow: StringShow[Os] = {
    case Windows(m) => str"Windows ($m)"
    case Linux(m)   => str"Linux ($m)"
    case MacOs(m)   => str"Mac OS X ($m)"
    case Unknown(description) => description
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
  case class Unknown(description: String) extends Machine
  implicit val stringShow: StringShow[Machine] = {
    case X64 => "64-bit x86"
    case X86 => "32-bit x86"
    case Unknown(description) => description
  }
}
sealed trait Machine
case object X64 extends Machine
case object X86 extends Machine

object Installation {

  lazy val system: Try[Os] = {
    import environments.enclosing
    
    val machine: Machine = sh"uname -m".exec[Try[String]] match {
      case Success("x86_64" | "amd64") => X64
      case Success("i386" | "i686")    => X86
      case other                       => X64
    }

    sh"uname".exec[Try[String]].map {
      case "Darwin"   => MacOs(machine)
      case "Linux"    => Linux(machine)
      case r"MINGW.*" => Windows(machine)
      case other      => Os.Unknown(other)(machine)
    }
  }

  def findExecutable(name: ExecName, env: Environment): Try[Path] = for {
    paths    <- env.variables.get("PATH").map(_.split(":").to[List].map(Path(_))).ascribe(EnvPathNotSet())
    
    execPath <- paths.find(_.childPaths.exists { f => !f.directory && f.isExecutable && f.name == name.key
                    }).ascribe(NotOnPath(name))

  } yield execPath / name.key


  val installDir: Path = Path(System.getProperty("fury.home"))
  val installVersion: String = installDir.name

  val usrDir: Path = (installDir / "usr").extant()
  val binDir: Path = (installDir / "bin").extant()
  val etcDir: Path = (installDir / "etc").extant()
  val scriptDir: Path = (installDir / "script").extant()
  val ipfsInstallDir: Path = installDir / "ipfs"
  val ipfsBin: Path = ipfsInstallDir / "go-ipfs" / "ipfs"

  val cache: Path = Xdg.cache("fury").extant()
  val config: Path = Xdg.config("fury").extant()
  val data: Path = Xdg.data("fury").extant()

  val rootBinDir: Path = (data / "bin").extant()
  val optDir: Path = (data / "opt").extant()
  val completionsDir: Path = (data / "completions").extant()
  val userConfig: Path = config / "config.fury"
  val aliasesPath: Path = config / "aliases"
  val policyFile: Path = config / "policy.conf"
  val srcsDir: Path = cache / "sources"
  val reposDir: Path = cache / "repos"
  val binsDir: Path = cache / "bins"
  val logsDir: Path = cache / "logs"
  val upgradeDir: Path = cache / "upgrade"
  val policyDir: Path = cache / "policies"
  val scriptsDir: Path = Xdg.runtimeDir.extant() / "scripts"
  val safeDir: Path = Xdg.runtimeDir.extant() / "safe"

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
  lazy val workingGit: Path = baseDir / ".git"
  lazy val bspDir: Path = (baseDir / ".bsp").extant()
  lazy val bspConfig: Path = bspDir / "fury.json"

  lazy val bloopDir: Path = (baseDir / ".bloop").extant()
  
  lazy val confFile: Path = {
    val newConfFile = furyDir / "config"
    val oldConfFile = baseDir / ".fury.conf"
    if(oldConfFile.exists() && !newConfFile.exists()) oldConfFile.moveTo(newConfFile)
    newConfFile
  }
  
  lazy val layerDb: Path = furyDir / "layers.db"
  lazy val confFileBackup: Path = baseDir / ".fury.conf.bak"

  lazy val classesDir: Path = (furyDir / "classes").extant()
  lazy val benchmarksDir: Path = (furyDir / "benchmarks").extant()
  lazy val analysisDir: Path = (furyDir / "analysis").extant()
  lazy val resourcesDir: Path = (furyDir / "resources").extant()
  lazy val basesDir: Path = (furyDir / "bases").extant()
  lazy val workDir: Path = (furyDir / "work").extant()
  lazy val sharedDir: Path = (furyDir / "build" / uniqueId).extant()
  lazy val logsDir: Path = (furyDir / "logs").extant()
  lazy val undoStack: Path = (furyDir / "history").extantParents()

  def bloopConfig(targetId: TargetId): Path = bloopDir.extant() / str"${targetId.key}.json"
  def outputDir(targetId: TargetId): Path = (analysisDir / targetId.key).extant()
  def workDir(targetId: TargetId): Path = (workDir / targetId.key).extant()
  def benchmarksDir(targetId: TargetId): Path = (benchmarksDir / targetId.key).extant()
  def classesDir(targetId: TargetId): Path = (classesDir / targetId.key).extant()
  def resourcesDir(targetId: TargetId): Path = (resourcesDir / targetId.key).extant()
}
