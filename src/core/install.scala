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
import guillotine._, environments.enclosing
import java.util.{List => _, _}

import scala.util._
import scala.io._

object Install {
  
  def apply(env: Environment, force: Boolean): Try[Unit] = for {
    _ <- cCompile(Installation.binDir / "procname.c", Installation.binDir / "libprocname.so", env, " -Wall",
             "-Werror", "-fPIC", "-shared")

    _ <- cCompile(Installation.binDir / "ng.c", Installation.binDir / "ng", env)
    _ <- zshrcInstall(env, force)
    _ <- bashrcInstall(env, force)
  } yield ()

  def findExecutable(name: ExecName, env: Environment): Try[Path] = for {
    paths    <- env.variables.get("PATH").map(_.split(":").to[List].map(Path(_))).ascribe(EnvPathNotSet())
    execPath <- paths.find(_.children.exists(_ == name.key)).ascribe(NotOnPath(name))
  } yield execPath / name.key

  final val furyTag: String = "# Added by Fury"
  final def setPathLine(paths: List[Path]): String = str"PATH=${paths.map(_.value).join(":")}:$$PATH $furyTag"
  
  final val zshCompletions = List(str"autoload -Uz compinit $furyTag",
      str"fpath=($$FURYHOME/completion/zsh $$fpath) $furyTag")

  def zshrcInstall(env: Environment, force: Boolean) = for {
    file  <- Try(Xdg.home / ".zshrc")
    lines <- Try(Source.fromFile(file.javaFile).getLines.filterNot(_.endsWith(furyTag)))
    _     <- file.writeSync((lines.to[List] :+ setPathLine(List(Installation.binDir,
                 Installation.usrDir))).join("\n"))
  } yield ()

  def bashrcInstall(env: Environment, force: Boolean) = for {
    file <- Try(Xdg.home / ".bashrc")
  } yield ()

  def cCompile(src: Path, dest: Path, env: Environment, args: String*): Try[Unit] = for {
    cc  <- findExecutable(ExecName("cc"), env)
    out <- sh"${cc.value} ${src.value} $args -o ${dest.value}".exec[Try[String]]
  } yield ()
}