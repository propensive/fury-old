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
package fury.core

import fury._, io._, strings._, ogdl._, model._

import gastronomy._
import kaleidoscope._
import guillotine._, environments.enclosing
import java.util.{List => _, _}

import scala.util._
import scala.io._

object Install {
  
  def apply(env: Environment, force: Boolean)(implicit log: Log): Try[Unit] = for {
    _ <- doCCompilation(env).recover { case e =>
           log.warn(msg"Could not find a C compiler (${ExecName("cc")}) on the PATH")
           log.warn(msg"Fury will work, but will be unable to display the ${ExecName("fury")} process name and"+
               msg" will use the slower Python Nailgun client, instead of a native client")
         }
    
    _ <- rcInstall(env, force, Xdg.home / ".zshrc", ExecName("zsh"), zshCompletions)
    _ <- rcInstall(env, force, Xdg.home / ".bashrc", ExecName("bash"), Nil)
    _ <- fishInstall(env)
  } yield ()

  private def doCCompilation(env: Environment)(implicit log: Log): Try[Unit] = for {
    _ <- cCompile(Installation.binDir / "procname.c", Installation.binDir / "libprocname.so",
                            env, "-Wall", "-Werror", "-fPIC", "-shared")

    _ <- cCompile(Installation.binDir / "ng.c", Installation.binDir / "ng", env)
    _ <- ~log.info(msg"Compiled the native Nailgun client and ${ExecName("fury")} process name wrapper")
  } yield ()

  private def fishInstall(env: Environment)(implicit log: Log): Try[Unit] =
    Try(log.warn(msg"Installation for ${ExecName("fish")} is not yet supported"))

  private final val furyTag: String = "# Added by Fury"
  
  private final def setPathLine(paths: List[Path]): List[String] =
    List("", str"PATH=${paths.map(_.value).join(":")}:$$PATH $furyTag")
  
  private final val zshCompletions: List[String] = List(
    str"fpath=(${Installation.scriptDir} $$fpath) $furyTag",
    str"compinit $furyTag"
  )

  private def rcInstall(env: Environment, force: Boolean, file: Path, shell: ExecName, extra: List[String])
                       (implicit log: Log)
                       : Try[Unit] = { for {
    lines <- Try(scala.io.Source.fromFile(file.javaFile).getLines.filterNot(_.endsWith(furyTag))).recover {
               case e if(force || shell == getShell(env)) => List("")
             }
    
    _     <- file.writeSync((lines.to[List] ++ setPathLine(List(Installation.binDir,
                 Installation.usrDir)) ++ extra).join("", "\n", "\n"))
    
    _     <- Try(log.info(msg"Updated ${file} to include ${ExecName("fury")} on the PATH"))
  } yield () }.recover { case e =>
    log.warn(msg"Could not find the file ${file} to install ${ExecName("fury")} on the PATH")
  }

  private def cCompile(src: Path, dest: Path, env: Environment, args: String*): Try[Unit] = for {
    cc  <- Installation.findExecutable(ExecName("cc"), env)
    out <- sh"${cc.value} ${src.value} $args -o ${dest.value}".exec[Try[String]]
  } yield ()

  private def getShell(env: Environment): Option[ExecName] =
    env.variables.get("SHELL").map(_.split("/").last).map(ExecName(_))
}