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
package fury.core

import fury.io._, fury.strings._, fury.model._

import coursier.{Module => CModule, _}

import scala.collection.mutable.{HashMap => MutableMap}
import scala.util._

object Coursier {
  
  private val scalaCore = Set(
    Organization("org.scala-lang") -> ModuleName("scala-library"),
    Organization("org.scala-lang") -> ModuleName("scala-compiler"),
    Organization("org.scala-lang") -> ModuleName("scala-reflect"),
    Organization("org.scala-lang") -> ModuleName("scala-xml")
  )

  private def mkRequest(binary: Binary): Try[List[String]] = Try {
    coursier.Fetch().addRepositories(binary.repo).addDependencies(binary.dependency).run()
  }.recoverWith {
    case ex: java.net.UnknownHostException => Failure(DnsResolutionFailure())
    case ex: java.net.SocketException      => Failure(OfflineException())
  }
  
  def fetch(io: Io, binary: Binary, layout: Layout): Try[List[Path]] = {
    val dir = layout.binariesDir / binary.group / binary.artifact / binary.version
    if(dir.exists) Success(dir.children)
    else {
      val tmpDir = layout.binariesDir / java.util.UUID.randomUUID().toString
      
      val paths = for {
        _     <- tmpDir.mkDir()
        lines <- mkRequest()
        paths <- ~lines.map(Path(_)).to[List]
        _     <- paths.map { file => file.hardLink(tmpDir / file.name) }.sequence
        _     <- dir.mkParents()
        _     <- tmpDir.moveTo(dir)
      } yield paths

      paths.recoverWith { _ => tmpDir.delete() }

      paths
    }
  }
}
