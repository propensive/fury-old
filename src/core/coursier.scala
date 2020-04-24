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

import fury.io._, fury.model._

import mercator._

import coursier.{Module => CModule, _}

import scala.util._

object Coursier {
  
  private val ScalaCore = Set("library", "compiler", "reflect", "xml").map { mod =>
    Organization("org.scala-lang") -> ModuleName(s"scala-$mod")
  }

  private def mkRequest(binary: Binary): Try[List[Path]] = {
    
    val dependency = Dependency(
      module = CModule(Organization(binary.group), ModuleName(binary.artifact)),
      version = binary.version
    ).withExclusions(if(binary.group == "org.scala-lang") Set.empty else ScalaCore)

    coursier.internal.SharedRepositoryParser.repository(binary.binRepo.id) match {
      case Left(err) => Failure(InvalidValue(binary.binRepo.id))
      case Right(repo) =>
        Try {
          val out = coursier.Fetch().addRepositories(repo).addDependencies(dependency).run()
          out.map(Path(_)).to[List]
        }.recoverWith {
          case ex: java.net.UnknownHostException => Failure(DnsResolutionFailure())
          case ex: java.net.SocketException      => Failure(OfflineException())
        }
    }
  }
  
  def fetch(binary: Binary)(implicit log: Log): Try[List[Path]] = synchronized {
    val dir = Installation.binsDir / binary.group / binary.artifact / binary.version
    if(dir.exists) Success(dir.children.map(dir / _))
    else {
      val tmpDir = Installation.binsDir / java.util.UUID.randomUUID().toString
      
      val paths = for {
        _     <- ~tmpDir.mkdir()
        paths <- mkRequest(binary)
        _     <- paths.map { file =>
                   val dest = tmpDir / file.name
                   dest.mkParents()
                   file.hardLink(dest)
                 }.sequence
        _     <- dir.mkParents()
        _     <- tmpDir.moveTo(dir)
      } yield paths

      paths.recoverWith { case _ => tmpDir.delete() }

      paths
    }
  }
}
