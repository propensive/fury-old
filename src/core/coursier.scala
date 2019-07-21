/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.5.0. Copyright 2018-19 Jon Pretty, Propensive Ltd.                                        ║
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

import fury.io._, fury.strings._

import scala.collection.mutable.{HashMap => MutableMap}
import scala.concurrent._
import coursier.{Module => CModule, _}

object Coursier {
  implicit val ec: ExecutionContext = ExecutionContext.global
  private val cache: MutableMap[Binary, Future[List[Path]]] = MutableMap()

  private val scalaCore = Set(
    Organization("org.scala-lang") -> ModuleName("scala-library"),
    Organization("org.scala-lang") -> ModuleName("scala-compiler"),
    Organization("org.scala-lang") -> ModuleName("scala-reflect"),
    Organization("org.scala-lang") -> ModuleName("scala-xml")
  )

  def fetch(io: Io, binary: Binary): Future[List[Path]] = {
    def resolveRepository(repoId: String): Future[Repository] =
      coursier.internal.SharedRepositoryParser.repository(repoId)
        .map(Future.successful(_)).left
        .map { _ => Future.failed[Repository](UnknownBinaryRepository(binary.binRepo)) }
        .merge

    def resolve(repo: Repository): Future[List[Path]] = {
      io.println(msg"Resolving $binary")
      val dependency = Dependency(
          module = CModule(Organization(binary.group), ModuleName(binary.artifact)),
          version = binary.version,
          exclusions = if(binary.group == "org.scala-lang") Set.empty else scalaCore
      )
      val request = coursier.Fetch().addRepositories(repo).addDependencies(dependency).future
      
      request.map(_.map(Path(_)).to[List])
    }

    cache.getOrElseUpdate(binary, resolveRepository(binary.binRepo.id).flatMap(resolve))
  }
}
