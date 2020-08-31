/*

    Fury, version 0.18.9. Copyright 2018-20 Jon Pretty, Propensive OÜ.

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

import coursier.{Module => CsModule, Dependency => CsDependency, _}

import scala.util._

object Coursier {
  
  private val ScalaCore = Set("library", "compiler", "reflect", "xml").map { mod =>
    Organization("org.scala-lang") -> ModuleName(s"scala-$mod")
  }

  private def csDependency(binary: Binary): CsDependency = CsDependency(
    module = CsModule(Organization(binary.group), ModuleName(binary.artifact)),
    version = binary.version
  ).withExclusions(if(binary.group == "org.scala-lang") Set.empty else ScalaCore)

  private def csBinRepo(binRepo: BinRepoId): Try[Repository] =
    internal.SharedRepositoryParser.repository(binRepo.id) match {
      case Left(err)   => Failure(InvalidValue(binRepo.id))
      case Right(repo) => Success(repo)
    }

  def resolve(binaries: Seq[Binary])(implicit log: Log): Try[List[BinaryRef]] =
    if(binaries.isEmpty) Success(Nil)
    else {
      val csDeps = binaries.map(csDependency(_))
      binaries.map(_.binRepo).to[List].distinct.traverse(csBinRepo(_)).map { csRepos =>
        val resolution = coursier.Resolve().addRepositories(csRepos: _*).addDependencies(csDeps: _*).run()
        binaries.foldLeft(resolution.dependencySet.set.map { csDependency =>
          val name = BinaryName(csDependency.module.organization.value, csDependency.module.name.value)
          val coordinates = BinaryCoordinates(name, Version(csDependency.version))

          BinaryRef(None, coordinates, None)
        }.map { binaryRef => (binaryRef.coordinates.name, binaryRef) }.toMap) { case (acc, next) =>
          acc.updated(next.name, acc(next.name).copy(id = Some(next.id), binRepo = Some(next.binRepo)))
        }.values.to[List].sortBy(_.transitive)
      }
    }
  
  def fetch(binaries: List[Binary])(implicit log: Log): Try[List[Path]] =
    if(binaries.isEmpty) Success(Nil)
    else {
      val csDeps = binaries.map(csDependency(_))
      binaries.map(_.binRepo).to[List].distinct.traverse(csBinRepo(_)).map { csRepos =>
        coursier.Fetch().addRepositories(csRepos: _*).addDependencies(csDeps: _*).run().map(Path(_)).to[List]
      }
    }
}
