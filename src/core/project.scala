/*

    Fury, version 0.33.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

import fury.model._, fury.text._

import mercator._
import gastronomy._

import scala.util._
import scala.collection.immutable._

case class ProjectConflict(ids: Map[ProjectRef, (Project, Set[Pointer])]) extends FuryException

object Project {
  implicit val msgShow: MsgShow[Project] = v => UserMsg(_.project(v.id.key))
  implicit val stringShow: StringShow[Project] = _.id.key
  implicit val diff: Diff[Project] = Diff.gen[Project]
  implicit val keyName: KeyName[Project] = () => msg"project"

  def available(projectId: ProjectId, layer: Layer): Boolean =
    !layer.projects.findBy(projectId).isSuccess
}

case class Project(id: ProjectId,
                   modules: SortedSet[Module] = TreeSet(),
                   main: Option[ModuleId] = None,
                   license: LicenseId = License.unknown,
                   description: String = "",
                   compiler: Option[CompilerRef] = None) {

  def projectRef(repos: SortedSet[Repo]): ProjectRef = {
    val modulesInput = modules.to[List].map(digesterInput(repos, _))
    val digest = (id, description, main, license, modulesInput).digest[Sha256]
    ProjectRef(id, Some(digest.encoded[Hex].take(6).toLowerCase))
  }

  private[this] def digesterInput(repos: SortedSet[Repo], m: Module) = {
    val inputs = (m.externalSources ++ m.externalResources)
    val rs = repos.filter{ r => inputs.map(_.repoId).contains(r.id) }
    (m.id, m.kind, m.manifest, m.compiler, m.dependencies.to[List], m.opts.to[List], m.binaries.to[List],
      m.environment.to[List], m.policy.to[List], m.hidden, m.optDefs.to[List], m.deterministic,
      m.sources.to[List] ++ m.resources.to[List], rs.to[List])
  }

  def apply(module: ModuleId): Try[Module] = modules.findBy(module)
  def moduleRefs: List[ModuleRef] = modules.to[List].map(_.ref(this))
  def compilerRefs: List[ModuleRef] = modules.to[List].collect { case m if m.kind.is[Compiler] => m.ref(this) }
  def allRepoIds: Set[RepoId] = modules.flatMap(_.sources).collect { case RepoSource(repoId, _, _) => repoId }
  def mainModule: Try[Option[Module]] = main.traverse(modules.findBy(_))
}