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

import fury.model._, fury.strings._

import mercator._

import scala.util._

/** A Universe represents a the fully-resolved set of projects available in the layer */
case class Universe(entities: Map[ProjectId, Entity] = Map()) {
  def ids: Set[ProjectId] = entities.keySet
  def entity(id: ProjectId): Try[Entity] = entities.get(id).ascribe(ItemNotFound(id))

  def makeTarget(ref: ModuleRef, layout: Layout)(implicit log: Log): Try[Target] =
    for {
      resolvedProject <- entity(ref.projectId)
      module          <- resolvedProject.project(ref.moduleId)
      compiler        <- if(module.compiler == ModuleRef.JavaRef) Success(None)
                         else makeTarget(module.compiler, layout).map(Some(_))
      binaries        <- module.allBinaries.map(_.paths).sequence.map(_.flatten)
      dependencies    =  module.dependencies.map { dep => TargetId(dep) }
      checkouts       <- checkout(ref, layout)
      sources         <- module.sources.map(_.dir(checkouts, layout)).sequence
    } yield Target(
      ref,
      module.kind,
      module.main,
      module.plugin,
      resolvedProject.layer.repos.map(_.remote).to[List],
      checkouts.checkouts.to[List],
      binaries.to[List],
      dependencies.to[List],
      compiler,
      module.bloopSpec,
      module.opts.to[List],
      module.policy.to[List],
      ref.intransitive,
      sources.to[List],
      module.environment.map { e => (e.id, e.value) }.toMap,
      module.properties.map { p => (p.id, p.value) }.toMap,
      module.optDefs,
      module.resources.to[List]
    )

  def checkout(ref: ModuleRef, layout: Layout)(implicit log: Log): Try[Checkouts] = for {
    entity <- entity(ref.projectId)
    module <- entity.project(ref.moduleId)
    repos  <- (module.externalSources ++ module.externalResources).to[List]
                  .groupBy(_.repoId).map { case (k, v) => entity.layer.repo(k, layout).map(_ -> v) }.sequence
  } yield Checkouts(repos.map { case (repo, paths) =>
    Checkout(repo.id, repo.remote, repo.localDir(layout), repo.commit, repo.branch, paths.map(_.dir).to[List])
  }.to[Set])

  def ++(that: Universe): Universe = Universe(entities ++ that.entities)

  private[fury] def dependencies(ref: ModuleRef, layout: Layout): Try[Set[ModuleRef]] =
    resolveTransitiveDependencies(forbidden = Set.empty, ref, layout)

  private[this] def resolveTransitiveDependencies(forbidden: Set[ModuleRef], ref: ModuleRef, layout: Layout):
      Try[Set[ModuleRef]] = for {
    entity   <- entity(ref.projectId)
    module   <- entity.project(ref.moduleId)
    deps     =  module.dependencies ++ module.compilerDependencies
    repeated =  deps.intersect(forbidden)
    _        <- if (repeated.isEmpty) ~() else Failure(CyclesInDependencies(repeated))
    tDeps    <- deps.map(resolveTransitiveDependencies(forbidden + ref, _,
                    layout).filter(!_.contains(ref))).sequence
  } yield deps ++ tDeps.flatten

  def clean(ref: ModuleRef, layout: Layout): Unit = layout.classesDir.delete().unit

  def getMod(ref: ModuleRef): Try[Module] = for {
    entity <- entity(ref.projectId)
    module <- entity.project(ref.moduleId)
  } yield module
}