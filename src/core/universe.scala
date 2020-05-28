/*

    Fury, version 0.16.1. Copyright 2018-20 Jon Pretty, Propensive OÜ.

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

import scala.util._
import scala.collection.immutable.TreeSet

/** A Universe represents a the fully-resolved set of projects available in the layer */
case class Universe(entities: Map[ProjectId, Entity] = Map()) {
  def ids: Set[ProjectId] = entities.keySet
  def entity(id: ProjectId): Try[Entity] = entities.get(id).ascribe(ItemNotFound(id))

  def checkout(ref: ModuleRef, layout: Layout)(implicit log: Log): Try[Checkouts] = for {
    entity <- entity(ref.projectId)
    module <- entity.project(ref.moduleId)
    repos  <- (module.externalSources ++ module.externalResources).to[List]
                  .groupBy(_.repoId).map { case (k, v) => entity.layer.repo(k, layout).map(_ -> v) }.sequence
  } yield Checkouts(repos.map { case (repo, paths) =>
    Checkout(repo.id, repo.remote, repo.localDir(layout), repo.commit, repo.branch, paths.map(_.dir).to[List])
  }.to[Set])

  def ++(that: Universe): Universe = Universe(entities ++ that.entities)

  private[fury] def dependencies(ref: ModuleRef, layout: Layout): Try[Set[Dependency]] =
    transitiveDependencies(forbidden = Set.empty, Dependency(ref), layout).map(_.filter(_.ref != ModuleRef.JavaRef))

  private[this] def transitiveDependencies(forbidden: Set[Dependency], dependency: Dependency, layout: Layout)
                                          : Try[Set[Dependency]] = for {
    entity       <- entity(dependency.ref.projectId)
    module       <- entity.project(dependency.ref.moduleId)
    dependencies  =  module.dependencies ++ module.compilerDependencies
    repeats       = dependencies.intersect(forbidden)
    _            <- if(repeats.isEmpty) ~() else Failure(CyclesInDependencies(repeats))
    tDeps        <- dependencies.to[Set].traverse(transitiveDependencies(forbidden + dependency, _,
                        layout).filter(!_.contains(dependency)))
  } yield dependencies ++ tDeps.flatten

  def clean(ref: ModuleRef, layout: Layout): Unit = layout.classesDir.delete().unit

  def apply(ref: ModuleRef): Try[Module] = for {
    entity <- entity(ref.projectId)
    module <- entity.project(ref.moduleId)
  } yield module
}