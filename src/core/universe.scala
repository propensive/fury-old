/*

    Fury, version 0.17.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

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
import scala.collection.immutable.TreeSet

object Universe { def apply(): Universe = Universe(Map(), Map(), Map()) }

/** A Universe represents a the fully-resolved set of projects available in the layer */
case class Universe(entities: Map[ProjectId, Entity],
                    repoSets: Map[RepoSetId, Set[RepoRef]],
                    imports: Map[ShortLayerRef, LayerEntity]) {
  def ids: Set[ProjectId] = entities.keySet
  def entity(id: ProjectId): Try[Entity] = entities.get(id).ascribe(ItemNotFound(id))
  def spec(id: ProjectId): Try[ProjectSpec] = entity(id).map(_.spec)

  def checkout(ref: ModuleRef, layout: Layout)(implicit log: Log): Try[Snapshots] = for {
    entity <- entity(ref.projectId)
    module <- entity.project(ref.moduleId)

    repos  <- (module.externalSources ++ module.externalResources).to[List].groupBy(_.repoId).map {
                case (k, v) => entity.layers.head._2.repos.findBy(k).map(_ -> v)
              }.sequence

  } yield Snapshots(repos.map { case (repo, paths) =>
    val snapshot = Snapshot(repo.id, repo.remote, repo.localDir(layout), repo.commit, repo.branch,
        paths.map(_.dir).to[List])
    
    snapshot.hash -> snapshot
  }.toMap)

  def ++(that: Universe): Universe = {
    val newEntities = that.entities.foldLeft(entities) { case (acc, (id, entity)) =>
      acc.updated(id, acc.get(id).fold(entity) { e => e.copy(layers = e.layers ++ entity.layers) })
    }

    val newRepoSets = that.repoSets.foldLeft(repoSets) { case (acc, (digest, set)) =>
      acc.updated(digest, acc.getOrElse(digest, Set()) ++ set)
    }

    val newImports = that.imports.foldLeft(imports) { case (acc, (layerRef, entity)) =>
      acc.updated(layerRef, acc.getOrElse(layerRef, entity) + entity.imports)
    }

    Universe(newEntities, newRepoSets, newImports)
  }

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