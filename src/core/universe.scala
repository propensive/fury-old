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

object Universe { def apply(hierarchy: Hierarchy): Universe = Universe(hierarchy, Map(), Map(), Map()) }

/** A Universe represents a the fully-resolved set of projects available in the layer */
case class Universe(hierarchy: Hierarchy,
                    projects: Map[ProjectId, Map[ProjectRef, Set[ImportPath]]],
                    repoSets: Map[RepoSetId, Set[RepoRef]],
                    imports: Map[ShortLayerRef, LayerEntity]) {
  def ids: Set[ProjectId] = projects.keySet
  
  def importPaths(id: ProjectId): Try[Set[ImportPath]] = projects.get(id).ascribe(ItemNotFound(id)).flatMap {
    case map if map.size == 1 => Success(map.values.head)
    case map                  => Failure(ProjectConflict(map))
  }

  def allProjects: Try[Set[Project]] = projects.keySet.traverse(apply(_))
  def layer(id: ProjectId): Try[Layer] = importPaths(id).flatMap { is => hierarchy(is.head) }
  def layer(id: ProjectRef): Try[Layer] = hierarchy(projects(id.id)(id).head)
  def apply(id: ProjectRef): Try[Project] = layer(id).flatMap(_.projects.findBy(id.id))
  def apply(id: ProjectId): Try[Project] = layer(id).flatMap(_.projects.findBy(id))
  def apply(id: RepoSetId): Try[Set[RepoRef]] = repoSets.get(id).ascribe(ItemNotFound(id))
  def apply(id: ShortLayerRef): Try[LayerEntity] = imports.get(id).ascribe(ItemNotFound(id))

  def projectRefs: Set[ProjectRef] =
    projects.foldLeft(Set[ProjectRef]()) { case (acc, (_, map)) => acc ++ map.keySet }

  def checkout(ref: ModuleRef, hierarchy: Hierarchy, layout: Layout)(implicit log: Log): Try[Snapshots] = for {
    project <- apply(ref.projectId)
    module  <- project(ref.moduleId)
    layer   <- layer(ref.projectId)

    repos   <- (module.externalSources ++ module.externalResources).to[List].groupBy(_.repoId).map {
                 case (k, v) => layer.repos.findBy(k).map(_ -> v)
               }.sequence

  } yield Snapshots(repos.map { case (repo, paths) =>
    val snapshot = Snapshot(repo.id, repo.remote, repo.localDir(layout), repo.commit, repo.branch,
        paths.map(_.dir).to[List])
    
    snapshot.hash -> snapshot
  }.toMap)

  def ++(that: Universe): Universe = {
    val newRepoSets = that.repoSets.foldLeft(repoSets) { case (acc, (digest, set)) =>
      acc.updated(digest, acc.getOrElse(digest, Set()) ++ set)
    }

    val newProjects = that.projects.foldLeft(projects) { case (acc, (id, map)) =>
      acc.updated(id, acc.get(id).fold(map)(map.foldLeft(_) { case (acc2, (ref, set)) =>
        acc2.updated(ref, acc2.get(ref).fold(set)(_ ++ set))
      }))
    }

    val newImports = that.imports.foldLeft(imports) { case (acc, (layerRef, entity)) =>
      acc.updated(layerRef, acc.getOrElse(layerRef, entity) + entity.imports)
    }

    Universe(hierarchy, newProjects, newRepoSets, newImports)
  }

  private[fury] def dependencies(ref: ModuleRef, layout: Layout): Try[Set[Dependency]] =
    transitiveDependencies(forbidden = Set.empty, Dependency(ref), layout).map(_.filter(_.ref != ModuleRef.JavaRef))

  private[this] def transitiveDependencies(forbidden: Set[Dependency], dependency: Dependency, layout: Layout)
                                          : Try[Set[Dependency]] = for {
    project      <- apply(dependency.ref.projectId)
    module       <- project(dependency.ref.moduleId)
    dependencies  =  module.dependencies ++ module.compilerDependencies
    repeats       = dependencies.intersect(forbidden)
    _            <- if(repeats.isEmpty) ~() else Failure(CyclesInDependencies(repeats))
    tDeps        <- dependencies.to[Set].traverse(transitiveDependencies(forbidden + dependency, _,
                        layout).filter(!_.contains(dependency)))
  } yield dependencies ++ tDeps.flatten

  def clean(ref: ModuleRef, layout: Layout): Unit = layout.classesDir.delete().unit

  def apply(ref: ModuleRef): Try[Module] = for {
    project <- apply(ref.projectId)
    module  <- project(ref.moduleId)
  } yield module
}