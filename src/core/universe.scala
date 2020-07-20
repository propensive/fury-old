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

object Universe {
  def apply(hierarchy: Hierarchy): Universe = Universe(hierarchy, Map(), Map(), Map())

  sealed trait ProjectDefinition {
    def id: ProjectId
    def +(other: ProjectDefinition): ProjectDefinition
  }

  case class UniqueProject(ref: ProjectRef, origins: Set[Pointer]) extends ProjectDefinition {
    override def id: ProjectId = ref.id

    override def +(other: ProjectDefinition): ProjectDefinition = {
      if(other.id != this.id) throw new IllegalArgumentException(str"Project IDs ${this.id} and ${other.id} are different")
      else other match {
        case UniqueProject(ref, origins) if ref == this.ref => UniqueProject(ref, this.origins ++ origins)
        case ConflictingProjects(origins) => ConflictingProjects(origins ++ this.origins.map(i => i -> this.ref))
      }
    }
  }

  case class ConflictingProjects private[Universe](origins: Map[Pointer, ProjectRef]) extends ProjectDefinition {
    override def id: ProjectId = origins.values.head.id

    override def +(other: ProjectDefinition): ProjectDefinition = {
      if (other.id != this.id) throw new IllegalArgumentException(str"Project IDs ${this.id} and ${other.id} are different")
      else other match {
        case UniqueProject(ref, origins) => ConflictingProjects(this.origins ++ origins.map(i => i -> ref))
        case ConflictingProjects(origins) => ConflictingProjects(this.origins ++ origins)
      }
    }
  }
}

/** A Universe represents a the fully-resolved set of projects available in the layer */
case class Universe(hierarchy: Hierarchy,
                    projects: Map[ProjectId, Universe.ProjectDefinition],
                    repoSets: Map[RepoSetId, Set[RepoRef]],
                    imports: Map[ShortLayerRef, LayerEntity]) {
  def ids: Set[ProjectId] = projects.keySet

  import Universe._

  def pointers(id: ProjectRef): Try[Set[Pointer]] = projects.get(id.id).ascribe(ItemNotFound(id)).flatMap {
    case UniqueProject(`id`, origins) => ~origins
    case UniqueProject(otherId, _) => Failure(new IllegalStateException(str"Expected $id but found $otherId"))
    case ConflictingProjects(origins) =>
      val refOrigins = origins.collect{ case (origin, `id`) => origin }
      if (refOrigins.isEmpty) Failure(new IllegalStateException(str"No known import for $id"))
      else ~refOrigins.toSet
  }

  def pointers(id: ProjectId): Try[Set[Pointer]] = projects.get(id).ascribe(ItemNotFound(id)).flatMap {
    case UniqueProject(_, origins) => ~origins
    case ConflictingProjects(origins) =>
      val projectsByRef = origins.values.toSet.map{ ref: ProjectRef => ref -> apply(ref) }.toMap
      val importsByRef = origins.values.map{ case ref => ref -> origins.collect{ case (o, `ref`) => o }.toSet }
      importsByRef.traverse { case (ref, _) =>
        for {
          project <- projectsByRef(ref)
          imports <- pointers(ref)
        } yield (ref, project, imports)
      } >>= (conflicts => Failure(ProjectConflict(conflicts.toList)))
  }

  def allProjects: Try[Set[Project]] = projects.keySet.traverse(apply(_))
  def layer(id: ProjectId): Try[Layer] = pointers(id).flatMap { is => hierarchy(is.head) }
  
  def layer(id: ProjectRef): Try[Layer] = (projects(id.id) match {
    case UniqueProject(`id`, origins) => ~origins.head
    case UniqueProject(otherId, _) => Failure(new IllegalStateException(str"Expected $id but found $otherId"))
    case _: ConflictingProjects => pointers(id) >> (_.head)
  }).flatMap(hierarchy(_))
  
  def apply(id: ProjectRef): Try[Project] = layer(id).flatMap(_.projects.findBy(id.id))
  def apply(id: ProjectId): Try[Project] = layer(id).flatMap(_.projects.findBy(id))
  def apply(id: RepoSetId): Try[Set[RepoRef]] = repoSets.get(id).ascribe(ItemNotFound(id))
  def apply(id: ShortLayerRef): Try[LayerEntity] = imports.get(id).ascribe(ItemNotFound(id))

  def projectRefs: Set[ProjectRef] = projects.foldLeft(Set[ProjectRef]()) {
    case (acc, (_, UniqueProject(ref, _))) => acc + ref.copy(digest = None)
    case (acc, (_, ConflictingProjects(origins))) => acc ++ origins.values
  }

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

    val newProjects = that.projects.values.foldLeft(projects) { case (acc, proj) =>
      acc.updated(proj.id, acc.get(proj.id).map(_ + proj).getOrElse(proj))
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