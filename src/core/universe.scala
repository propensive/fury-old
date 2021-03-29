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

import fury.io._, fury.model._, fury.text._, fury.utils._

import mercator._
import jovian._

import scala.util._

import scala.collection.mutable.HashMap

object Universe {
  def apply(hierarchy: Hierarchy): Universe = Universe(hierarchy, Map(), Map(), Map())
}

/** A Universe represents a the fully-resolved set of projects available in the layer */
case class Universe(hierarchy: Hierarchy,
                    projects: Map[ProjectId, Uniqueness[ProjectRef, Pointer]],
                    repoSets: Map[RepoSetId, Set[RepoRef]],
                    imports: Map[ShortLayerRef, LayerProvenance]) {
  def ids: Set[ProjectId] = projects.keySet
  def apply(id: ProjectRef): Try[Project] = layer(id).flatMap(_.projects.findBy(id.id))
  def apply(id: ProjectId): Try[Project] = layer(id).flatMap(_.projects.findBy(id))
  def apply(id: RepoSetId): Try[Set[RepoRef]] = repoSets.get(id).ascribe(ItemNotFound(id))
  def apply(id: ShortLayerRef): Try[LayerProvenance] = imports.get(id).ascribe(ItemNotFound(id))
  def apply(ref: ModuleRef): Try[Module] = apply(ref.projectId) >>= (_(ref.moduleId))
  def clean(ref: ModuleRef, layout: Layout): Unit = layout.classesDir.delete().unit
  def allProjects: Try[Set[Project]] = projects.keySet.traverse(apply(_))
  def resolvedProjects: Try[Set[Project]] = projects.keySet.map(apply(_)).filter(_.isSuccess).sequence
  def layer(id: ProjectId): Try[Layer] = pointers(id).flatMap { is => hierarchy(is.head) }
  def deepModuleRefs: Try[Set[ModuleRef]] = allProjects.map(_.flatMap(_.moduleRefs).to[Set])

  val javaVersions: HashMap[ModuleRef, Try[Int]] = HashMap()

  import Uniqueness._

  def pointers(id: ProjectRef): Try[Set[Pointer]] = projects.get(id.id).ascribe(ItemNotFound(id)).flatMap {
    case Unique(_, origins) => ~origins
    case Ambiguous(origins) =>
      val refOrigins = origins.collect { case (origin, `id`) => origin }
      if (refOrigins.isEmpty) Failure(new IllegalStateException(str"No known import for $id"))
      else ~refOrigins.toSet
  }

  def pointers(id: ProjectId): Try[Set[Pointer]] = projects.get(id).ascribe(ItemNotFound(id)).flatMap {
    case Unique(_, origins) => ~origins
    case Ambiguous(origins) =>
      val projects = origins.values.toSet.map { ref: ProjectRef => ref -> apply(ref) }.toMap
      val imports = origins.values.map { case ref => ref -> origins.collect { case (o, `ref`) => o }.toSet }
      
      imports.to[List].traverse { case (ref, _) =>
        for(project <- projects(ref); imports <- pointers(ref)) yield (ref, (project, imports))
      } >>= (conflicts => Failure(ProjectConflict(conflicts.toMap)))
  }

  def layer(id: ProjectRef): Try[Layer] = (projects(id.id) match {
    case Unique(someId, origins) if someId.id == id.id => ~origins.head
    case Unique(otherId, _) => Failure(new IllegalStateException(str"Expected $id but found $otherId"))
    case _: Ambiguous[ProjectRef, Pointer] => pointers(id) >> (_.head)
  }).flatMap(hierarchy(_))
  

  def deepDependencySearch(ref: ModuleRef): Set[ModuleRef] = { for {
    dependencies <- apply(ref) >> (_.dependencies.map(_.ref).to[Set])
  } yield dependencies.flatMap(deepDependencySearch) }.getOrElse(Set()) + ref

  def projectRefs: Set[ProjectRef] = projects.foldLeft(Set[ProjectRef]()) {
    case (acc, (_, Unique(ref, _)))     => acc + ref.copy(digest = None)
    case (acc, (_, Ambiguous(origins))) => acc ++ origins.values
  }

  private[this] def repoPaths(ref: ModuleRef): Try[Map[Repo, List[Path]]] = for {
    project <- apply(ref.projectId)
    module  <- project(ref.moduleId)
    layer   <- layer(ref.projectId)
    inputs  =  (module.externalSources ++ module.externalResources ++ module.includeSources).to[List]
    
    repos   <- inputs.groupBy(_.rootId.repo).to[List].traverse { case (k, v) =>
                 layer.repos.findBy(k).map(_ -> v.map(_.path))
               }
  } yield repos.toMap

  def checkout(ref: ModuleRef, layout: Layout): Try[Snapshot] = for {
    repoPaths   <- repoPaths(ref)
  } yield Snapshot(repoPaths.map { case (repo, paths) =>
    val stash = Stash(repo.id, repo.remote, repo.localDir(layout), repo.commit, repo.branch, paths)
    stash.hash -> stash
  }.toMap)

  def workspace(ref: ModuleRef, layout: Layout): Try[Option[Path]] = for {
    project   <- apply(ref.projectId)
    module    <- project(ref.moduleId)
    layer     <- layer(ref.projectId)
    layerRef  <- Layer.store(layer)
    workspace <- module.workspace.traverse(layer.workspaces.findBy(_))
  } yield workspace.map { ws => ws.local.getOrElse(layout.workspaceDir(project.id, ws.id)) }

  def packageMap: Try[Map[Pkg, ModuleRef]] = for {
    //_        <- ~Log().info(msg"Getting projects")
    projects <- resolvedProjects
    //_        <- ~Log().info(msg"Got projects: $projects")
    packages  = projects.flatMap { p => p.modules.flatMap { m => m.packages.map(_ -> m.ref(p)) } }
  } yield packages.toMap

  def packageMatch(query: Pkg): Try[ModuleRef] = for {
    map   <- packageMap
    //_     <- ~Log().info(msg"Looking for '$query' in ${packageMap.toString}")
    found <- map.filter { case (pkg, ref) => query.key startsWith pkg.key }.to[List].sortBy(_._2.key.length).headOption.map(_._2).ascribe(UnknownPkg(query))
  } yield found

  def javaVersion(ref: ModuleRef, layout: Layout): Try[Int] =
    javaVersions.getOrElseUpdate(ref, for {
      module   <- apply(ref)
      
      version  <- module.compiler match {
                    case Javac(n)         => Success(n)
                    case BspCompiler(ref) => javaVersion(ref, layout)
                  }
      
      deps     <- dependencies(ref, layout)
      others   <- deps.map(_.ref).traverse(javaVersion(_, layout))
    } yield (others.to[Vector] :+ version).reduce(_ max _))

  def ++(that: Universe): Universe = {
    val newRepoSets = that.repoSets.foldLeft(repoSets) { case (acc, (digest, set)) =>
      acc.updated(digest, acc.getOrElse(digest, Set()) ++ set)
    }

    val newProjects = that.projects.toSeq.foldLeft(projects) { case (acc, (id, uniq)) =>
      acc.updated(id, acc.get(id).map(_ + uniq).getOrElse(uniq))
    }

    val newImports = that.imports.foldLeft(imports) { case (acc, (layerRef, entity)) =>
      acc.updated(layerRef, acc.getOrElse(layerRef, entity) + entity.imports)
    }

    Universe(hierarchy, newProjects, newRepoSets, newImports)
  }

  def buildDependencies[T]
                       (ref: ModuleRef, layout: Layout, targets: Map[ModuleRef, T] = Map())
                       (build: ModuleRef => T)
                       : Try[Map[ModuleRef, T]] =
    dependencies(ref, layout).map(_.map(_.ref)).flatMap(_.filterNot(targets.contains(_)).foldLeft(Try(targets)) {
      case (targetsTry, next) => targetsTry.flatMap { targets =>
        buildDependencies[T](next, layout, targets.updated(ref, build(ref)))(build)
      }
    })

  private[fury] def dependencies(ref: ModuleRef, layout: Layout): Try[Set[Input]] =
    transitiveDependencies(forbidden = Set.empty, Input(ref), layout).map(_.filter(_.ref != ModuleRef.JavaRef))

  private[this] def transitiveDependencies(forbidden: Set[Input], dependency: Input, layout: Layout)
                                          : Try[Set[Input]] = for {
    project      <- apply(dependency.ref.projectId)
    module       <- project(dependency.ref.moduleId)
    dependencies  =  module.dependencies ++ module.compilerDependencies
    repeats       = dependencies.intersect(forbidden)
    _            <- if(repeats.isEmpty) ~() else Failure(CyclesInDependencies(repeats))
    tDeps        <- dependencies.to[Set].traverse(transitiveDependencies(forbidden + dependency, _,
                        layout).filter(!_.contains(dependency)))
  } yield dependencies ++ tDeps.flatten
}
