/*

    Fury, version 0.18.8. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

import mercator._

import fury.model._, fury.io._, fury.text._

import scala.util._

object Target {
  case class Graph(deps: Map[ModuleRef, Set[Dependency]], targets: Map[ModuleRef, Target]) {
    def links: Map[ModuleRef, Set[Dependency]] = dependencies.map { case (ref, dependencies) =>
      (ref, dependencies.map { dRef => if(targets(dRef.ref).module.kind.is[Compiler]) dRef.hide else dRef })
    }.toMap

    lazy val dependencies = deps.updated(ModuleRef.JavaRef, Set())
  }

  def apply(ref: ModuleRef, hierarchy: Hierarchy, universe: Universe, layout: Layout)
           (implicit log: Log)
           : Try[Target] = for {
      project   <- universe(ref.projectId)
      module    <- project(ref.moduleId)
      _         <- universe.binaryConflicts(ref)
      binPaths  <- universe.binaryPaths(ref)
      snapshots <- universe.checkout(ref, hierarchy, layout)
      sources   <- module.sources.to[List].traverse(snapshots.dir(_, layout))
    } yield Target(ref, module, project, snapshots, sources, binPaths )
}

case class Target(ref: ModuleRef,
                  module: Module,
                  project: Project,
                  snapshots: Snapshots,
                  sourcePaths: List[Path],
                  binaries: List[Path]) {
  lazy val environment: Map[String, String] = module.environment.map { e => e.id -> e.value }.toMap
  lazy val properties: Map[String, String] = module.properties.map { p => p.id -> p.value }.toMap
}