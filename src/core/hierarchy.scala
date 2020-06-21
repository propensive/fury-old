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

import fury.model._

import scala.util._

case class Hierarchy(layer: Layer, path: ImportPath, inherited: Set[Hierarchy]) {
  lazy val universe: Try[Universe] = {
    val localProjectIds = layer.projects.map(_.id)

    def merge(universe: Try[Universe], hierarchy: Hierarchy): Try[Universe] = for {
      projects     <- universe
      nextProjects <- hierarchy.universe
      candidates    = (projects.ids -- localProjectIds).intersect(nextProjects.ids)
      conflictIds   = candidates.filter { id => projects.spec(id) != nextProjects.spec(id) }

      allProjects  <- conflictIds.to[List] match {
                        case Nil => Success(projects ++ nextProjects)
                        case _   => Failure(ProjectConflict(conflictIds, path, hierarchy.path))
                      }
    } yield allProjects

    val empty: Try[Universe] = Success(Universe())

    for(allInherited <- inherited.foldLeft(empty)(merge)) yield {
      val layerEntities = layer.projects.map { project => project.id -> Entity(project, Map(path -> layer)) }
      allInherited ++ Universe(layerEntities.toMap)
    }
  }
}
