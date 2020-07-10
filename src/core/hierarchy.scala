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

import scala.util._

case class Hierarchy(layer: Layer, path: ImportPath, children: Map[ImportId, Hierarchy]) {
  lazy val universe: Try[Universe] = {
    val localProjectIds = layer.projects.map(_.id)

    def merge(universe: Try[Universe], hierarchy: (ImportId, Hierarchy)): Try[Universe] = for {
      projects     <- universe
      nextProjects <- hierarchy._2.universe
      candidates    = (projects.ids -- localProjectIds).intersect(nextProjects.ids)
      conflictIds   = candidates.filter { id => projects.spec(id) != nextProjects.spec(id) }

      allProjects  <- conflictIds.to[List] match {
                        case Nil => Success(projects ++ nextProjects)
                        case _   => Failure(ProjectConflict(conflictIds, path, hierarchy._2.path))
                      }
    } yield allProjects

    val empty: Try[Universe] = Success(Universe(Map(), Map()))

    for(allChildren <- children.foldLeft(empty)(merge)) yield {
      val layerEntities = layer.projects.map { project => project.id -> Entity(project, Map(path -> layer)) }
      allChildren ++ Universe(layerEntities.toMap, layer.repoSets(path))
    }
  }

  def apply(importPath: ImportPath): Try[Layer] =
    if(importPath.isEmpty) Success(layer)
    else children.get(importPath.head).ascribe(CantResolveLayer(importPath)).flatMap(_(importPath.tail))

  def update(importPath: ImportPath, newLayer: Layer)(implicit log: Log): Try[Hierarchy] =
    if(importPath.isEmpty) Success(copy(layer = newLayer))
    else children.get(importPath.head).ascribe(CantResolveLayer(importPath)).flatMap { hierarchy =>
      hierarchy.update(importPath.tail, newLayer).flatMap { h => Layer.store(newLayer).map { ref =>
        copy(
          children = children.updated(importPath.head, h),
          layer = Layer(_.imports(importPath.head).layerRef)(layer) = ref
        )
      } }
    }
  
  def save(importPath: ImportPath, layout: Layout)(implicit log: Log): Try[LayerRef] =
    children.values.to[List].traverse(_.save(importPath, layout)).flatMap { _ =>
      Layer.store(layer).flatMap { ref =>
        if(path == ImportPath.Root) Layer.saveFuryConf(FuryConf(ref, importPath), layout).map(ref.waive)
        else Success(ref)
      }
    }
}
