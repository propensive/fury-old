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

    def merge(getUniverse: Try[Universe], child: (ImportId, Hierarchy)): Try[Universe] = for {
      universe     <- getUniverse
      next         <- child._2.universe
      candidates    = (universe.ids -- localProjectIds).intersect(next.ids)
      conflictIds   = candidates.filter { id => universe(id) != next(id) }

      newUniverse  <- if(conflictIds.isEmpty) Success(universe ++ next)
                      else Failure(ProjectConflict(conflictIds, path, child._2.path))
    } yield newUniverse

    children.foldLeft(Try(Universe()))(merge).map(_ ++ layer.localUniverse(path))
  }

  def apply(importPath: ImportPath): Try[Layer] =
    if(importPath.isEmpty) Success(layer)
    else children.get(importPath.head).ascribe(CantResolveLayer(importPath)).flatMap(_(importPath.tail))

  def update(importPath: ImportPath, newLayer: Layer)(implicit log: Log): Try[Hierarchy] =
    if(importPath.isEmpty) Success(copy(layer = newLayer))
    else children.get(importPath.head).ascribe(CantResolveLayer(importPath)).flatMap { hierarchy =>
      hierarchy.update(importPath.tail, newLayer).flatMap { h => Layer.store(h.layer).map { ref =>
        copy(
          children = children.updated(importPath.head, h),
          layer = Layer(_.imports(importPath.head).layerRef)(layer) = ref
        )
      } }
    }

  def updateAll[T]
               (items: Traversable[(ImportPath, T)])
               (fn: (Layer, T) => Layer)
               (implicit log: Log)
               : Try[Hierarchy] =
    items.foldLeft(Try(this)) { case (getHierarchy, (path, value)) => for {
      hierarchy <- getHierarchy
      layer     <- hierarchy(path) >> (fn(_, value))
      hierarchy <- hierarchy(path) = layer
    } yield hierarchy }

  def save(importPath: ImportPath, layout: Layout)(implicit log: Log): Try[LayerRef] =
    children.values.to[List].traverse(_.save(importPath, layout)).flatMap { _ =>
      Layer.store(layer).flatMap { ref =>
        if(path.isEmpty) Layer.saveFuryConf(FuryConf(ref, importPath), layout).map(ref.waive) else Success(ref)
      }
    }
}
