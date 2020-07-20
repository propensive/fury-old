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

case class Hierarchy(layer: Layer, path: Pointer, children: Map[ImportId, Hierarchy]) {
  lazy val universe: Try[Universe] = {
    val localProjectIds = layer.projects.map(_.id)

    def merge(getUniverse: Try[Universe], child: (ImportId, Hierarchy)): Try[Universe] = for {
      universe     <- getUniverse
      next         <- child._2.universe
    } yield universe ++ next

    children.foldLeft(Try(Universe(this)))(merge).map(_ ++ layer.localUniverse(this, path))
  }

  def apply(pointer: Pointer): Try[Layer] =
    if(pointer.isEmpty) Success(layer)
    else children.get(pointer.head).ascribe(CantResolveLayer(pointer)).flatMap(_(pointer.tail))

  def update(pointer: Pointer, newLayer: Layer)(implicit log: Log): Try[Hierarchy] =
    if(pointer.isEmpty) Success(copy(layer = newLayer))
    else children.get(pointer.head).ascribe(CantResolveLayer(pointer)).flatMap { hierarchy =>
      hierarchy.update(pointer.tail, newLayer).flatMap { h => Layer.store(h.layer).map { ref =>
        copy(
          children = children.updated(pointer.head, h),
          layer = Layer(_.imports(pointer.head).layerRef)(layer) = ref
        )
      } }
    }

  def updateAll[T]
               (items: Traversable[(Pointer, T)])
               (fn: (Layer, T) => Layer)
               (implicit log: Log)
               : Try[Hierarchy] =
    items.foldLeft(Try(this)) { case (getHierarchy, (path, value)) => for {
      hierarchy <- getHierarchy
      layer     <- hierarchy(path) >> (fn(_, value))
      hierarchy <- hierarchy(path) = layer
    } yield hierarchy }

  def save(pointer: Pointer, layout: Layout)(implicit log: Log): Try[LayerRef] =
    children.values.to[List].traverse(_.save(pointer, layout)).flatMap { _ =>
      Layer.store(layer).flatMap { ref =>
        if(path.isEmpty) Layer.saveFuryConf(FuryConf(ref, pointer), layout).map(ref.waive) else Success(ref)
      }
    }
}
