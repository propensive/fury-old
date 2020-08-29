/*

    Fury, version 0.18.22. Copyright 2018-20 Jon Pretty, Propensive OÜ.

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
    //TODO remove Try
    def merge(getUniverse: Universe, child: (ImportId, Hierarchy)): Universe =
      getUniverse ++ child._2.universe.get

    Try(children.foldLeft(Universe(this))(merge) ++ layer.localUniverse(this, path))
  }

  def on(pointer: Pointer)(updateLayer: Layer => Try[Layer])(implicit log: Log): Try[Hierarchy] =
    apply(pointer) >>= updateLayer >>= (this(pointer) = _)

  def apply(pointer: Pointer): Try[Layer] =
    if(pointer.isEmpty) ~layer
    else children.get(pointer.head).ascribe(CantResolveLayer(pointer)).flatMap(_(pointer.tail))

  def update(pointer: Pointer, newLayer: Layer)(implicit log: Log): Try[Hierarchy] =
    if(pointer.isEmpty) Success(copy(layer = newLayer))
    else children.get(pointer.head).ascribe(CantResolveLayer(pointer)).flatMap { hierarchy =>
      hierarchy.update(pointer.tail, newLayer).flatMap { h => h.layerRef.map { ref =>
        copy(
          children = children.updated(pointer.head, h),
          layer = Layer(_.imports(pointer.head).layerRef)(layer) = ref
        )
      } }
    }

  def updateAll[T](xs: Traversable[(Pointer, T)])(fn: (Layer, T) => Layer)(implicit log: Log): Try[Hierarchy] =
    xs.foldLeft(Try(this)) { case (getHierarchy, (path, value)) => for {
      hierarchy <- getHierarchy
      layer     <- hierarchy(path) >> (fn(_, value))
      hierarchy <- hierarchy(path) = layer
    } yield hierarchy }

  def save(pointer: Pointer, layout: Layout)(implicit log: Log): Try[LayerRef] =
    children.values.to[List].traverse(_.save(pointer, layout)).flatMap { _ =>
      layerRef.flatMap { ref =>
        if(path.isEmpty) Layer.saveFuryConf(FuryConf(ref, pointer), layout).map(ref.waive) else Success(ref)
      }
    }

  lazy val layerRef: Try[LayerRef] = Layer.store(layer)(Log())
  def focus(pointer: Pointer): Try[Focus] = layerRef >> (Focus(_, pointer, None))
  
  def focus(pointer: Pointer, projectId: ProjectId): Try[Focus] =
    layerRef >> (Focus(_, pointer, Some((projectId, None))))

  def focus(pointer: Pointer, projectId: ProjectId, moduleId: ModuleId): Try[Focus] =
    layerRef >> (Focus(_, pointer, Some((projectId, Some(moduleId)))))
}
