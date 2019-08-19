/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.6.6. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
   ║                                                                                                           ║
   ║ The primary distribution site is: https://propensive.com/                                                 ║
   ║                                                                                                           ║
   ║ Licensed under  the Apache License,  Version 2.0 (the  "License"); you  may not use  this file  except in ║
   ║ compliance with the License. You may obtain a copy of the License at                                      ║
   ║                                                                                                           ║
   ║     http://www.apache.org/licenses/LICENSE-2.0                                                            ║
   ║                                                                                                           ║
   ║ Unless required  by applicable law  or agreed to in  writing, software  distributed under the  License is ║
   ║ distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. ║
   ║ See the License for the specific language governing permissions and limitations under the License.        ║
   ╚═══════════════════════════════════════════════════════════════════════════════════════════════════════════╝
*/
package fury.core

import fury.io._, fury.ogdl._, fury.model._

import scala.util._

import language.higherKinds

final class LayerRepository(revisions: LayerRevisions, current: Path) {

  def restorePrevious(io: Io, layout: Layout): Try[Unit] =
    for {
      previous <- revisions.previous(io, layout)
      _        <- Ogdl.write(previous, current)
      _        <- revisions.discardPrevious()
    } yield Unit

  def update(io: Io, layer: Layer, layout: Layout): Try[Unit] = currentLayer(io, layout) match {
    case None => Ogdl.write(layer, current)
    case Some(currentLayer) =>
      for {
        _ <- revisions.store(currentLayer)
        _ <- Ogdl.write(layer, current)
      } yield Unit
  }

  private def currentLayer(io: Io, layout: Layout): Option[Layer] =
    if(current.exists) Layer.read(io, current, layout).toOption
    else None
}

object LayerRepository {
  // TODO make configurable
  private val retainedRevisions = 16

  def apply(layout: Layout): LayerRepository = {
    val revisions = new LayerRevisions(layout.historyDir, retainedRevisions)
    new LayerRepository(revisions, layout.furyConfig)
  }
}
