/*

    Fury, version 0.18.9. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury

import fury.text._, fury.core._, fury.io._, fury.model._

import antiphony._
import euphemism._
import guillotine._

object Rest {

  implicit val log: Log = Log()

  private var Server: Option[LiveHttpServer] = None
    
  def start(env: Environment): Unit = if(Server.isEmpty) {
    Server = new HttpServer({ request =>
      val menu = FuryMenu.menu(Nil)
      val result: Option[Layer] = for {
        path   <- request.params.get("path")
        layout <- Some(Layout(Path(env.variables("HOME")), Path(path), env, Path(path)))
        conf   <- Layer.readFuryConf(layout).toOption
        layer  <- Layer.get(conf.layerRef, None).toOption
      } yield layer

      Response(Json(result))
    }).bind(6325).to[Option]
  }
  
  def shutdown(): Unit = Server.map(_.shutdown())
}