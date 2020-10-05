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

  private val port = 6325

  implicit val log: Log = Log()

  private var Server: Option[LiveHttpServer] = None
    
  def start(env: Environment): Unit = Rest.synchronized {
    if(Server.isEmpty) {
      log.info(msg"Starting HTTP server on port $port")
      Server = new HttpServer({ request =>
        request.path match {
          case "/universe" =>

            def convert(universe: Universe): Api.Universe =
              Api.Universe(
                universe.projects.to[List].map { case (ProjectId(id), project) =>
                  Api.Project(id)
                },
                universe.repoSets.to[List].map { case (RepoSetId(id), repos) =>
                  Api.Repo(id, repos.to[List].map(_.repoId.key))
                },
                universe.imports.to[List].map { case (ShortLayerRef(id), LayerProvenance(ref, imports)) =>
                  Api.Import(id, ref.key, imports.to[Set].flatMap(_._2.remote.toSet).to[List])
                }
              )

            val result: Option[Api.Universe] = for {
              path      <- request.params.get("path")
              layout    <- Some(Layout(Path(env.variables("HOME")), Path(path), env, Path(path)))
              conf      <- Layer.readFuryConf(layout).toOption
              layer     <- Layer.get(conf.layerRef, None).toOption
              hierarchy <- layer.hierarchy(Pointer.Root).toOption
              universe  <- hierarchy.universe.toOption
            } yield convert(universe)

            Response(Json(result))
          case "/layer" =>
            val menu = FuryMenu.menu(Nil)
            val result: Option[Layer] = for {
              path   <- request.params.get("path")
              layout <- Some(Layout(Path(env.variables("HOME")), Path(path), env, Path(path)))
              conf   <- Layer.readFuryConf(layout).toOption
              layer  <- Layer.get(conf.layerRef, None).toOption
            } yield layer

            Response(Json(result))
        }
      }).bind(port).to[Option]
      if(Server.isEmpty) log.warn(msg"Failed to start HTTP server on port $port")
      log.info(msg"Started HTTP server")
    }
  }
  
  def shutdown(): Unit = {
    log.info(msg"Shutting down HTTP server on port $port")
    Server.map(_.shutdown())
    log.info(msg"Shutdown complete")
  }

  object Api {
    case class Envelope[T](request: String, result: Option[T], error: Option[String])
    case class Project(id: String)
    case class Repo(commit: String, ids: List[String])
    case class Import(id: String, ref: String, remotes: List[PublishedLayer])
    case class Universe(projects: List[Project], repos: List[Repo], imports: List[Import])
  }
}