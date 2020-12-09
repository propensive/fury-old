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
package fury

import fury.text._, fury.core._, fury.io._, fury.model._

import antiphony._
import euphemism._
import guillotine._
import mercator._

import scala.util._
import fury.core.Uniqueness.Ambiguous
import scala.concurrent._

object Rest {

  private val port = 6325

  implicit val log: Log = Log()

  private var Server: Option[LiveHttpServer] = None
    
  def start(env: Environment): Unit = Rest.synchronized {
    if(Server.isEmpty) {
      log.info(msg"Starting HTTP server on port $port")
      Server = new HttpServer({ request =>
        val json: Try[Json] = request.path match {
          case "/universe" => for {
            path        <- request.params.get("path").ascribe(MissingParam(Args.PathArg))
            layout      <- Try(Layout(Path(env.variables("HOME")), Path(path), env, Path(path)))
            conf        <- Layer.readFuryConf(layout)
            layer       <- Layer.get(conf.layerRef, None)
            hierarchy   <- layer.hierarchy(Pointer.Root)
            universe    <- hierarchy.universe
            apiUniverse <- Api.Universe(universe, layout)
          } yield Json(apiUniverse)
          
          case "/layer" => for {
            path      <- request.params.get("path").ascribe(MissingParam(Args.PathArg))
            layout    <- Try(Layout(Path(env.variables("HOME")), Path(path), env, Path(path)))
            conf      <- Layer.readFuryConf(layout)
            layer     <- Layer.get(conf.layerRef, None)
            hierarchy <- layer.hierarchy(Pointer.Root)
            universe  <- hierarchy.universe
            apiLayer  <- Api.Layer(universe, layout, layer)
          } yield Json(apiLayer)
          
          case "/hierarchy" => for {
            path         <- request.params.get("path").ascribe(MissingParam(Args.PathArg))
            layout       <- Try(Layout(Path(env.variables("HOME")), Path(path), env, Path(path)))
            conf         <- Layer.readFuryConf(layout)
            layer        <- Layer.get(conf.layerRef, None)
            hierarchy    <- layer.hierarchy(Pointer.Root)
            apiHierarchy <- Api.Hierarchy(hierarchy)
          } yield Json(apiHierarchy)
          
          case "/wait" => for {
            path      <- request.params.get("path").ascribe(MissingParam(Args.PathArg))
            layout    <- Try(Layout(Path(env.variables("HOME")), Path(path), env, Path(path)))
            timestamp <- Try(Await.result(Trigger.listen(layout.baseDir).future, duration.Duration.Inf))
          } yield Json(Api.Update(timestamp))
        }

        Response(Json(Api.Envelope(request.path, json)))
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
    case class Envelope(request: String, result: Option[Json], error: Option[String])
    case class Project(id: String, modules: List[Module], description: String)
    case class Repo(id: String, commit: String, remote: String, refSpec: String)
    case class RepoSet(commit: String, ids: List[String])
    case class ProjectRef(id: String, projects: List[Project])
    case class Import(id: String, ref: String, remotes: List[PublishedLayer])
    case class Hierarchy(id: String, layer: String, children: Option[List[Hierarchy]])
    case class Update(timestamp: Long)
    
    case class Module(
      id: String,
      sources: List[Source],
      dependencies: List[String],
      binaries: List[Binary],
      kind: String
    )

    case class Source(id: String, path: Option[String], editable: Boolean)
    case class Binary(id: String, group: String, artifact: String, version: String)
    case class Layer(id: String, repos: List[Repo], projects: List[Project])
    case class Universe(projects: List[ProjectRef], repos: List[RepoSet], imports: List[Import])

    object Envelope {
      def apply[T](request: String, result: Try[Json]): Envelope =
        Envelope(request, result.map(Json(_)).toOption,
        result.failed.toOption.map {
          case e: Exception => "An unexpected error occurred"
        })
    }

    object Project {
      def apply(project: fury.core.Project, universe: fury.core.Universe, layout: Layout): Project =
        Project(
          project.id.key,
          project.modules.to[List].map(Module(_, project, universe, layout)),
          project.description)
    }

    object Module {
      def apply(module: fury.core.Module,
                project: fury.core.Project,
                universe: fury.core.Universe,
                layout: Layout): Module =
        Module(
          id = module.id.key,
          sources = module.sources.to[List].map { s =>
            val dir = for {
              checkout <- universe.checkout(module.ref(project), layout)
              dir      <- s.dir(checkout, layout)
            } yield dir.value
            Source(s.key, dir.toOption, s.local)
          },
          dependencies = module.dependencies.to[List].map(_.key),
          binaries = module.binaries.to[List].map { b => Binary(b.id.key, b.group, b.artifact, b.version) },
          module.kind.name.name
        )
        
    }

    object Universe {
      def apply(universe: fury.core.Universe, layout: Layout): Try[Universe] = for {
        projects <- universe.projects.to[List].traverse { case (ProjectId(id), project) =>
                      universe.projects.to[List].traverse { case (projectId, elements) => for {
                        layer <- universe.hierarchy(elements.some)
                        project <- layer.projects.findBy(projectId)
                      } yield Project(project, universe, layout) }.map(ProjectRef(id, _))
                    }
      } yield Universe(
          projects,
          universe.repoSets.to[List].map { case (RepoSetId(id), repos) =>
            RepoSet(id, repos.to[List].map(_.repoId.key))
          },
          universe.imports.to[List].map { case (ShortLayerRef(id), LayerProvenance(ref, imports)) =>
            Import(id, ref.key, imports.to[Set].flatMap(_._2.remote.toSet).to[List])
          }
        )
    }
    
    object Hierarchy {
      def apply(hierarchy: fury.core.Hierarchy): Try[Hierarchy] = for {
        ref      <- hierarchy.layer.ref
        children <- hierarchy.children.values.to[List].traverse(Hierarchy(_))
      } yield Hierarchy(hierarchy.path.lastOption.map(_.key).getOrElse("/"), ref.key,
          if(children.isEmpty) None else Some(children))
    }

    object Layer {
      def apply(universe: fury.core.Universe, layout: Layout, layer: fury.core.Layer): Try[Layer] = for {
        ref <- layer.ref
      } yield Layer(ref.key, layer.repos.to[List].map { r => Repo(r.id.key, r.commit.key, r.remote.ref,
          r.branch.id) }, layer.projects.to[List].map(Project(_, universe, layout)))
    }
  }
}