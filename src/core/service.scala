/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.8.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.                                         ║
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

import fury.io._, fury.model._, fury.strings._

import euphemism._

import scala.util._
import guillotine._

object Service {
  def catalog(service: String)(implicit log: Log): Try[List[Artifact]] = {
    val url = Https(Path(service) / "catalog")
    
    for {
      bytes <- Http.get(url, Set())
      catalog <- Try(Json.parse(new String(bytes, "UTF-8")).get)
      artifacts <- Try(catalog.entries.as[List[Artifact]].get)
    } yield artifacts
  }

  def list(service: String, path: String)(implicit log: Log): Try[List[Artifact]] = {
    val url = Https(Path(service) / "list" / path)
    
    for {
      bytes <- Http.get(url, Set())
      catalog <- Try(Json.parse(new String(bytes, "UTF-8")).get)
      artifacts <- Try(catalog.entries.as[List[Artifact]].get)
    } yield artifacts
  }

  def latest(service: String, path: String, current: Option[LayerVersion])(implicit log: Log): Try[Artifact] =
    for {
      artifacts <- list(service, path)
      grouped   <- ~artifacts.groupBy(_.version.major)
      artifact  <- ~current.fold(grouped.maxBy(_._1)._2) { lv => grouped(lv.major) }.maxBy(_.version.minor)
    } yield artifact

  def publish(env: Environment, hash: String, path: String, quiet: Boolean, breaking: Boolean,
                  public: Boolean, major: Int, minor: Int, token: String)
             (implicit log: Log)
             : Try[PublishedLayer] = {

    val url = Https(Path(ManagedConfig().service) / "publish")
    case class Request(path: String, token: String, hash: String, breaking: Boolean, public: Boolean,
        major: Int, minor: Int)

    case class Response(major: Int, minor: Int)
    for {
      ipfs <- Ipfs.daemon(env, quiet)
      id   <- Try(ipfs.id().get)
      out  <- Http.post(url, Json(Request(path, token, hash, breaking, public, major, minor)), headers = Set())
      str  <- Success(new String(out, "UTF-8"))
      json <- Try(Json.parse(str).get)
      _    <- ~log.info(json.toString)
      res  <- Try(json.as[Response].get)
    } yield PublishedLayer(FuryUri(ManagedConfig().service, path), LayerVersion(res.major, res.minor))
  }
}