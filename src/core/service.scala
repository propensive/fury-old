/*

    Fury, version 0.18.15. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

import fury.io._, fury.model._, fury.text._

import euphemism._
import antiphony._

import scala.util._
import guillotine._

object Service {
  def catalog(service: DomainName)(implicit log: Log): Try[List[String]] = {
    val url: Uri = Https(service) / "catalog"
    
    for {
      _         <- ~log.note(msg"Sending GET request to $url")
      bytes     <- Http.get(url.key, Set()).to[Try]
      catalog   <- Json.parse(new String(bytes, "UTF-8")).to[Try]
      _         <- ~log.note(msg"Response: $catalog")
      artifacts <- catalog.entries.as[List[String]].to[Try]
    } yield artifacts
  }

  def list(service: DomainName, path: String)(implicit log: Log): Try[List[Artifact]] = {
    val url = Https(service) / "list" / path
    
    for {
      _       <- ~log.note(msg"Sending GET request to $url")
      bytes   <- Http.get(url.key, Set()).to[Try]
      json    <- Json.parse(new String(bytes, "UTF-8")).to[Try]
      _       <- ~log.note(msg"Response: $json")
      catalog <- handleError[Catalog](json)
    } yield catalog.entries
  }

  def latest(service: DomainName, path: String, current: Option[FullVersion])
            (implicit log: Log)
            : Try[Artifact] =
    for {
      artifacts <- list(service, path)
      grouped   <- ~artifacts.groupBy(_.version.major)
      artifact  <- if(grouped.size == 0) Failure(UnknownLayer(path, service))
                   else ~current.fold(grouped.maxBy(_._1)._2) { lv => grouped(lv.major) }.maxBy(_.version.minor)
    } yield artifact

  def fetch(service: DomainName, path: String, version: LayerVersion)
            (implicit log: Log)
            : Try[Artifact] =
    for {
      artifacts <- list(service, path)
      stream    <- artifacts.groupBy(_.version.major).get(version.major).ascribe(UnknownVersion(version))
      artifact  <- version.minor.fold(~stream.maxBy(_.version.minor)) { v =>
                     stream.find(_.version.minor == v).ascribe(UnknownVersion(version))
                   }
    } yield artifact

  def share(service: DomainName, ref: IpfsRef, token: OauthToken, dependencies: Set[IpfsRef], ttl: Int)
           (implicit log: Log)
           : Try[Unit] = {
    
    val url = Https(service) / "share"

    case class Request(refs: List[String], token: String, ttl: Int)
    case class Response(refs: List[String], expiry: Long)

    val request = Json(Request((ref :: dependencies.to[List]).map(_.key), token.value, ttl))

    for {
      _    <- ~log.note(msg"Sending POST request to $url")
      _    <- ~log.note(msg"Request: $request")
      out  <- Http.post(url.key, request, headers = Set()).to[Try]
      str  <- Success(new String(out, "UTF-8"))
      json <- Json.parse(str).to[Try]
      _    <- ~log.note(msg"Response $json")
      res  <- handleError[Response](json)
    } yield ()
  }

  def tag(service: DomainName,
          hash: IpfsRef,
          group: Option[String],
          name: String,
          breaking: Boolean,
          public: Boolean,
          major: Int,
          minor: Int,
          description: Option[String],
          ttl: Int,
          token: OauthToken)
         (implicit log: Log)
         : Try[PublishedLayer] = {

    val url = Https(service) / "tag"
    
    case class Request(ref: String, token: String, major: Int, minor: Int, organization: String, name: String,
        public: Boolean, breaking: Boolean, ttl: Option[Int], description: Option[String])

    case class Response(path: String, ref: String, version: FullVersion)

    val request = Json(Request(hash.key, token.value, major, minor, group.getOrElse(""), name, public, breaking,
        Some(ttl), description))
    
    for {
      _    <- ~log.note(msg"Sending POST request to $url")
      _    <- ~log.note(msg"Request: $request")
      out  <- Http.post(url.key, request, headers = Set()).to[Try]
      str  <- Success(new String(out, "UTF-8"))
      json <- Json.parse(str).to[Try]
      _    <- ~log.note(msg"Response: $json")
      res  <- handleError[Response](json)
    } yield PublishedLayer(FuryUri(ManagedConfig().service, res.path), res.version, LayerRef(res.ref))
  }
  
  import Json._

  def handleError[R: Deserializer](json: Json): Try[R] = json.as[R].to[Option].map(Try(_)).getOrElse {
    json.as[ServiceException].to[Option] match {
      case Some(v) => Failure(v)
      case None    => Failure(UnexpectedError("Could not parse JSON response"))
    }
  }
  
}

sealed abstract class ServiceException(msg: String) extends Exception(msg) with Product with Serializable

case class GithubUserDetailsError(code: Int) extends ServiceException(
    str"Could not fetch the information about the user due to HTTP error code $code")

case class PinataAddHashFailure() extends ServiceException("Could not pin the hash to IPFS service")
case class PinataPinFailure() extends ServiceException("Could not pin the file to IPFS service")
case class InvalidPrefix(prefix: String) extends ServiceException(str"The name prefix $prefix is not yours")

case class InvalidNameFormat(name: String) extends ServiceException(
    str"The name $name is not in the right format")

case class InvalidVersion() extends ServiceException(s"The version number is not valid")
case class InvalidOrganization(organization: String, valid: List[String]) extends ServiceException(s"The group name is not valid for publishing. Valid groups are: ${valid.mkString(", ")}")
case class UnexpectedError(msg: String) extends ServiceException(msg)
case class InputError() extends ServiceException("The input data was not in the correct format")

case class ThirdPartyError() extends ServiceException(
    "There was an invalid interaction with a third-party service")

case class NameNotFound(name: String) extends ServiceException(s"A public layer with the name '${name}' was not found")
