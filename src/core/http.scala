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

import fury.strings._, fury.model._, fury.io._

import euphemism._

import scala.concurrent._
import scala.util._
import scala.annotation.tailrec

import java.net._
import java.io._

case class HttpException(code: Int) extends Exception

object Postable {
  implicit object string extends Postable[String]("text/plain") {
    def content(value: String): Array[Byte] = value.bytes
  }

  implicit object map extends Postable[Map[String, String]]("multipart/form-data") {
    def content(value: Map[String, String]): Array[Byte] =
      value.map { case (key, value) => str"${key.urlEncode}=${value.urlEncode}" }.join("&").bytes
  }

  implicit object json extends Postable[Json]("application/json") {
    def content(value: Json): Array[Byte] = value.toString.bytes
  }

}

abstract class Postable[T](val contentType: String) { def content(value: T): Array[Byte] }

case class HttpHeader(key: String, value: String)

object Https { def apply(path: Path, query: Query = Query.empty): Uri = Uri("https", path, query) }

object Http {

  def apply(path: Path): Uri = Uri("http", path)

  def post[T: Postable](url: Uri, content: T, headers: Set[HttpHeader])(implicit log: Log): Try[Array[Byte]] =
    request[T](url, content, "POST", headers)

  def get(uri: Uri, headers: Set[HttpHeader])
         (implicit log: Log)
         : Try[Array[Byte]] =
    request(uri, Map.empty[String, String], "GET", headers)

  private def request[T: Postable]
                     (url: Uri, content: T, method: String, headers: Set[HttpHeader])
                     (implicit log: Log)
                     : Try[Array[Byte]] = {
    requestStream[T](url, content, method, headers).flatMap { in =>
      val data = new ByteArrayOutputStream()
      val buf = new Array[Byte](65536)
      
      @tailrec
      def read(): Array[Byte] = {
        val bytes = in.read(buf, 0, buf.length)
        if(bytes < 0) data.toByteArray else {
          data.write(buf, 0, bytes)
          read()
        }
      }  
      
      Try(read())
    }
  }

  def requestStream[T: Postable]
                   (url: Uri, content: T, method: String, headers: Set[HttpHeader])
                   : Try[InputStream] = {
    new URL(url.key).openConnection match {
      case conn: HttpURLConnection =>
        conn.setRequestMethod(method)
        conn.setConnectTimeout(10000)
        conn.setReadTimeout(10000)
        
        if(method == "POST" || method == "PUT")
          conn.setRequestProperty("Content-Type", implicitly[Postable[T]].contentType)
        
        conn.setRequestProperty("User-Agent", str"Fury ${FuryVersion.current}")
        headers.foreach { case HttpHeader(key, value) => conn.setRequestProperty(key, value) }
        
        (method match {
          case "GET" | "HEAD" => Success(())
          case "POST" | "PUT" =>
            conn.setDoOutput(true)
            Try(conn.getOutputStream()).recoverWith {
              case ex: UnknownHostException =>
                Failure(DnsLookupFailure(ex.getMessage))
            }.flatMap { out =>
              out.write(implicitly[Postable[T]].content(content))
              out.close()
              Success(())
            }
        }).flatMap { _ =>
          conn.getResponseCode match {
            case 200  => Success(conn.getInputStream())
            case 400  => Failure(HttpBadRequest(url))
            case 401  => Failure(HttpUnauthorized(url))
            case 403  => Failure(HttpForbidden(url))
            case 404  => Failure(HttpNotFound(url))
            case 405  => Failure(HttpMethodNotAllowed(url))
            case 500  => Failure(HttpInternalServerError(url))
            case 501  => Failure(HttpNotImplemented(url))
            case 502  => Failure(HttpBadGateway(url))
            case 503  => Failure(HttpServiceUnavailable(url))
            case 504  => Failure(HttpGatewayTimeout(url))
            case code => Failure(HttpError(code, url))
          }
        }
    }
  }
}

