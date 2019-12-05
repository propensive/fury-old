/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.7.14. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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
package fury.utils

import fury.strings._

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

object Http {
  def post[T: Postable](url: String, content: T, headers: Set[HttpHeader]): Try[Array[Byte]] =
    request[T](url, content, "POST", headers)

  def get(url: String, params: Map[String, String], headers: Set[HttpHeader]): Try[Array[Byte]] =
    request(url, params, "POST", headers)

  def request[T: Postable](url: String, content: T, method: String, headers: Set[HttpHeader]): Try[Array[Byte]] = {
    new URL(url).openConnection match {
      case conn: HttpURLConnection =>
        conn.setRequestMethod(method)
        conn.setRequestProperty("Content-Type", implicitly[Postable[T]].contentType)
        conn.setRequestProperty("User-Agent", str"Fury ${Version.current}")
        headers.foreach { case HttpHeader(key, value) => conn.setRequestProperty(key, value) }
        conn.setDoOutput(true)
        val out = conn.getOutputStream()
        out.write(implicitly[Postable[T]].content(content))
        out.close()

        conn.getResponseCode match {
          case 200 =>
            val in = conn.getInputStream()
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
          case code => Failure(HttpException(code))
        }
    }
  }
}

