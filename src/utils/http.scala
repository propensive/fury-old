/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.7.14. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                        ║
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

import java.io._, java.net._, javax.net.ssl._

import scala.concurrent._

object Http {
  
  case class RequestProperty(key: String, value: String)

  case class Response(content: InputStream, status: Int)

  def get(url: String, properties: List[RequestProperty])(implicit ec: ExecutionContext): Future[Response] = {
    HttpURLConnection.setFollowRedirects(true)
    val conn = new URL(url).openConnection()
    conn.setConnectTimeout(10000)

    conn match {
      case conn: HttpsURLConnection =>
        conn.setRequestMethod("GET")
        properties.foreach { property => conn.setRequestProperty(property.key, property.value) }
        conn.setUseCaches(false)
        Future(Response(conn.getInputStream(), conn.getResponseCode()))
      case conn =>
        Future.failed(???)
    }
  }
}
