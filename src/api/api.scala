/*

    Fury, version 0.16.1. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.api

import domain._

import fury.model._, fury.core._, fury.io._, fury.text._

import antiphony._
import euphemism._
import quarantine._

object ApiServer {
  val server = HttpServer { request =>
    
    val response: Result[ApiResponse] = for {
      json       <- Json.parse(new String(request.content)).adapt
      apiRequest <- json.as[ApiRequest].adapt
      response   <- apiRequest.response()
    } yield response

    response.map { r => Response(Json(r)) }.recover { error =>
      Response(Json.of(failure = true))
    }
  }
}

sealed trait ApiResponse
case class LongRunning() extends ApiResponse
case class TabularData() extends ApiResponse

sealed abstract class ApiRequest() { def response(): domain.Result[ApiResponse] }

case class CompileFragment(layer: LayerName, code: String) extends ApiRequest() {
  def response(): domain.Result[ApiResponse] = domain.Answer(LongRunning())
}

case class ShowProjects(layer: LayerName) extends ApiRequest() {
  def response(): domain.Result[ApiResponse] = domain.Answer(LongRunning())
}