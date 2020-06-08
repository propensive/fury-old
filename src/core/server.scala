package fury.core

import domain._

import fury.model._, fury.core._, fury.io._, fury.text._

import antiphony._
import euphemism._
import quarantine._
import gastronomy._

object ApiServer {
  val server = HttpServer { request =>

    val response: Result[ApiResponse] = for {
      json       <- Json.parse(new String(request.body.foldLeft(Bytes(Array[Byte]()))(_ ++ _).to[Array])).adapt
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
case class Token(start: Int, end: Int, kind: String)
case class Markup(counter: Int, points: List[Token]) extends ApiResponse

sealed abstract class ApiRequest() { def response(): domain.Result[ApiResponse] }

case class CompileFragment(counter: Int, code: String) extends ApiRequest() {
  def response(): domain.Result[ApiResponse] = {

    def scan(str: String,
             idx: Int = 0,
             tokens: List[Token] = Nil,
             start: Int = 0,
             space: Boolean = false,
             polarity: Boolean = false): List[Token] =
      if(idx >= str.length) tokens else str(idx) match {
        case ' ' =>
          if(space) scan(str, idx + 1, tokens, start, true, polarity)
          else scan(str, idx + 1, Token(start, idx, "space") :: tokens, idx, true, polarity)
        case ch  =>
          if(space) scan(str, idx + 1, Token(start, idx, if(polarity) "word" else "word2") :: tokens, idx, false, !polarity)
          else scan(str, idx + 1, tokens, start, false, polarity)
      }

    domain.Answer(Markup(counter, scan(code).reverse))
  }
}

case class ShowProjects(layer: LayerName) extends ApiRequest() {
  def response(): domain.Result[ApiResponse] = domain.Answer(LongRunning())
}