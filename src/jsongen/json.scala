/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.7.10. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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
package fury.jsongen

import fury.strings._

import language.dynamics, language.implicitConversions

sealed trait Json { def serialize: String }

object JsonEncoder {
  implicit val int: JsonEncoder[Int]               = JsonDecimal(_)
  implicit val long: JsonEncoder[Long]             = JsonDecimal(_)
  implicit val double: JsonEncoder[Double]         = JsonDecimal(_)
  implicit val boolean: JsonEncoder[Boolean]       = JsonBoolean(_)
  implicit val string: JsonEncoder[String]         = JsonString(_)
  implicit val nil: JsonEncoder[Iterable[Nothing]] = xs => JsonArray(Nil)

  implicit def iterable[T: JsonEncoder]: JsonEncoder[Iterable[T]] =
    xs => JsonArray(xs.map(implicitly[JsonEncoder[T]].encode))
}
trait JsonEncoder[-T] { def encode(value: T): Json }

object Json extends Dynamic {

  def applyDynamicNamed(methodName: String)(elements: (String, Json)*): JsonObject =
    JsonObject(elements.filterNot(_._2 == Undefined))

  implicit def encode[T: JsonEncoder](value: T): Json = implicitly[JsonEncoder[T]].encode(value)
}

case object Undefined extends Json {
  def serialize: String = throw new IllegalStateException("Cannot serialize an undefined value to JSON")
}

case class JsonObject(elements: Iterable[(String, Json)]) extends Json {

  def serialize: String =
    elements.map {
      case (k, v) =>
        s"""${JsonString(k).serialize}: ${v.serialize}"""
    }.join("{ ", ", ", " }")
}

case class JsonArray(elements: Iterable[Json]) extends Json {
  def serialize: String = elements.map(_.serialize).join("[", ", ", "]")
}

case class JsonDecimal(number: BigDecimal) extends Json {
  def serialize: String = number.toString
}

case class JsonString(string: String) extends Json {

  def serialize: String =
    "\"" + string.replaceAll("\n", "\\n").replaceAll("\r", "\\r").replaceAll("\"", "\\\"") + "\""
}

case object JsonNull extends Json {
  def serialize: String = "null"
}

case class JsonBoolean(boolean: Boolean) extends Json {
  def serialize: String = boolean.toString
}
