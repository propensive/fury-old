/*
  Fury, version 0.1.2. Copyright 2018 Jon Pretty, Propensive Ltd.

  The primary distribution site is: https://propensive.com/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required  by applicable  law or  agreed to  in writing,  software  distributed  under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
  express  or  implied.  See  the  License for  the specific  language  governing  permissions and
  limitations under the License.
                                                                                                  */
package fury

sealed trait Json { def serialize: String }

case class JsonObject(elements: (String, Json)*) extends Json {
  def serialize: String = elements.map { case (k, v) =>
    s"""${JsonString(k).serialize}: ${v.serialize}"""
  }.join("{ ", ", ", " }")
}

case class JsonArray(elements: Json*) extends Json {
  def serialize: String = elements.map(_.serialize).join("[", ", ", "]")
}

case class JsonDecimal(number: BigDecimal) extends Json {
  def serialize: String = number.toString
}

case class JsonString(string: String) extends Json {
  def serialize: String =
    "\""+string.replaceAll("\n", "\\n").replaceAll("\r", "\\r").replaceAll("\"", "\\\"")+"\""
}

case object JsonNull extends Json {
  def serialize: String = "null"
}

case class JsonBoolean(boolean: Boolean) extends Json {
  def serialize: String = boolean.toString
}
