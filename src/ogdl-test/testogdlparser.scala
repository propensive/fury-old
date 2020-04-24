/*

    Fury, version 0.15.1. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.ogdl

import fury.ogdl._

import fury.ogdl._

import java.nio.ByteBuffer
import java.nio.ByteBuffer.wrap

import probably._

import scala.language.implicitConversions

object OgdlParserTest extends TestApp {
  private[this] val empty = Ogdl(Vector())

  override def tests(): Unit = {
    test("Not terminated input is an empty graph") {
      read("ignored")
    }.assert(_ == empty)

    test("Input terminated with new line is a node name") {
      read("A\n")
    }.assert(_ == graph("A"))

    test("Input terminated with tabulator is a node name") {
      read("A\t")
    }.assert(_ == graph("A"))

    test("Node name can be empty") {
      read("\t")
    }.assert(_ == graph(""))

    test("Input with spaces is a single node") {
      read("Some node with spaces\n")
    }.assert(_ == graph("Some node with spaces"))

    test("Trailing spaces are included in the node name") {
      read("  name  \n")
    }.assert(_ == graph("  name  "))

    test("Indented node on the same line is a child of previous node") {
      read("A\tB\n")
    }.assert(_ == graph("A" -> "B"))

    test("Indented node on the next line is a child of previous node") {
      read("A\n\tB\n")
    }.assert(_ == graph("A" -> "B"))

    test("Comments at the start are skipped") {
      read("#comment\nA\n\tB\n")
    }.assert(_ == graph("A" -> "B"))

    test("Comments at the end are skipped") {
      read("A\n\tB\n#comment")
    }.assert(_ == graph("A" -> "B"))

    test("Comments in the middle are skipped") {
      read("A\n#comment\n\tB\n")
    }.assert(_ == graph("A" -> "B"))

    test("Node on the next line is a sibling of previous node") {
      read("A\nB\n")
    }.assert(_ == graph("A", "B"))

    test("Nodes on the same indentation level are siblings") {
      read("A\n\tB\n\t\tC\n\t\tD\nE\n")
    }.assert(
        _ == graph(
            "A" ->
              graph("B" -> graph("C", "D")),
            "E"
        ))
  }

  private[this] def read(string: String): Ogdl = OgdlParser.parse(string)

  implicit private[this] def buffer(string: String): ByteBuffer        = wrap(string.getBytes())
  implicit private[this] def graph(values: (String, Ogdl)*): Ogdl      = Ogdl(values.toVector)
  implicit private[this] def leafToTuple(name: String): (String, Ogdl) = name -> empty
  implicit private[this] def leafToGraph(name: String): Ogdl           = graph(name -> empty)
}
