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

import java.nio.ByteBuffer
import java.nio.ByteBuffer.wrap

import probably._

import scala.language.implicitConversions

object OgdlSerializerTest extends TestApp {
  private[this] val empty = Ogdl(Vector())

  override def tests(): Unit = {
    test("Empty graph is a newline") {
      write(empty)
    }.assert(_ == "\n")

    test("Single node is terminated with a newline") {
      write(graph("A"))
    }.assert(_ == "A\n")

    test("Node name can be empty") {
      write(graph(""))
    }.assert(_ == "\n")

    test("Input with spaces is a single node") {
      write(graph("Some node with spaces"))
    }.assert(_ == "Some node with spaces\n")

    test("Trailing spaces are included in the node name") {
      write(graph("  name  "))
    }.assert(_ == "  name  \n")

    test("A child of previous node becomes an indented node on the same line") {
      write(graph("A" -> "B"))
    }.assert(_ == "A\tB\n")

    test("A sibling of previous node becomes a node on the next line") {
      write(graph("A", "B"))
    }.assert(_ == "AB\n")

    test("Sibling nodes have the same indentation level") {
      write(
          graph(
              "A" ->
                graph("B" -> graph("C", "D")),
              "E"
          ))
    }.assert(_ == "A\tB\tCD\nE\n")
  }

  private def write(node: Ogdl): String = Ogdl.serialize(node)

  implicit private[this] def buffer(string: String): ByteBuffer        = wrap(string.getBytes())
  implicit private[this] def graph(values: (String, Ogdl)*): Ogdl      = Ogdl(values.toVector)
  implicit private[this] def leafToTuple(name: String): (String, Ogdl) = name -> empty
  implicit private[this] def leafToGraph(name: String): Ogdl           = graph(name -> empty)
}
