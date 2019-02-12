/*
  Fury, version 0.4.0. Copyright 2018-19 Jon Pretty, Propensive Ltd.

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

import java.nio.ByteBuffer
import java.nio.ByteBuffer.wrap

import probably._

import scala.language.implicitConversions

object DirectedGraphTest extends TestApp {

  override def tests(): Unit = {
    test("Graph with no edges has no cycles") {
      DirectedGraph[Int](Map(1 -> Set()))
    }.assert(!_.hasCycle(1))

    test("Single vertex pointing to itself is a cycle") {
      DirectedGraph[Int](Map(1 -> Set(1)))
    }.assert(_.hasCycle(1))

    test("Two vertices pointing to each other build a cycle") {
      DirectedGraph[Int](Map(1 -> Set(2), 2 -> Set(1)))
    }.assert(_.hasCycle(1))

    test("Two vertices with one connection") {
      DirectedGraph[Int](Map(1 -> Set(2)))
    }.assert(!_.hasCycle(1))

    test("Find a cycle not containing startpoint") {
      DirectedGraph(Map(1 -> Set(2), 2 -> Set(3), 3 -> Set(4), 4 -> Set(2)))
    }.assert(_.findCycle(1) == Some(List(1, 2, 3, 4, 2)))

    test("Diamond without cycles") {
      DirectedGraph(Map(1 -> Set(2, 3), 2 -> Set(4), 3 -> Set(4)))
    }.assert(!_.hasCycle(1))

    test("single element") {
      DirectedGraph(Map(1 -> Set.empty[Int])).subgraph(Set(1))
    }.assert(_ == DirectedGraph(Map(1 -> Set.empty[Int])))

    test("two separate elements") {
      val input = DirGraph(Map(1 -> Set(2), 2 -> Set.empty[Int]))
      input.subgraph(Set(1, 2))
    }.assert { _ == DirGraph(Map(1 -> Set(2), 2 -> Set.empty[Int])) }

    test("two related elements") {
      val input = DirectedGraph(Map(1 -> Set(2), 2 -> Set.empty[Int]))
      input.subgraph(Set(1))
    }.assert { _ == DirectedGraph(Map(1 -> Set.empty[Int])) }

    test("remove two elements") {
      val input = DirectedGraph(Map(1 -> Set(2), 2 -> Set(3), 3 -> Set.empty[Int]))
      input.subgraph(Set(1))
    }.assert { _ == DirectedGraph(Map(1 -> Set.empty[Int])) }

    test("remove one element of three") {
      val input = DirectedGraph(Map(1 -> Set(2), 2 -> Set(3), 3 -> Set.empty[Int]))
      input.subgraph(Set(1, 2))
    }.assert { _ == DirectedGraph(Map(1 -> Set(2), 2 -> Set.empty[Int])) }

    test("remove two element of four") {
      val input =
        DirectedGraph(Map(1 -> Set(2, 4), 2 -> Set(3, 4), 3 -> Set(4), 4 -> Set.empty[Int]))
      input.subgraph(Set(1, 2))
    }.assert { _ == DirectedGraph(Map(1 -> Set(2), 2 -> Set.empty[Int])) }

  }
}
