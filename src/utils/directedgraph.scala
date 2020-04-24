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
package fury.utils

import scala.annotation.tailrec

case class DirectedGraph[T](connections: Map[T, Set[T]]) {

  def remove(element: T): DirectedGraph[T] = {
    val pointingTo = connections(element)
    val noFromEdge = connections - element
    DirectedGraph(noFromEdge.mapValues { map => if(map(element)) map ++ pointingTo - element else map })
  }

  def subgraph(verticesToLeave: Set[T]): DirectedGraph[T] =
    (connections.keySet &~ verticesToLeave).foldRight(this) { (element, graph) => graph.remove(element) }

  def neighbours(start: T): Set[T] = connections.getOrElse(start, Set())
  def hasCycle(start: T): Boolean = findCycle(start).isDefined

  def findCycle(start: T): Option[List[T]] = {
    @tailrec
    def findCycleHelper(queue: List[(T, List[T])], finished: Set[T]): Option[List[T]] = queue match {
      case List() =>
        None
      case (vertex, trace) :: tail =>
        trace.toSet.intersect(neighbours(vertex)).headOption match {
          case Some(element) =>
            Some(trace ++ List(vertex, element))
          case None =>
            val queue = tail ++ neighbours(vertex).diff(finished).toList.map((_, trace :+ vertex))
            findCycleHelper(queue, finished + vertex)
        }
    }

    findCycleHelper(List((start, List())), Set())
  }

  def allDescendants(start: T): Either[List[T], Set[T]] = {

    @tailrec
    def allDescendantsHelper(stack: List[T], ans: Set[T]): Set[T] = stack match {
      case List()       => ans
      case head :: tail => allDescendantsHelper(neighbours(head).toList ++ tail, ans + head)
    }

    findCycle(start) match {
      case Some(cycle) => Left(cycle)
      case None        => Right(neighbours(start).flatMap(c => allDescendantsHelper(List(c), Set())))
    }
  }
}
