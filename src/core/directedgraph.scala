package fury
import scala.annotation.tailrec

case class DirectedGraph[T](connections: Map[T, Set[T]]) {

  def remove(element: T): DirectedGraph[T] = {
    val pointingTo = connections(element)
    val noFromEdge = connections - element
    DirectedGraph(noFromEdge.mapValues { x =>
      if (x(element)) x ++ pointingTo - element else x
    })
  }

  def subgraph(verticesToLeave: Set[T]): DirectedGraph[T] = {
    val toCut = connections.keySet &~ verticesToLeave
    toCut.foldRight(this) { (element, graph) =>
      graph.remove(element)
    }
  }

  def neighbours(start: T): Set[T] =
    connections.getOrElse(start, Set())

  def findCycle(start: T): Option[List[T]] = {
    @tailrec
    def findCycleHelper(queue: List[(T, List[T])], finished: Set[T]): Option[List[T]] =
      queue match {
        case List() => None
        case (vertex, trace) :: tail => {
          val commonElement = trace.toSet.intersect(neighbours(vertex)).headOption
          commonElement match {
            case Some(element) => Some(trace ++ List(vertex, element))
            case None =>
              findCycleHelper(
                  tail ++ neighbours(vertex).diff(finished).toList.map((_, trace :+ vertex)),
                  finished + vertex)
          }
        }
      }

    findCycleHelper(List((start, List())), Set())
  }

  def hasCycle(start: T): Boolean = findCycle(start).isDefined

  def allDescendants(start: T): Either[List[T], Set[T]] = {
    @tailrec
    def allDescendantsHelper(stack: List[T], ans: Set[T]): Set[T] =
      stack match {
        case List()       => ans
        case head :: tail => allDescendantsHelper(neighbours(head).toList ++ tail, ans + head)
      }

    findCycle(start) match {
      case Some(cycle) => Left(cycle)
      case None        => Right(neighbours(start).flatMap(c => allDescendantsHelper(List(c), Set())))
    }
  }

}
