package fury.error

import scala.reflect._
import scala.util._

trait FuryException extends Exception

object Outcome {

  class Rescue[E <: Exception](val error: E => FuryException) {
    def apply[T](fn: => T)(implicit classTag: ClassTag[E]): Try[T] =
      try Success(fn) catch { case e: E => Failure(error(e)) }
  }

  def rescue[E <: Exception](error: E => FuryException): Rescue[E] = new Rescue[E](error)

  def rescue[E <: Exception](error: FuryException): Rescue[E] = new Rescue[E]({ e: Exception => error })
}

object `package` {
  type Outcome[+T] = Try[T]
  implicit class Unitize[T](t: T) { def unit: Unit = () }
  implicit class AutoRight[T](t: T) { def unary_~ : Try[T] = Success(t) }
  implicit class Ascribe[T](value: Option[T]) { def ascribe(e: Exception): Outcome[T] = value.map(Success(_)).getOrElse(Failure(e)) }
}
