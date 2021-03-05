/*

    Fury, version 0.33.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.utils

import fury.text._

import scala.concurrent._, duration._
import scala.util._

class Activity[T](initial: => Try[T], onSuccess: T => Unit, stopOnSuccess: Boolean)(implicit exec: ExecutionContext) { activity =>
  private var last: Option[T] = None
  private var continue: Boolean = true
  private var state = State(mkFuture(initial), None, Vector())
  private val promise: Promise[Unit] = Promise()
  
  def complete(): Unit = try promise.complete(Success(())) catch { case _: Exception => () }
  def abort(): Unit = continue = false
  def await(): Unit = Await.result(promise.future, Duration.Inf)
  def addTask(fn: => Unit): Unit = update(state.addTask(fn))
  def enqueue(fn: Option[T] => Try[T]): Unit = update(state.enqueue(fn))

  def mkFuture[S](fn: => S): Future[S] = {
    val future = Future(fn)
    future.andThen { case _ => update(state) }
    future
  }
  
  def update(newState: => State): Unit =
    if(continue) activity.synchronized { state = newState.proceed() } else complete()
  
  case class State(current: Future[Try[T]], enqueued: Option[Option[T] => Try[T]], tasks: Vector[() => Unit]) {
    def addTask(fn: => Unit): State = copy(tasks = tasks :+ { () => fn })
    def enqueue(fn: Option[T] => Try[T]): State = copy(enqueued = Some(fn))
    
    def proceed(): State = current.value.fold(this) { value =>
      if(stopOnSuccess && tasks.isEmpty) {
        complete()
        this
      } else {
        value.flatten.foreach { v =>
          onSuccess(v)
          last = Some(v)
        }
        if(tasks.isEmpty) copy(enqueued.fold(current) { t => mkFuture(t(last)) }, enqueued = None)
        else copy(current.andThen { case _ => mkFuture(tasks.foreach(_())) }, tasks = Vector())
      }
    }
  }
}