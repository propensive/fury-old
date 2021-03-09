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
import scala.collection.mutable.HashSet

import scala.annotation._

sealed trait Event
case class LogMessage(str: Message) extends Event

object TaskId { def apply(): TaskId = TaskId(java.util.UUID.randomUUID().toString) }

case class TaskId(key: String)
case class Task(id: TaskId, name: Message)
case class Job(name: Message, thread: Thread)

case class StartJob(job: Job) extends Event
case class StartTask(task: TaskId) extends Event
case class AbortJob(job: Job) extends Event
case class FinishTask(job: Job, task: Task) extends Event
case class TaskProgress(task: TaskId, progress: Int) extends Event
case class FinishJob(job: Job) extends Event
case object Shutdown extends Event

case class State(jobs: Map[Job, Dag[TaskId]] = Map(), progress: Map[TaskId, Int] = Map().withDefault { _ => 0 }) {
  def +(job: Job) = copy(jobs.updated(job, Dag()))
  def -(job: Job) = copy(jobs - job)
  def apply(job: Job): Dag[TaskId] = jobs(job)
  def update(job: Job, dag: Dag[TaskId]): State = copy(jobs.updated(job, dag))
  def setProgress(task: TaskId, complete: Int): State = copy(progress = progress.updated(task, complete))
}

trait Interest[T] { def predicate(value: T): Event => Boolean }

case class Tick(state: Option[State], events: List[Event])

object Bus {
  final val interval: Long = 100L
  def apply(): State = Bus.synchronized(state)
  def put(event: Event): Unit = Bus.synchronized { receive(event) }

  def listen[T: Interest, S](value: T)(block: Stream[Tick] => S): S = {
    val listener = new Listener(implicitly[Interest[T]].predicate(value))
    register(listener)
    try block(listener.tickStream) finally unregister(listener)
  }

  private implicit lazy val execCtx: ExecutionContext = ExecutionContext.global
  private val listeners: HashSet[Listener] = HashSet()
  private def register(listener: Listener): Unit = Bus.synchronized(listeners += listener)
  private def unregister(listener: Listener): Unit = Bus.synchronized(listeners -= listener)
  private var state: State = State()
  
  private def chunked[T](stream: Stream[T], t0: Long = System.currentTimeMillis, chunk: List[T] = Nil)
                     : Stream[List[T]] = {
    val window: Long = interval - (System.currentTimeMillis - t0)
    if(stream.isEmpty) Stream(chunk.reverse)
    else try chunked(stream.tail, t0, Await.result(Future(blocking(stream)), window.milliseconds).head :: chunk)
    catch { case _: TimeoutException => chunk.reverse #:: chunked(stream, System.currentTimeMillis) }
  }

  private class Listener(predicate: Event => Boolean) { listener =>
    def tickStream: Stream[Tick] = chunked(stream).map { es => Tick(es.lastOption.map(_._2), es.map(_._1)) }

    def put(event: Event, state: State): Unit = if(predicate(event)) {
      promise.complete(Success((event, state)))
      promise = Promise()
    }
    
    def stream: Stream[(Event, State)] = Await.result(promise.future, Duration.Inf) #:: stream
    private[this] var promise: Promise[(Event, State)] = Promise()
  }

  private def receive(event: Event): Unit = synchronized {
    state = update(state, event)
    listeners.foreach { listener => listener.put(event, state) }
  }

  private def update(state: State, event: Event): State = event match {
    case StartJob(job)                => state + job
    case FinishJob(job)               => state - job
    case StartTask(task)              => state.setProgress(task, 0)
    case AbortJob(job)                => state
    case Shutdown                     => state
    case FinishTask(job, task)        => state
    case TaskProgress(task, progress) => state.setProgress(task, progress)
    case LogMessage(str)              => state
  }
}