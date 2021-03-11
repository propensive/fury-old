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
package fury.text

import scala.concurrent._, duration._
import scala.util._
import scala.collection.mutable.HashSet

import scala.annotation._

sealed trait Event

object TaskId { def apply(): TaskId = TaskId(java.util.UUID.randomUUID().toString) }

case class TaskId(key: String)
case class Task[T](id: TaskId, name: Message, action: () => T)
case class Job(name: Message, thread: Thread)

case class StartTask(task: TaskId) extends Event
case class FinishTask(task: TaskId) extends Event
case class TaskProgress(task: TaskId, progress: Double) extends Event
case class RestartJob(job: Job) extends Event
case class FinishJob(job: Job) extends Event
case class LogMessage(msg: Message) extends Event
case object Shutdown extends Event

case class State(jobs: Map[Job, Dag[TaskId]] = Map(), progress: Map[TaskId, Double] = Map().withDefault { _ => 0 }) {
  def +(job: Job) = copy(jobs.updated(job, Dag()))
  def -(job: Job) = copy(jobs - job)
  def apply(job: Job): Dag[TaskId] = jobs(job)
  def update(job: Job, dag: Dag[TaskId]): State = copy(jobs.updated(job, dag))
  def setProgress(task: TaskId, complete: Double): State = copy(progress = progress.updated(task, complete))
}

trait Interest[T] { def predicate(value: T): Event => Boolean }

case class Tick(state: Option[State], events: List[Event])

object Bus {
  def apply(): State = Bus.synchronized(state)
  def put(event: Event): Unit = Bus.synchronized { receive(event) }

  def listen[T: Interest, S](value: T, interval: Int = 100)(block: Stream[Tick] => S): S = {
    val listener = new Listener(interval, implicitly[Interest[T]].predicate(value))
    register(listener)
    try block(listener.tickStream) finally unregister(listener)
  }

  def listen[S](interval: Int)(block: Stream[Tick] => S): S = {
    val listener = new Listener(interval, { _ => true })
    register(listener)
    try block(listener.tickStream) finally unregister(listener)
  }
  
  private implicit lazy val execCtx: ExecutionContext = ExecutionContext.global
  private val listeners: HashSet[Listener] = HashSet()
  private def register(listener: Listener): Unit = Bus.synchronized(listeners += listener)
  private def unregister(listener: Listener): Unit = Bus.synchronized(listeners -= listener)
  private var state: State = State()
  
  private def chunked[T](stream: Stream[T], interval: Int, t0: Long = System.currentTimeMillis, chunk: List[T] = Nil)
                     : Stream[List[T]] = {
    val window: Long = interval - (System.currentTimeMillis - t0)
    if(stream.isEmpty) Stream(chunk.reverse)
    else try chunked(stream.tail, interval, t0, Await.result(Future(blocking(stream)), window.milliseconds).head :: chunk)
    catch { case _: TimeoutException => chunk.reverse #:: chunked(stream, interval, System.currentTimeMillis) }
  }

  private class Listener(interval: Int, predicate: Event => Boolean) { listener =>
    def tickStream: Stream[Tick] = chunked(stream, interval).map { es => Tick(es.lastOption.map(_._2), es.map(_._1)) }

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
    case FinishJob(job)               => state - job
    case StartTask(task)              => state.setProgress(task, 0)
    case FinishTask(task)             => state.setProgress(task, 100)
    case Shutdown                     => state
    case TaskProgress(task, progress) => state.setProgress(task, progress)
    case LogMessage(str)              => state
    case RestartJob(job)              => state
  }
}