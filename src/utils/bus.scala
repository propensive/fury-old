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


sealed trait Event
case class LogMessage(str: Message) extends Event
case object Tick extends Event
case class Task(name: Message)
case class Job(name: Message, thread: Thread)

case class StartJob(job: Job) extends Event
case class StartTask(job: Job, task: Task) extends Event
case class AbortTask(job: Job, task: Task) extends Event
case class FinishTask(job: Job, task: Task) extends Event
case class TaskProgress(job: Job, task: Task, progress: Int) extends Event
case class FinishJob(job: Job) extends Event

case class State(jobs: Map[Job, Dag[Task]] = Map(), progress: Map[Task, Int] = Map().withDefault { _ => 0 }) {
  def +(job: Job) = copy(jobs.updated(job, Dag()))
  def -(job: Job) = copy(jobs - job)
  def apply(job: Job): Dag[Task] = jobs(job)
  def update(job: Job, dag: Dag[Task]): State = copy(jobs.updated(job, dag))
  def setProgress(task: Task, complete: Int): State = copy(progress = progress.updated(task, complete))
}

trait Interest[T] { def predicate(value: T): Event => Boolean }

case class Update(state: State, event: Event)

object Bus {

  

  def chunkStream[T](stream: Stream[T], t0: Long = System.currentTimeMillis, chunk: List[T] = Nil)
                    (implicit ec: ExecutionContext): Stream[List[T]] = {
    val remaining: Long = 1000L - (System.currentTimeMillis - t0)
    try chunkStream(stream.tail, t0, Await.result(Future(blocking(stream)), remaining.milliseconds).head :: chunk)
    catch { case _: TimeoutException => chunk.reverse #:: chunkStream(stream.tail, System.currentTimeMillis) }
  }

  def apply(): State = Bus.synchronized(state)
  def put(event: Event): Unit = Bus.synchronized { receive(event) }

  def listen[T: Interest, S](value: T, interval: Duration = 100.milliseconds)(block: Stream[Update] => S): S = {
    val listener = Listener(implicitly[Interest[T]].predicate(value))
    register(listener)
    try block(listener.stream(interval)) finally unregister(listener)
  }

  private val listeners: HashSet[Listener] = HashSet()
  private def register(listener: Listener): Unit = Bus.synchronized(listeners += listener)
  private def unregister(listener: Listener): Unit = Bus.synchronized(listeners -= listener)
  
  private case class Listener(predicate: Event => Boolean) {
    private[this] var promise = Promise[Update]()
    
    private[Bus] def stream(interval: Duration): Stream[Update] =
      (try Await.result(promise.future, interval)
      catch { case _: TimeoutException => Update(state, Tick) }) #:: stream(interval)
    
    private[Bus] def put(event: Event): Unit = if(predicate(event)) Bus.synchronized {
      promise.complete(Success(Update(state, event)))
      promise = Promise()
    }
  }

  private var state: State = State()

  private def receive(event: Event): Unit = {
    state = update(state, event)
    listeners.foreach { listener => listener.put(event) }
  }

  private def update(state: State, event: Event): State = event match {
    case StartJob(job) =>
      state + job
    case FinishJob(job) =>
      state - job
    case StartTask(job, task) =>
      state
    case AbortTask(job, task) =>
      state
    case FinishTask(job, task) => 
      state
    case TaskProgress(job, task, progress) =>
      state.setProgress(task, progress)
    case LogMessage(str) => state
    case Tick => state
  }
}
