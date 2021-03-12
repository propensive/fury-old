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

import mercator._

import scala.concurrent._, duration._
import scala.util._
import scala.collection.mutable.{HashSet, HashMap}

import scala.annotation._

import java.util.concurrent.Executors

sealed class ExitStatus(val code: Int)
case object Done  extends ExitStatus(0)
case object Abort extends ExitStatus(1)
case object Continuation extends ExitStatus(91)

object Pid {
  implicit val stringShow: StringShow[Pid] = pid => Integer.toHexString(pid.pid).padTo(5, '0')

  implicit val msgShow: MsgShow[Pid] =
    pid => Message { theme => msg"${theme.active(stringShow.show(pid))}".string(theme) }
}

case class Pid(pid: Int)

sealed trait Event {
  def apply(state: State): State
  def relevant(job: Option[Job], state: State): Boolean
}

abstract class InternalEvent(action: State => State) extends Event {
  def apply(state: State): State = action(state)
  def relevant(listenerJob: Option[Job], state: State): Boolean = listenerJob.isEmpty
}

sealed abstract class JobEvent(job: Job, action: State => State) extends Event {
  def apply(state: State): State = action(state)
  def relevant(listenerJob: Option[Job], state: State): Boolean = listenerJob == Some(job)
}

sealed abstract class TaskEvent(task: TaskId, action: State => State) extends Event {
  def apply(state: State): State = action(state)
  
  def relevant(listenerJob: Option[Job], state: State): Boolean = true
    //listenerJob.fold(false)(state.tasks(task).job == _)
}

object TaskId { def apply(): TaskId = TaskId(java.util.UUID.randomUUID().toString) }

case class TaskId(key: String)

sealed trait Status { def withIssue(issue: Issue): Status = Failed(Vector(issue)) }
case class Working(complete: Double) extends Status
case object Started extends Status
case object Succeeded extends Status
case object NotStarted extends Status

case class Failed(issues: Vector[Issue]) extends Status {
  override def withIssue(issue: Issue): Status = Failed(issues :+ issue)
}

case class Issue(msg: Message)

object Task {
  private val tasks: HashMap[TaskId, Task[_]] = HashMap()
  private implicit val ec: ExecutionContext = Bus.execCtx
  
  def schedule[T](name: Message, prerequisites: Set[TaskId] = Set())(action: => T): Task[T] = {
    val task = Task[T](TaskId(java.util.UUID.randomUUID().toString), name, prerequisites, { () => action })
    synchronized { tasks(task.id) = task }
    task
  }

  def apply(id: TaskId): Option[Task[_]] = synchronized(tasks.get(id))
}

case class Task[T](id: TaskId, name: Message, prerequisites: Set[TaskId], action: () => T) {
  import Bus.execCtx
  lazy val future: Future[T] = Future.sequence(prerequisites.map(Task.tasks(_).future)).map { _ => action() }
  def await(): T = Await.result(future, Duration.Inf)
}

object Job {
  def apply(pid: Pid, name: Message, dag: Dag[TaskId], goal: TaskId): Unit = {
    val promise: Promise[Unit] = Promise()
    //Job(name, pid, promise)
  }
}

case class Job(name: Message, pid: Pid) {
  val completion: Promise[ExitStatus] = Promise()
  def complete(exit: ExitStatus): Unit = completion.complete(Success(exit))
  def await(): ExitStatus = Await.result(completion.future, Duration.Inf)
}

case class StartTask(task: TaskId) extends TaskEvent(task, _.startTask(task))
case class StopTask(task: TaskId) extends TaskEvent(task, _.completeTask(task))
case class StartJob(job: Job, dag: Dag[TaskId]) extends JobEvent(job, _.startJob(job, dag))
case class StopJob(job: Job) extends JobEvent(job, _.stopJob(job))

case class TaskProgress(task: TaskId, progress: Double) extends TaskEvent(task, _.setProgress(task, progress))
case class TaskIssue(task: TaskId, issue: Issue) extends TaskEvent(task, _.taskIssue(task, issue))
case class LogMessage(msg: Message) extends InternalEvent(identity)
case class RawMessage(msg: String) extends InternalEvent(identity)
case class JobMessage(job: Job, msg: Message) extends JobEvent(job, identity)

case class State(jobs: Map[Job, Dag[TaskId]] = Map(),
                 statuses: Map[TaskId, Status] = Map().withDefault { _ => NotStarted },
                 tasks: Map[TaskId, Task[_]] = Map()) {
  def apply(job: Job): Dag[TaskId] = jobs(job)
  def update(job: Job, dag: Dag[TaskId]): State = copy(jobs.updated(job, dag))
  
  private def setStatus(task: TaskId, status: Status): State =
    copy(statuses = statuses.updated(task, status))
  
  private def updateStatus(task: TaskId, modify: Status => Status): State =
    copy(statuses = statuses.updated(task, modify(statuses(task))))
  
  def startTask(task: TaskId): State = setStatus(task, Started)
  def startJob(job: Job, dag: Dag[TaskId]): State = copy(jobs = jobs.updated(job, dag))
  def stopJob(job: Job): State = copy(jobs = jobs - job)
  def completeTask(task: TaskId): State = setStatus(task, Succeeded)
  def setProgress(task: TaskId, progress: Double): State = setStatus(task, Working(progress))
  def taskIssue(task: TaskId, issue: Issue): State = updateStatus(task, _.withIssue(issue))
}

case class Tick(state: Option[State], events: List[Event])

object Bus {
  implicit val execCtx: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(64))
  def apply(): State = Bus.synchronized(state)
  def put(event: Event): Unit = Bus.synchronized { receive(event) }

  def listen[S](job: Job, interval: Int = 100)(block: Stream[Tick] => S): S = {
    val listener = new Listener(interval, Some(job))
    register(listener)
    try block(listener.tickStream) finally unregister(listener)
  }

  def listen[S](interval: Int)(block: Stream[Tick] => S): S = {
    val listener = new Listener(interval, None)
    register(listener)
    try block(listener.tickStream) finally unregister(listener)
  }
  
  private val listeners: HashSet[Listener] = HashSet()
  private def register(listener: Listener): Unit = Bus.synchronized(listeners += listener)
  private def unregister(listener: Listener): Unit = Bus.synchronized(listeners -= listener)
  private var state: State = State()
  
  private def chunked[T](stream: Stream[T], interval: Int, t0: Long = System.currentTimeMillis, chunk: List[T] = Nil)
                     : Stream[List[T]] = {
    val window: Long = interval - (System.currentTimeMillis - t0)
    if(stream.isEmpty) Stream(chunk.reverse)
    else try chunked(stream.tail, interval, t0, Await.result(Future(stream)(execCtx), window.milliseconds).head :: chunk)
    catch { case _: TimeoutException => chunk.reverse #:: chunked(stream, interval, System.currentTimeMillis) }
  }

  private class Listener(interval: Int, job: Option[Job]) { listener =>
    def tickStream: Stream[Tick] = chunked(stream, interval).map { es => Tick(es.lastOption.map(_._2), es.map(_._1)) }

    def put(event: Event, state: State): Unit = if(event.relevant(job, state)) {
      promise.complete(Success((event, state)))
      promise = Promise()
    }
    
    def stream: Stream[(Event, State)] = Await.result(promise.future, Duration.Inf) #:: stream
    private[this] var promise: Promise[(Event, State)] = Promise()
  }

  private def receive(event: Event): Unit = synchronized {
    state = event(state)
    listeners.foreach { listener => listener.put(event, state) }
  }
}

object log {
  def fine(message: Message): Unit = Bus.put(LogMessage(message))
  def info(message: Message): Unit = Bus.put(LogMessage(message))
  def warn(message: Message): Unit = Bus.put(LogMessage(message))
  def fail(message: Message): Unit = Bus.put(LogMessage(message))
  def flush(): Unit = () // FIXME

  def raw(message: String): Unit = Bus.put(RawMessage(message))
}