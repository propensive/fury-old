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
import guillotine._
import gastronomy._
import acyclicity._
import optometry._
import jovian._

import scala.concurrent._, duration._
import scala.util._
import scala.collection.mutable.{HashSet, HashMap, ArrayBuffer}

import scala.annotation._

import java.io._

trait Completable[T] {
  val promise: Promise[T] = Promise()
  def await(): T = Await.result(promise.future, Duration.Inf)
  def complete(value: T): Unit = promise.complete(Success(value))
  
  def complete(completable: Option[Completable[T]], value: T): Unit = {
    import Bus.execCtx
    completable.fold(complete(value)) { c => promise.completeWith(c.promise.future) }
  }
}

object Session {
  implicit val userMsg: MsgShow[Session] = _.startTime.msg
  implicit val stringShow: StringShow[Session] = _.startTime.msg.string(Theme.NoColor)
}

case class Session(in: InputStream, outStream: PrintStream, errStream: PrintStream, env: Environment,
    args: List[String], exit: ExitStatus => Unit) extends Completable[ExitStatus] {
  
  override def toString(): String = Session.stringShow.show(this)

  val startTime: Timestamp = Timestamp.now()
  val out: PrintWriter = new PrintWriter(outStream)
  val err: PrintWriter = new PrintWriter(errStream)
  def continue: Boolean = !promise.isCompleted
}

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

case class TaskId(key: String)

object Task {
  implicit val msgShow: MsgShow[Task] = _.name
}

sealed trait Task {
  def id: TaskId
  def digest: String
  def name: Message
  def ready: Boolean
}

case class ExecTask[T](id: TaskId, digest: String, name: Message, action: () => T) extends Task {
  val promise: Promise[T] = Promise()
  def ready: Boolean = promise.isCompleted
}

case class VirtualTask(id: TaskId, digest: String, name: Message) extends Task {
  def ready: Boolean = true
}

case class SilentTask(id: TaskId, digest: String, name: Message) extends Task {
  def ready: Boolean = false
}

object Status {
  implicit val msgShow: MsgShow[Status] = _.name
}

sealed abstract class Status(val name: Message, val done: Boolean)
case class Working(complete: Double) extends Status(msg"in progress (${(complete*100).dp(0)}%)", false)
case object Started extends Status(msg"started", false)
case object Succeeded extends Status(msg"succeeded", true)
case object Aborted extends Status(msg"aborted", true)
case object NotStarted extends Status(msg"not started", false)

case class Failed(issues: Vector[Issue]) extends Status(msg"failed (${issues.size} errors)", true)

object Issue {
  implicit val msgShow: MsgShow[Issue] = { issue =>
    val severity = issue.severity match {
      case Fine => msg"${'['}${Message(_.hazard("H"))}${']'}"
      case Info => msg"${'['}${Message(_.info("I"))}${']'}"
      case Warn => msg"${'['}${Message(_.ongoing("W"))}${']'}"
      case Fail => msg"${'['}${Message(_.hazard("E"))}${']'}"
      case Halt => msg"${'['}${Message(_.hazard("X"))}${']'}"
    }

    val pathLine = issue.path.map { path => msg"$path${issue.code.fold(msg"") { cs => msg"${':'}$cs" }}" }
    val codeLine = issue.code.map(_.msg)

    msg"$severity ${pathLine.getOrElse(msg"")}${codeLine.getOrElse(msg"")}"+"\n"+issue.message
  }
}

object Highlight {
  implicit val msgShow: MsgShow[Highlight] = f => msg"${f.line}"+(f.range.fold(msg"") { r => msg"${':'}$r" })
}

case class Highlight(line: Int, range: Option[CharRange])

object CharRange {
  implicit val msgShow: MsgShow[CharRange] =
    cr => msg"${cr.start}"+(cr.length.fold(msg"") { l => msg"${'-'}${l + cr.start}" })
}

case class CharRange(start: Int, length: Option[Int])

object Code {
  implicit val msgShow: MsgShow[Code] = code => code.highlight match {
    case None            => msg"$code"
    case Some(highlight) => msg"${code.code}${':'}$highlight"
  }
}

case class Code(code: String, highlight: Option[Highlight]) {
  lazy val whitespace: Int = code.prefixLength(_.isWhitespace)
}

case class Issue(message: Message, severity: LogLevel, path: Option[Path], code: Option[Code])

object JobId {
  implicit val msgShow: MsgShow[JobId] = id => Message(_.active(id.key))
}

case class JobId(key: String)

object Job {
  implicit val msgShow: MsgShow[Job] = _.name
}

case class Job(name: Message, dag: Dag[Task] = Dag(), id: JobId = JobId(Rnd.token(12))) extends
    Completable[ExitStatus]

object State {
  // implicit val msgShow: MsgShow[State] = { state =>
  //   state.sessions.relations.flatMap { case (pid, session) =>
  //     msg"» $pid:$session" :: (state.job(session).to[List].flatMap { job =>
  //       msg"    » $job" :: (job.dag.keys.to[List].map { task =>
  //         msg"        » $task: ${state.status(task).fold(msg"${'-'}")(_.msg)}"
  //       })
  //     })
  //   }.foldLeft(msg"")(_ + "\n" + _)
  // }
}

case class TaskStatus(jobs: Set[Job], status: Status, incoming: Set[Task], outgoing: Set[Task],
    issues: List[Issue])

case class State(sessions: Bimap[Pid, Session] = Bimap(),
                 jobs: Join[Session, Job, Task] = Join(),
                 tasks: Map[Task, TaskStatus] = Map(),
                 triggers: Dag[Task] = Dag()) {
  import Bus.execCtx

  def schedule(session: Session, job: Job): State =
    copy(jobs = jobs ++ dag.keys.map((session, job, _), triggers = triggers ++ job.dag.invert)

  def start(job: Job): State = job.dag.sources.foldLeft(this)(_.start(_))

  def start(task: Task): State = {
    task match {
      case task@ExecTask(id, digest, name, action) =>
        Run(id.key) {
          Bus.enqueue(msg"Starting task $name", _(task) = Started)
          try task.action() catch {
            case exc: Exception =>
              Bus.enqueue(msg"Execution of task $name failed with exception ${exc.getMessage}")
          } finally Bus.enqueue(msg"Finishing task", _.finish(task))
        }
      case _ =>
        ()
    }

    update(task, Started)
  }
  
  def setStatus(task: Task)(change: TaskStatus => TaskStatus): State =
    copy(tasks = tasks.updated(task, change(tasks.getOrElse(task, TaskStatus())))

}

// case class State(sessions: Bimap[Pid, Session] = Bimap(),
//                  data: Join[Session, Job, Task] = Join(),
//                  jobs: Assoc[Session, Job] = Assoc(),
//                  paths: Assoc[Job, Path] = Assoc(),
//                  tasks: Map[Task, TaskStatus] = Map(),
//                  refs: Map[(JobId, TaskId), Task] = Map(),
//                  interrupted: Set[Session] = Set(),
//                  cancelled: Set[Job] = Set()) {

  // import Bus.execCtx

  /*private def enqueueJob(session: Session, job: Job): State =
    copy(data = data ++ job.dag.keys.map((session, job, _)))

  def job(session: Session): Option[Job] = jobs(session).headOption
  def job(pid: Pid): Option[Job] = session(pid).flatMap(job)
  def session(pid: Pid): Option[Session] = sessions.get(pid)
  def status(task: Task): Option[Status] = tasks.get(task).map(_.status)

  def terminateFinishedJobs(task: Task): State =
    tasks(task).jobs.filter(_.finished(this)).foldLeft(this) { case (state, next) => state.removeJob(next) }

  private def updateTask(jobId: JobId, taskId: TaskId)(change: TaskStatus => TaskStatus): State =
    copy(tasks = refs.get((jobId, taskId)).fold(tasks) { task => tasks.updated(task, change(tasks(task))) })

  def setProgress(jobId: JobId, taskId: TaskId, progress: Double): State =
    updateTask(jobId, taskId)(_.copy(status = Working(progress)))

  def setFinished(jobId: JobId, taskId: TaskId): State =
    updateTask(jobId, taskId)(_.copy(status = Succeeded)).terminateFinishedJobs(refs(jobId, taskId))
  
  def setStarted(jobId: JobId, taskId: TaskId): State = updateTask(jobId, taskId)(_.copy(status = Started))

  def addIssues(jobId: JobId, taskId: TaskId, issues: List[Issue]) =
    updateTask(jobId, taskId) { status => status.copy(issues = status.issues ::: issues) }

  def removeJob(job: Job): State =
    copy(cancelled = cancelled - job, refs = refs.filter(_._1._1 == job.id),
      tasks = job.dag.keys.foldLeft(tasks)(_ - _))

  def schedule(session: Session, job: Job): State = {
    val executions = job.dag.filter {
      case ExecTask(_, _, _, _) => true
      case _                    => false
    }

    val subsequent = executions.invert

    copy(
      refs = job.dag.keys.foldLeft(refs) { case (refs, task) => refs.updated((job.id, task.id), task) },
      tasks = job.dag.keys.foldLeft(tasks) { case (tasks, keyTask) =>
        val newTaskStatus: TaskStatus = tasks.get(keyTask).fold {
          TaskStatus(Set(job), executions(keyTask), subsequent(keyTask), NotStarted,
              if(subsequent(keyTask).isEmpty) Set(job) else Set(), Nil)
        } { ts => ts.copy(jobs = ts.jobs + job) }
        
        tasks.updated(keyTask, newTaskStatus)
      }
    ).enqueueJob(session, job)
  }
  
  def update(task: Task, newStatus: Status): State =
    tasks.get(task).fold(this) { status => copy(tasks = tasks.updated(task, status.copy(status = newStatus))) }
  
  def update(job: Job, pathSet: Set[Path]): State = copy(paths = paths.updated(job, pathSet))
  
  def finish(job: Job): State = copy(paths = paths - job, cancelled = cancelled - job)

  def finish(task: Task): State = {
    task match {
      case task@ExecTask(id, digest, name, action) =>
        val status = tasks(task)
        val subsequent = status.subsequent
        
        val updatedStatus = status.copy(status = Succeeded)
        
        val newTasks = subsequent.foldLeft(tasks.updated(task, updatedStatus)) { case (tasks, next) =>
          tasks.updated(next, tasks(next).copy(previous = tasks(next).previous - task))
        }

        val newState = copy(tasks = newTasks)
        val todo = newState.tasks(task).subsequent.filter(newState.tasks(_).previous.isEmpty)
  
        status.trigger.foreach(_.complete(Done))

        todo.foldLeft(newState)(_.start(_))
        
      case _ =>
        this
    }
  }

  def shutdown(): State = copy(interrupted = interrupted ++ jobs.keySet)

  def start(job: Job): State = job.dag.sources.foldLeft(this)(_.start(_))

  def start(task: Task): State = {
    task match {
      case task@ExecTask(id, digest, name, action) =>
        Run(id.key) {
          Bus.enqueue(msg"Starting task $name", _(task) = Started)
          try task.action() finally Bus.enqueue(msg"Finishing task", _.finish(task))
        }
      case _ =>
        ()
    }

    update(task, Started)
  }
}*/

object log {
  def fine(message: Message): Unit = Bus.enqueue(message)
  def info(message: Message): Unit = Bus.enqueue(message)
  def warn(message: Message): Unit = Bus.enqueue(message)
  def fail(message: Message): Unit = Bus.enqueue(message)
  def halt(message: Message): Unit = Bus.enqueue(message)
  def raw(message: String): Unit = Bus.enqueue(message)
}

object Run {
  def apply[T](name: String)(fn: => T): Unit = new Thread(() => fn, str"mechanism://$name").start()
}