/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.8.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.                                         ║
   ║                                                                                                           ║
   ║ The primary distribution site is: https://propensive.com/                                                 ║
   ║                                                                                                           ║
   ║ Licensed under  the Apache License,  Version 2.0 (the  "License"); you  may not use  this file  except in ║
   ║ compliance with the License. You may obtain a copy of the License at                                      ║
   ║                                                                                                           ║
   ║     http://www.apache.org/licenses/LICENSE-2.0                                                            ║
   ║                                                                                                           ║
   ║ Unless required  by applicable law  or agreed to in  writing, software  distributed under the  License is ║
   ║ distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. ║
   ║ See the License for the specific language governing permissions and limitations under the License.        ║
   ╚═══════════════════════════════════════════════════════════════════════════════════════════════════════════╝
*/
package fury.task

import fury.strings._, fury.model._, fury.io._

import mercator._

import scala.collection.mutable.HashMap
import scala.concurrent._
import scala.util._

case class TaskId(id: UserMsg)

abstract class Task() {
  type Point
  type Result
  def network: Option[Boolean]
  def dependencies: List[Task]
  def init(inputs: Seq[Task]): Option[Point]
  def run(point: TaskContext[Point]): Try[Result]
  def cancel(): Unit = TaskManager.cancel(this)
  def subgraph: Map[TaskId, Set[TaskId]] = Map(TaskId(id), Set())
}

final case class Channel(id: Int) extends AnyVal {
  def suffix: String = if(id == 0) "" else s"_$id"
}

case class TaskContext[T](value: T) {
  private var cancelled: Boolean = false
  def progress(x: Double): Unit
  def cancellation(fn: => Unit): Unit
  def apply(): T = value
}

object TaskManager {
  private val tasks: HashMap[Task, Process[_]] = HashMap()

  // There can be any number of Channels indexed on a path, 
  private val channels: HashMap[Path, Set[Channel]] = HashMap()
  
  // Sessions are indexed by PID
  private val sessions: HashMap[Pid, Session] = HashMap()

  private implicit val executionContext: ExecutionContext = ExecutionContext.global

  private def unusedChannelFor(pwd: Path): Channel = synchronized {
    val current = channels.getOrElseUpdate(pwd, Set())
    Stream.from(0).map(Channel(_)).find(!current.contains(_)).get
  }

  def cancel(task: Task): Future[Unit] = ()

  def session[T](pwd: Path, pid: Pid)(action: Session => T): T = {
    val session: Session = Session(pwd, pid, Map())
    TaskManager.synchronized { sessions(pid) = session }
    val result: T = action(session)
    close(session)
    result
  }

  def run[T](context: TaskContext[T]): Process[T] =
    task.dependencies.traverse(run(_).future).flatMap(task.future)

  def close(session: Session) = TaskManager.synchronized {
    channels(session.pwd) -= session.channel
    sessions -= session.pid
  }

  def terminate(pid: Pid) = sessions.get(pid).foreach { session => }


  object Session {
    def apply(pwd: Path, pid: Pid, id: TaskId): Session = Session(pwd, pid, Map(id -> Set()))
  }

  case class Session(pwd: Path, pid: Pid, dependencies: Map[TaskId, Set[TaskId]]) {
    
    lazy val channel: Channel = TaskManager.synchronized {
      val currentChannels = channels(pwd)
      Stream.from(0).map(Channel(_)).find(!current.contains(_)).get
    }

    def active: List[Task]
    def dependencies(task: Task): Set[Task]
  }

  private[task] class Process[Result](block: => Result) {
    val started: Long = System.currentTimeMillis
    val finished: Promise[Long] = Promise()
    lazy val future: Future[Result] = Future(blocking(block))

    def apply(): Result = Await.result(future, duration.Duration.Inf)
  }
}


case class DownloadBinary(binary: String) extends Task(id: TaskId) {
  def run(context: TaskContext): Process[Path] = Process {
    
  }
}

/*
case class StartIpfs() extends Task(msg"Starting IPFS") {

}

case class IpfsDownload(ref: IpfsRef) extends Task(msg"$ref")

case class BloopCompile(target: Target) extends Task(msg"$target") {

}

case class GitClone() extends Task(msg"") {

}

case class GitCheckout() extends Task(msg"") {

}

case class BuildJar() extends Task(msg"") {

}

// The ref should be based on the sources if it's a deterministic module, or something random otherwise
case class RunJava(ref: Hash) extends Task(msg"") {

}

case class ShareLayer()
*/