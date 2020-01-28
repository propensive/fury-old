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

import scala.collection.mutable._
import scala.concurrent._
import scala.util._

abstract class Task(id: UserMsg) {
  type Point
  type Result
  def dependencies: List[Task]
  def init(inputs: Seq[Task]): Option[Point]
  def run(point: Point): Try[Result]
  def cancel(point: Point): Unit
}

final case class Channel(id: Int) extends AnyVal

object TaskManager {
  private val tasks: HashMap[Task, Process[_]] = HashMap()
  private val channels: HashMap[Path, Set[Channel]] = HashMap()
 
  def session(pwd: Path, channel: Channel, pid: Pid): Session = synchronized {
    val currentChannels = channels.getOrElseUpdate(pwd, Set())
    val channel = Iterator.from(0).map(Channel(_)).find(!currentChannels.contains(_)).get
  
    Session(channel, pwd, pid, Map())
  }

  def run(task: Task): Process = {
    task.dependencies.traverse(run(_))
  }

  def close(session: Session) = synchronized {
    channels(session.pwd) -= session.channel
    sessions(session.pid) -= session
  }

  def terminate(pid: Pid) = sessions.get(pid).foreach { session => }
  
  case class Session(channel: Channel, pwd: Path, pid: Pid, dependencies: Map[Task, Set[Task]]) {
    def active: List[Task]
    def dependencies(task: Task): Set[Task]
  }

  private[task] class Process(block: => Result) {
    type Point
    type Result
    lazy val future: Future[Result] = Future(blocking(block))
    def apply(): Result = Await.result(future, duration.Duration.Inf)
  }
}


/*
case class DownloadBinary(binary: Binary) extends Task(msg"$binary") {

}

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