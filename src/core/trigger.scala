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
package fury.core

import fury.io._, fury.text._

import scala.concurrent._
import scala.collection.mutable.HashMap

import scala.util._

object Trigger {

  private case class Waiting(timestamp: Long, promises: Set[Promise[Long]])
  
  private val waiting: HashMap[Path, Waiting] = HashMap()

  def listen(path: Path): Promise[Long] = Trigger.synchronized {
    val promise = Promise[Long]()
    Log().info(msg"Listening for updates to $path")
    waiting(path) = waiting.get(path).fold(Waiting(0L, Set(promise))) { wait =>
      wait.copy(promises = wait.promises + promise)
    }
    promise
  }
  
  def notify(path: Path, timestamp: Long): Unit = Trigger.synchronized {
    Log().info(msg"Notifying ${waiting.get(path).fold(0)(_.promises.size)} listeners to $path")
    waiting.get(path).foreach(_.promises.foreach(_.complete(Success(timestamp))))
    waiting(path) = Waiting(System.currentTimeMillis, Set())
  }

  def shutdown(): Unit = Trigger.synchronized {
    waiting.values.foreach(_.promises.foreach(_.complete(Success(0L))))
  }
}
