/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.6.7. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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
package fury.core

import java.util.concurrent.atomic.AtomicBoolean

import scala.annotation.tailrec
import scala.collection.mutable.HashSet
import scala.util.Try

object Lifecycle {
  private[this] val terminating: AtomicBoolean = new AtomicBoolean(false)
  private[this] val running: HashSet[Thread]   = new HashSet()
  private[this] def busy(): Option[Int] =
    running.synchronized(if(running.size > 1) Some(running.size - 1) else None)

  def busyCount: Int = busy().getOrElse(0)

  def trackThread(action: => Int): Int =
    if(!terminating.get) {
      running.synchronized(running += Thread.currentThread)
      try action
      finally {
        running.synchronized(running -= Thread.currentThread)
      }
    } else {
      println("New tasks cannot be started while Fury is shutting down.")
      1
    }
  
  def halt(): Unit = {
    System.exit(busyCount)
  }

  @tailrec
  def shutdown(previous: Int = -1): Try[ExitStatus] = {
    terminating.set(true)
    busy() match {
      case None => util.Success(Done)
      case Some(count) =>
        if(previous != count) {
          val plural = if(count > 1) "s" else ""
          println(s"Waiting for $count active task$plural to complete...")
        }
        Thread.sleep(10)
        shutdown(count)
    }
  }
  
}