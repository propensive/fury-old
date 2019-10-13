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
package fury.utils

import scala.concurrent._
import scala.util._

class Isolator()(implicit ec: ExecutionContext) { isolator =>

  private[this] var isolationQueue: Vector[Task[_]] = Vector()
  private[this] var queue: Vector[Task[_]] = Vector()

  case class Task[T](action: () => T) { task =>
    private[this] val promise: Promise[Unit] = Promise[Unit]()
    
    val future: Future[T] = promise.future.map { unit =>
      try blocking(action()) finally { isolator.synchronized {
        isolationQueue = isolationQueue.filter(_ != task)
        queue = queue.filter(_ != task)
        processQueues()
      } }
    }
   
    def active: Boolean = promise.isCompleted
    def start(): Unit = if(!active) promise.complete(Success(()))
  }

  private[this] def processQueues(): Unit =
    if(isolationQueue.isEmpty) queue.foreach(_.start())
    else if(!queue.exists(_.active)) isolationQueue.head.start()

  def run[T](block: => T, isolated: Boolean = false): Future[T] = {
    val task = Task(() => block)
    isolator.synchronized {
      if(isolated) isolationQueue = isolationQueue :+ task
      else queue = queue :+ task
      processQueues()
    }

    task.future
  }
}
