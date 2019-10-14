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

  private[this] var queue: Vector[Task[_]] = Vector()

  case class Task[T](action: () => T, isolated: Boolean) { task =>
    private[this] val promise: Promise[Unit] = Promise[Unit]()
    
    val future: Future[T] = promise.future.map { unit =>
      try blocking(action()) finally { isolator.synchronized {
        queue = queue.filter(_ != task)
        processQueue()
      } }
    }
   
    def active: Boolean = promise.isCompleted
    def start(): Unit = if(!active) promise.complete(Success(()))
  }

  private[this] def processQueue(): Unit =
    if(queue.headOption.exists(_.isolated)) queue.head.start()
    else queue.takeWhile(!_.isolated).foreach(_.start())

  def run[T](isolated: Boolean = false)(block: => T): Future[T] = {
    val task = Task(() => block, isolated)
    isolator.synchronized {
      queue = queue :+ task
      processQueue()
    }

    task.future
  }
}

