/*

    Fury, version 0.15.1. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.utils

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Executors, ThreadFactory}

import scala.concurrent.ExecutionContext

object Threads {

  private val baseFactory = Executors.defaultThreadFactory()

  def factory(prefix: String, daemon: Boolean = false): ThreadFactory = new ThreadFactory {
    
    private val threadCounter: AtomicInteger = new AtomicInteger(0)
    
    override def newThread(runnable: Runnable): Thread = {
      val thread = baseFactory.newThread(runnable)
      thread.setName(s"$prefix-${threadCounter.getAndIncrement}")
      thread.setDaemon(daemon)
      thread
    }
  }

  def singleThread(prefix: String, daemon: Boolean = false): ExecutionContext =
    ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor(Threads.factory(prefix, daemon)), throw _)

  val launcher: ThreadFactory = factory("launcher", true)
  val logging: ThreadFactory = factory("logging", true)
  val bsp = Executors.newCachedThreadPool(Threads.factory("bsp", daemon = true))
}
