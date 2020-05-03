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
package fury.core

import java.util.concurrent.atomic.AtomicBoolean

import fury.model.ModuleRef
import fury.utils.Multiplexer

import scala.annotation.tailrec
import scala.collection.mutable.HashSet
import scala.concurrent.Promise
import scala.util.Try

object Lifecycle {
  
  trait Shutdown { def shutdown(): Unit }

  trait ResourceHolder {
    type Resource
    def acquire(session: Session, resource: Resource)
    def release(session: Session)
  }
  
  val bloopServer = Promise[Shutdown with ResourceHolder]

  case class Session(cli: Cli, thread: Thread) {
    val pid = cli.pid
    val started: Long = System.currentTimeMillis
    private[this] var sessionMultiplexer:  Multiplexer[ModuleRef, CompileEvent] = new Multiplexer(Set.empty)
    
    def multiplexer = synchronized(sessionMultiplexer)
    def multiplexer_=(m: Multiplexer[ModuleRef, CompileEvent]) = synchronized { sessionMultiplexer = m }
  }

  private[this] val terminating: AtomicBoolean = new AtomicBoolean(false)
  private[this] val running: HashSet[Session] = new HashSet()
  private[this] def busy(): Option[Int] =
    running.synchronized(if(running.size > 1) Some(running.size - 1) else None)

  private[this] def close(session: Session) = running.synchronized {
    bloopServer.future.value.map(_.map(_.release(session)))
    running -= session
  }

  def busyCount: Int = busy().getOrElse(0)

  def sessions: List[Session] = running.synchronized(running.to[List]).sortBy(_.started)

  def currentSession(implicit log: Log): Session = {
    sessions.find(_.pid == log.pid).get
  }

  def trackThread(cli: Cli, whitelisted: Boolean)(action: => Int): Int = {
    running.find(_.pid == cli.pid) match {
      case Some(session) =>
        session.thread.interrupt()
        close(session)
        0
      case None if terminating.get && !whitelisted =>
        println("New tasks cannot be started while Fury is shutting down.")
        2
      case _ =>
        val session = Session(cli, Thread.currentThread)
        running.synchronized(running += session)
        try action
        finally { close(session) }
    }
  }

  def halt(): Unit = System.exit(busyCount)

  @tailrec
  def shutdown(previous: Int = -1): Try[ExitStatus] = {
    terminating.set(true)
    busy() match {
      case None => {
        bloopServer.future.value.map(_.get.shutdown())
        util.Success(Done)
      }
      case Some(count) =>
        if(previous > count) {
          val plural = if(count > 1) "s" else ""
          println(s"Waiting for $count active task$plural to complete...")
        }
        Thread.sleep(10)
        shutdown(count)
    }
  }
  
}
