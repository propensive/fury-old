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

import java.util.concurrent.atomic.AtomicBoolean

import fury.model._, fury.utils._, fury.text._, fury.io._

import jovian._

import scala.annotation.tailrec
import scala.collection.mutable.HashSet
import scala.concurrent._, duration._
import scala.util._

object Lifecycle {
  implicit val ec = ExecutionContext.global
  
  trait Shutdown { def shutdown(): Unit }

  trait ResourceHolder {
    type Resource
    def acquire(session: Session, resource: Resource)
    def release(session: Session)
  }
  
  val bloopServer = Promise[Shutdown with ResourceHolder]

  case class Session(cli: Cli, private val thread: Thread) {
    private var abort = 0
    lazy val checkInotify: Unit = Inotify.check()

    val pid = cli.job.pid
    val started: Long = System.currentTimeMillis
    private val cancel: Promise[Unit] = Promise()

    def cancellation: Future[Unit] = cancel.future

    def interrupt(): Boolean = {
      abort += 1
      if(abort == 1) {
        Log().info(msg"Session $pid interrupted with Ctrl+C")
        sessionMultiplexer.message(Warning(msg"Compilation interrupted. Press Ctrl+C again to stop this operation"))
        Await.result(cancellation, Duration.Inf)
        false
      } else {
        Log().info(msg"Session $pid terminated with a second Ctrl+C")
        sessionMultiplexer.message(Warning(msg"Build terminated."))
        cancel.complete(Success(()))
        cancel.future.andThen { case _ => thread.interrupt() }
        true
      }
    }

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

  def trackThread(cli: Cli, whitelisted: Boolean)(action: => Int): Int =
    running.find(_.pid == cli.job.pid) match {
      case Some(session) =>
        if(session.interrupt()) close(session)
        0
      case None if terminating.get && !whitelisted =>
        println("New tasks cannot be started while Fury is shutting down.")
        2
      case None =>
        val session = Session(cli, Thread.currentThread)
        running.synchronized(running += session)
        try action finally close(session)
    }

  def halt(): Unit = System.exit(busyCount)

  def doShutdown(cli: Cli): Try[ExitStatus] = cli.call().flatMap { call => shutdown() }

  @tailrec
  def shutdown(previous: Int = -1): Try[ExitStatus] = {
    terminating.set(true)
    busy() match {
      case None =>
        bloopServer.future.value.map(_.get.shutdown())
        util.Success(Done)

      case Some(count) =>
        if(previous > count) println(s"Waiting for $count task${if(count > 1) "s" else ""} to complete...")
        Thread.sleep(10)
        shutdown(count)
    }
  }
}
