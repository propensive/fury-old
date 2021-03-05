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

import fury.io._, fury.model._, fury.text._, fury.utils._

import jovian._

import _root_.io.methvin.watcher._

import scala.collection.mutable
import scala.concurrent._, duration._
import scala.util._

import java.util.concurrent.Executors

class Activity[T](initial: => Try[T], onSuccess: T => Unit, stopOnSuccess: Boolean)(implicit exec: ExecutionContext) { activity =>
  private var last: Option[T] = None
  private var continue: Boolean = true
  private val promise: Promise[Unit] = Promise()
  
  def mkFuture[S](fn: => S): Future[S] = {
    val future = Future(fn)
    future.andThen { case _ => update(state) }
    future
  }

  // FIXME
  def complete(): Unit = try promise.complete(Success(())) catch { case _: Exception => () }
  def abort(): Unit = continue = false
  def await(): Unit = Await.result(promise.future, Duration.Inf)

  def update(newState: => State): Unit =
    if(continue) activity.synchronized { state = newState.proceed() }
    else complete()

  var state = State(mkFuture(initial), None, Vector())
  
  case class State(current: Future[Try[T]], enqueued: Option[Option[T] => Try[T]], tasks: Vector[() => Unit]) {
    def addTask(fn: => Unit): State = {
      Log().info("Adding task")
      copy(tasks = tasks :+ { () => Log().info("Actually running task"); fn })
    }
    def enqueue(fn: Option[T] => Try[T]): State = copy(enqueued = Some(fn))
    
    def proceed(): State = current.value.fold(this) { value =>
      Log().info(msg"Tasks: ${tasks.toString}")
      if(stopOnSuccess && tasks.isEmpty) {
        complete()
        this
      } else {
        value.flatten.foreach { v =>
          onSuccess(v)
          last = Some(v)
        }
        if(tasks.isEmpty) copy(enqueued.fold(current) { t => mkFuture(t(last)) }, enqueued = None)
        else copy(current.andThen { case _ => Log().info(msg"Running tasks"); mkFuture(tasks.foreach(_())) }, tasks = Vector())
      }
    }
  }

  def addTask(fn: => Unit): Unit = update(state.addTask(fn))
  def enqueue(fn: Option[T] => Try[T]): Unit = update(state.enqueue(fn))
}

object Monitor {
  def listen(layout: Layout)(action: => Unit): Listener = {
    val listener = new Listener(layout.baseDir, action)
    rootListeners(layout.baseDir) += listener
    listeners(layout.baseDir) = listeners.get(layout.baseDir).fold(Set[Listener](listener))(_ + listener)
    listener
  }
  
  private val suspensions: mutable.HashSet[Path] = mutable.HashSet()
  private val tolerance: Long = 200L
  private val executor = Executors.newCachedThreadPool(Threads.factory("watcher", daemon = true))
  private implicit val execContext: ExecutionContext = ExecutionContext.fromExecutor(executor, throw _)

  private val watchers: mutable.HashMap[Path, DirectoryWatcher] = mutable.HashMap()
  private val listeners: mutable.Map[Path, Set[Listener]] = mutable.HashMap().withDefaultValue(Set())
  private val rootListeners: mutable.Map[Path, Set[Listener]] = mutable.HashMap().withDefaultValue(Set())

  def suspend[T](path: Path)(task: => T): T = if(!suspensions.contains(path)) {
    Monitor.synchronized(suspensions += path)
    val result = task
    Monitor.synchronized(suspensions -= path)
    result
  } else task

  class Listener(baseDir: Path, onChange: => Unit) {
    private var lastHit: Long = 0L
    private var paths: Set[Path] = Set()

    def interesting(path: Path, timestamp: Long): Boolean =
      timestamp > lastHit && path.value.endsWith(".scala") && !path.value.startsWith(".") &&
          !suspensions.exists(path.value startsWith _.value)

    def onEvent(t: Long): Unit = {
      lastHit = t
      onChange
    }
    
    def stop(): Unit = Monitor.synchronized {
      paths.foreach(unregister(_, this))
      rootListeners -= baseDir
      unregister(baseDir, this)
    }

    def updatePaths(newPaths: Set[Path]): Unit = Monitor.synchronized {
      (newPaths -- paths).foreach(register(_, this))
      (paths -- newPaths).foreach(unregister(_, this))
      paths = newPaths
    }
  }

  private def unregister(path: Path, listener: Listener): Unit = Monitor.synchronized {
    Log().info(msg"Unregistering listener at $path")
    if(listeners(path).size <= 1) {
      listeners -= path
      watchers(path).close()
      watchers -= path
    } else listeners(path) -= listener
  }
 
  private def register(path: Path, listener: Listener): Unit = {
    lazy val watcher = DirectoryWatcher.builder().path(path.javaPath).listener { event =>
      val now = System.currentTimeMillis
      listeners(path).foreach { case l =>
        val eventPath = Path(event.path)
        if(l.interesting(eventPath, now - tolerance)) {
          Log().info(msg"File $eventPath has been modified")
          l.onEvent(now)
          Log().info(msg"${event.eventType.toString} event triggered on ${path} for ${listeners(path).size} listeners")
        }
      }
    }.fileHashing(false).build()

    synchronized {
      listeners(path) ++= Set(listener)
      if(!watchers.contains(path)) {
        Log().info(msg"Registering listener at $path")
        watchers(path) = watcher
        watcher.watchAsync(executor)
      }
    }
  }

  def trigger(layout: Layout): Unit = rootListeners(layout.baseDir).foreach(_.onEvent(System.currentTimeMillis))
}
