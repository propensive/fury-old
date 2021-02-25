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
import scala.collection.mutable.HashMap
import scala.concurrent._
import scala.util._

import java.util.concurrent.Executors

class BuildStream[T](initial: => Try[T])(implicit exec: ExecutionContext) { buildStream =>
  private var last: Option[T] = None
  def mkFuture[S](fn: => S): Future[S] = {
    val future = Future(fn)
    future.andThen { case _ => update(state) }
    future
  }

  def update(newState: => State): Unit = buildStream.synchronized { state = newState.proceed() }
  var state = State(mkFuture(initial), None, Vector())
  
  case class State(current: Future[Try[T]], enqueued: Option[Option[T] => Try[T]], tasks: Vector[() => Unit]) {
    def addTask(fn: => Unit): State = copy(tasks = tasks :+ { () => fn })
    def enqueue(fn: Option[T] => Try[T]): State = copy(enqueued = Some(fn))
    
    def proceed(): State = current.value.fold(this) { value =>
      value.flatten.foreach { v => last = Some(v) }
      if(tasks.isEmpty) copy(enqueued.fold(current) { t => mkFuture(t(last)) }, enqueued = None)
      else copy(current.andThen { case _ => mkFuture(tasks.foreach(_())) }, tasks = Vector())
    }
  }

  def addTask(fn: => Unit): Unit = update(state.addTask(fn))
  def enqueue(fn: Option[T] => Try[T]): Unit = update(state.enqueue(fn))
}

object Monitor {

  def listen(layout: Layout, paths: Set[Path])(action: => Unit): Listener = {
    val listener = new Listener(layout.baseDir, paths, action)
    paths.foreach(register(_, listener))
    listeners(layout.baseDir) = listeners.get(layout.baseDir).fold(Set[Listener](listener))(_ + listener)
    listener
  }
  
  private val tolerance: Long = 200L
  private val executor = Executors.newCachedThreadPool(Threads.factory("watcher", daemon = true))
  private implicit val execContext: ExecutionContext = ExecutionContext.fromExecutor(executor, throw _)

  private val watchers: HashMap[Path, DirectoryWatcher] = HashMap()
  private val listeners: mutable.Map[Path, Set[Listener]] = HashMap().withDefaultValue(Set())

  class Listener(baseDir: Path, initPaths: Set[Path], onChange: => Unit) {
    var lastHit: Long = 0L
    private var paths: Set[Path] = initPaths

    def interesting(path: Path): Boolean = path.value.endsWith(".scala") && !path.value.startsWith(".")

    def onEvent(): Unit = onChange
    
    def stop(): Unit = synchronized {
      paths.foreach(unregister(_, this))
      unregister(baseDir, this)
    }

    def updatePaths(newPaths: Set[Path]): Unit = synchronized {
      (newPaths -- paths).foreach(register(_, this))
      (paths -- newPaths).foreach(unregister(_, this))
      paths = newPaths
    }
  }

  private def unregister(path: Path, listener: Listener): Unit = synchronized {
    Log().info(msg"Unregistering listener at $path")
    if(listeners(path).size <= 1) {
      listeners -= path
      watchers(path).close()
      watchers -= path
    } else listeners(path) -= listener
  }
 
  private def register(path: Path, listener: Listener): Unit = {
    Log().info(msg"Registering listener at $path")
    val watcher = DirectoryWatcher.builder().path(path.javaPath).listener { event =>
      val now = System.currentTimeMillis
      listeners(path).foreach { case l =>
        val path = Path(event.path)
        if(now - tolerance > l.lastHit && l.interesting(path)) {
          l.onEvent()
          Log().info(msg"${event.eventType.toString} event triggered on ${Path(event.path)} for ${listeners(path).size} listeners")
        }
      }
    }.fileHashing(false).build()

    synchronized {
      watchers(path) = watcher
      listeners(path) = listeners.get(path).fold(Set[Listener](listener))(_ + listener)
      watcher.watchAsync(executor)
    }
  }

  def trigger(layout: Layout): Unit = listeners(layout.baseDir).foreach(_.onEvent())

}
