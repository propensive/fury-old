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

import org.slf4j._

object Monitor {

  val tolerance: Long = 200L
  private val executor = java.util.concurrent.Executors.newCachedThreadPool(Threads.factory("file-watcher", daemon = true))
  private implicit val execContext: ExecutionContext = ExecutionContext.fromExecutor(executor, throw _)

  private val directoryWatchers: HashMap[Path, DirectoryWatcher] = HashMap()
  private val listeners: mutable.Map[Path, Set[Listener[_]]] = HashMap().withDefaultValue(Set())

  class Listener[T](baseDir: Path, initPaths: Set[Path], action: Option[T] => Unit) {
    var lastHit: Long = 0L
    private[Monitor] var previous: Option[T] = None
    private var last: Future[Unit] = Future.successful(())
    private var paths: Set[Path] = initPaths
    private val promise: Promise[Unit] = Promise()

    def future: Future[Unit] = promise.future

    def onEvent(path: Path, full: Option[T]): Unit = {
      lastHit = System.currentTimeMillis
      last.andThen { case _ => synchronized { last = Future(action(full)) } }
    }
    
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

  private def unregister(path: Path, listener: Listener[_]): Unit = synchronized {
    Log().info(msg"Unregistering listener at $path")
    if(listeners(path).size <= 1) {
      listeners -= path
      directoryWatchers(path).close()
      directoryWatchers -= path
    } else listeners(path) -= listener
  }
 
  private def register(path: Path, listener: Listener[_]): Unit = {
    Log().info(msg"Registering listener at $path")
    val watcher = DirectoryWatcher.builder().path(path.javaPath).listener { event =>
      val now = System.currentTimeMillis
      listeners(path).foreach { case l =>
        if(now - tolerance > l.lastHit) {
          l.onEvent(Path(event.path), l.previous)
          Log().info(msg"${event.eventType.toString} event triggered on ${Path(event.path)} for ${listeners(path).size} listeners")
        }
      }
    }.fileHashing(false).build()

    synchronized {
      directoryWatchers(path) = watcher
      listeners(path) = listeners.get(path).fold(Set[Listener[_]](listener))(_ + listener)
      watcher.watchAsync(executor)
    }
  }

  def trigger(layout: Layout): Unit = listeners(layout.baseDir).foreach(_.onEvent(layout.baseDir, None))

  def listen[T](layout: Layout, paths: Set[Path])(action: Option[T] => Unit): Listener[T] = {
    val listener = new Listener(layout.baseDir, paths, action)
    paths.foreach(register(_, listener))
    listeners(layout.baseDir) = listeners.get(layout.baseDir).fold(Set[Listener[_]](listener))(_ + listener)
    listener
  }
}
