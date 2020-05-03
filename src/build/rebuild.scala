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
package fury

import fury.core._, fury.strings._, fury.model._

import java.util.concurrent.atomic.AtomicBoolean

import _root_.io.methvin.better.files.RecursiveFileMonitor
import better.files.File
import fury.io.Path
import fury.utils.Threads

import scala.concurrent.{ExecutionContext, Future}
import scala.util._


object Inotify {
  private var done: Boolean = false
  def check(watching: Boolean)(implicit log: Log): Try[Unit] = {
    val maxQueuedWatchers = Path("/proc/sys/fs/inotify/max_user_watches")
    val count = if(watching && !done && maxQueuedWatchers.exists()) { for {
      lines <- maxQueuedWatchers.lines()
      count <- Try(lines.next().toInt)
    } yield count } else Success(Int.MaxValue)

    Try(if(count.getOrElse(Int.MaxValue) < 8192) {
      done = true
      log.warn(msg"Your system's ${ExecName("inotify")} settings may cause filewatching to fail.")
      log.warn(msg"Please append,")
      log.warn(msg"    fs.inotify.max_user_watches=524288")
      log.warn(msg"to your ${Path("/etc/sysctl.conf")} file, and run,")
      log.warn(msg"    echo 524288 > ${maxQueuedWatchers.value}")
      log.warn(msg"")
    })
  }
}

trait Repeater[Res]{
  def repeatCondition(): Boolean
  def action(): Res

  def continue(res: Res): Boolean

  def delayMillis: Long = 100

  def start(): Res = {
    Iterator.iterate[(Boolean, Res)](false -> action()) { case (stopped, prev) =>
      Thread.sleep(delayMillis)
      if(!Thread.currentThread.isInterrupted && repeatCondition() && !stopped) {
        val result = action()
        !continue(result) -> result
      } else stopped -> prev
    }.dropWhile(!_._1).next()._2
  }
}

class SourceWatcher(sources: Set[Path]){
  
  private val executor =
    java.util.concurrent.Executors.newCachedThreadPool(Threads.factory("file-watcher", daemon = true))
  
  private val ec: ExecutionContext = ExecutionContext.fromExecutor(executor, throw _)
  private[this] val changed = new AtomicBoolean(true)

  def isImportant(file: File): Boolean =
    file.isRegularFile && (file.extension.exists(Set(".scala", ".java").contains))

  val directories: Set[File] = sources.map(src => File(src.value))

  def start(): SourceWatcher = {
    watchers.foreach(_.start()(ec))
    this
  }

  def stop(): SourceWatcher = {
    watchers.foreach(_.close())
    executor.shutdown()
    this
  }

  def hasChanges: Boolean = changed.get

  def clear(): Unit = changed.set(false)

  private[this] lazy val watchers = directories.map { src => new RecursiveFileMonitor(src) {
    override def onCreate(file: File, count: Int) = onChange(file)
    override def onModify(file: File, count: Int) = onChange(file)
    override def onDelete(file: File, count: Int) = onChange(file)
    override def start()(implicit ec: ExecutionContext): Unit = Future(watcher.watch())(ec)
  } }

  private[this] def onChange(file: File) = {
    val important: Boolean = file.extension.contains(".scala") || file.extension.contains(".java")
    if(important) changed.set(true)
  }

}
