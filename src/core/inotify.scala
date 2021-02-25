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

import fury.text._, fury.model._, fury.io._

import jovian._

import java.util.concurrent.atomic.AtomicBoolean

import fury.utils.Threads

import scala.concurrent.{ExecutionContext, Future}
import scala.util._

object Inotify {
  private var done: Boolean = false
  def check()(implicit log: Log): Try[Unit] = {
    val maxQueuedWatchers = path"/proc/sys/fs/inotify/max_user_watches"
    val count = if(!done && maxQueuedWatchers.exists()) { for {
      lines <- maxQueuedWatchers.lines()
      count <- Try(lines.next().toInt)
    } yield count } else Success(Int.MaxValue)

    Try(if(count.getOrElse(Int.MaxValue) < 8192) {
      done = true
      log.warn(msg"Your system's ${ExecName("inotify")} settings may cause filewatching to fail.")
      log.warn(msg"Please append,")
      log.warn(msg"    fs.inotify.max_user_watches=524288")
      log.warn(msg"to your ${path"/etc/sysctl.conf"} file, and run,")
      log.warn(msg"    echo 524288 > ${maxQueuedWatchers.value}")
      log.warn(msg"")
    })
  }
}
