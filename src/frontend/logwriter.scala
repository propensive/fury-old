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
package fury

import fury._, core._, text._, model._, io._, utils._

import com.facebook.nailgun.NGContext
import exoskeleton._
import guillotine._
import jovian._
import scala.collection.JavaConverters._
import scala.collection.mutable.HashSet

import annotation.tailrec
import scala.concurrent._, duration._

import scala.util._

import java.io._
import java.util.Date
import java.text._

object LogWriter {
  private final val dateFormat: DateFormat = new SimpleDateFormat("yyyy-MM-dd")
  private def logFile() = Installation.logsDir.extant() / str"${dateFormat.format(new Date()).toString}.log"
  private def today(): Int = new Date().getDay()

  private def printWriter(): PrintWriter =
    new PrintWriter(new BufferedWriter(new FileWriter(logFile().javaFile, true)))

  val logger: Logger = Logger(true)

  @tailrec
  def apply(stream: Stream[List[Event]], writer: PrintWriter = printWriter(), day: Int = today()): Unit = {
    if(!stream.isEmpty) {
      val newDay = today()

      // When the day changes or the file gets deleted, create a new printWriter
      val newWriter = if(newDay != day || !logFile().exists) printWriter() else writer

      stream.head.foreach { event =>
        newWriter.write(logger.format(event.msg, Timestamp.precise(), Info, None))
      }
      
      newWriter.flush()

      apply(stream.tail, newWriter, newDay)
    }
  }
}