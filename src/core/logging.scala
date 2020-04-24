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

import fury.strings._, fury.io._, fury.model._

import scala.collection.mutable.HashMap

import java.io._
import java.nio.channels._
import java.text.SimpleDateFormat
import java.util.Date
import java.time._

import language.higherKinds

object LogStyle {
  final val dateTimeFormat = new SimpleDateFormat("yyyy-MM-dd.HH:mm:ss.SSS ")
  final val dateFormat = new SimpleDateFormat("yyyy-MM-dd")
  final val dp3: java.text.DecimalFormat = new java.text.DecimalFormat("0.000 ")
  
  def apply(printWriter: => java.io.PrintWriter, debug: Boolean): LogStyle = {
    
    val config = ManagedConfig()
    val timestamps = if(config.timestamps) Some(false) else None
    val logLevel = if(debug) Log.Note else Log.Info
    
    LogStyle(() => printWriter, timestamps, false, false, true, config.theme, logLevel, true)
  }
}


case class LogStyle(printWriter: () => PrintWriter, timestamps: Option[Boolean], logLevel: Boolean,
    showSession: Boolean, raw: Boolean, theme: Theme, minLevel: Int, autoflush: Boolean) {

  private[this] val startTime: Long = System.currentTimeMillis

  private[this] def paddedTime(time: Long): String = timestamps match {
    case None        => ""
    case Some(true)  => theme.time(LogStyle.dateTimeFormat.format(new Date()))
    case Some(false) => theme.time(LogStyle.dp3.format((time - startTime)/1000.0).reverse.padTo(8, ' ').reverse)
  }

  private[this] final val noteString: String = msg"${'['}${theme.gray("note")}${']'} ".string(theme)
  private[this] final val infoString: String = msg"${'['}${theme.module("info")}${']'} ".string(theme)
  private[this] final val warnString: String = msg"${'['}${theme.ongoing("warn")}${']'} ".string(theme)
  private[this] final val failString: String = msg"${'['}${theme.failure("fail")}${']'} ".string(theme)
  
  private[this] def optionalLogLevel(level: Int): String = if(!logLevel) "" else (level match {
    case Log.Note => noteString
    case Log.Info => infoString
    case Log.Warn => warnString
    case Log.Fail => failString
  })

  private[this] val wipe = if(theme.name == Theme.NoColor.name) "" else theme.wipe()

  def log(msg: UserMsg, time: Long, level: Int, pid: Pid): Unit = if(level >= minLevel) {
    val fTime = paddedTime(time)
    val fLevel = optionalLogLevel(level)
    val paddedSession: String = if(!showSession) "" else msg"$pid".string(theme)+" "
    val bold: String = if(level >= Log.Warn) theme.bold() else ""
    
    printWriter().append(msg.string(theme).split("\n").map { line =>
      s"$wipe$fTime$fLevel$paddedSession$bold$line${theme.reset()}"
    }.mkString("", "\n", "\n"))
    
    printWriter().flush()
  }
  
  def flush(force: Boolean = false): Unit = if(autoflush || force) printWriter().flush()
  def raw(string: String): Unit = if(raw) printWriter().append(string)
  
  def rawln(string: String): Unit = if(raw) {
    printWriter().append(string)
    printWriter().append('\n')
  }
}

object Log {
  val Note = 1
  val Info = 2
  val Warn = 3
  val Fail = 4

  private val logFiles: HashMap[Path, LogStyle] = HashMap()
  
  private lazy val global: LogStyle = {
    val initDate = LocalDate.now()
    var cached: Int = initDate.getDayOfMonth
    
    def create(date: LocalDate) =
      new PrintWriter(new BufferedWriter(new FileWriter(path(date).javaFile, true)))
    
    def path(date: LocalDate) = Installation.logsDir.extant() / str"${date.toString}.log"
    var printWriter: PrintWriter = create(initDate)
    def update(date: LocalDate): Unit = printWriter = create(date)

    def get(): PrintWriter = {
      val date = LocalDate.now()
      if(cached != date.getDayOfMonth) update(date)
      printWriter
    }
    
    LogStyle(get, Some(true), true, true, false, Theme.Full, Note, autoflush = false)
  }

  def log(pid: Pid): Log = new Log(global, pid)
}

class Log(private[this] val output: LogStyle, val pid: Pid) {

  private[this] var writers: List[LogStyle] = List(output)

  private[this] def log(msg: UserMsg, time: Long, level: Int, pid: Pid): Unit = 
    writers.foreach(_.log(msg, if(time == -1) System.currentTimeMillis else time, level, pid))

  def attach(writer: LogStyle): Unit = writers ::= writer
  def raw(str: String): Unit = writers.foreach(_.raw(str))
  def rawln(str: String): Unit = writers.foreach(_.rawln(str))

  def note(msg: UserMsg, time: Long = -1): Unit = log(msg, time, Log.Note, pid)
  def info(msg: UserMsg, time: Long = -1): Unit = log(msg, time, Log.Info, pid)
  def warn(msg: UserMsg, time: Long = -1): Unit = log(msg, time, Log.Warn, pid)
  def fail(msg: UserMsg, time: Long = -1): Unit = log(msg, time, Log.Fail, pid)

  def infoWhen(pred: Boolean)(msg: UserMsg, time: Long = -1): Unit =
    if(pred) info(msg, time) else note(msg, time)

  def warnWhen(pred: Boolean)(msg: UserMsg, time: Long = -1): Unit =
    if(pred) warn(msg, time) else note(msg, time)

  def failWhen(pred: Boolean)(msg: UserMsg, time: Long = -1): Unit =
    if(pred) fail(msg, time) else note(msg, time)

  def stream(fn: String => Unit): PrintStream = {
    val pipe = Pipe.open()
    val in = Channels.newInputStream(pipe.source())
    val out = Channels.newOutputStream(pipe.sink())

    new PrintStream(new BufferedOutputStream(out) {
      override def write(array: Array[Byte], off: Int, len: Int): Unit =
        new String(array, off, len).split("\n").foreach(fn)

      override def write(char: Int): Unit = ()
    })
  }

  def writersCount: Int = writers.size

  def flush(force: Boolean = false): Unit = writers.foreach(_.flush(force))

  def await(success: Boolean = true): ExitStatus = {
    flush(force = true)
    if(success) Done else Abort
  }

}
