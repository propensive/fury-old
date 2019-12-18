/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.7.14. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                        ║
   ║                                                                                                           ║
   ║ The primary distribution site is: https://propensive.com/                                                 ║
   ║                                                                                                           ║
   ║ Licensed under  the Apache License,  Version 2.0 (the  "License"); you  may not use  this file  except in ║
   ║ compliance with the License. You may obtain a copy of the License at                                      ║
   ║                                                                                                           ║
   ║     http://www.apache.org/licenses/LICENSE-2.0                                                            ║
   ║                                                                                                           ║
   ║ Unless required  by applicable law  or agreed to in  writing, software  distributed under the  License is ║
   ║ distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. ║
   ║ See the License for the specific language governing permissions and limitations under the License.        ║
   ╚═══════════════════════════════════════════════════════════════════════════════════════════════════════════╝
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
  
  def apply(printWriter: => java.io.PrintWriter, pid: Pid, debug: Boolean, clearLine: Boolean): LogStyle = {
    val config = ManagedConfig()
    val timestamps = if(config.timestamps) Some(false) else None
    val logLevel = if(debug) Log.Note else Log.Info
    LogStyle(() => printWriter, pid, timestamps, false, false, true, config.theme, logLevel, autoflush = true, clearLine = clearLine)
  }
}


case class LogStyle(printWriter: () => PrintWriter, pid: Pid, timestamps: Option[Boolean], logLevel: Boolean,
    showSession: Boolean, raw: Boolean, theme: Theme, minLevel: Int, autoflush: Boolean,
    clearLine: Boolean = false) {

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
  private[this] final val paddedSession: String = if(!showSession) "" else msg"$pid".string(theme)+" "
  
  private[this] def optionalLogLevel(level: Int): String = if(!logLevel) "" else (level match {
    case Log.Note => noteString
    case Log.Info => infoString
    case Log.Warn => warnString
    case Log.Fail => failString
  })

  private[this] val wipe = if(clearLine) theme.wipe() else ""

  def log(msg: UserMsg, time: Long, level: Int): Unit = if(level >= minLevel) {
    val fTime = paddedTime(time)
    val fLevel = optionalLogLevel(level)
    
    printWriter().append(msg.string(theme).split("\n").map { line =>
      s"$wipe$fTime$fLevel$paddedSession$line"
    }.mkString("", "\n", "\n"))
    
    printWriter().flush()
  }

  def raw(string: String): Unit = if(raw) printWriter().append(string)
  def flush(force: Boolean = false): Unit = if(autoflush || force) printWriter().flush()
}

object Log {
  val Note = 1
  val Info = 2
  val Warn = 3
  val Fail = 4

  private val logFiles: HashMap[Path, LogStyle] = HashMap()
  
  lazy val global: Log = {
    val initDate = LocalDate.now()
    var cached: Int = initDate.getDayOfMonth
    
    def create(date: LocalDate) =
      new PrintWriter(new BufferedWriter(new FileWriter(path(date).javaFile, true)))
    
    def path(date: LocalDate) = Installation.logsDir / str"${date.toString}.log"
    var printWriter: PrintWriter = create(initDate)
    def update(date: LocalDate): Unit =printWriter = create(date)

    def get(): PrintWriter = {
      val date = LocalDate.now()
      if(cached != date.getDayOfMonth) update(date)
      printWriter
    }
    
    new Log(LogStyle(get, Pid(0), Some(true), true, true, false, Theme.Full, Note, autoflush = false))
  }
}

class Log(private[this] val output: LogStyle) {

  private[this] var writers: List[LogStyle] = List(output)

  private[this] def log(msg: UserMsg, time: Long, level: Int): Unit = 
    writers.foreach(_.log(msg, if(time == -1) System.currentTimeMillis else time, level))

  def attach(writer: LogStyle): Unit = writers ::= writer
  def raw(str: String): Unit = writers.foreach(_.raw(str))

  def note(msg: UserMsg, time: Long = -1): Unit = log(msg, time, Log.Note)
  def info(msg: UserMsg, time: Long = -1): Unit = log(msg, time, Log.Info)
  def warn(msg: UserMsg, time: Long = -1): Unit = log(msg, time, Log.Warn)
  def fail(msg: UserMsg, time: Long = -1): Unit = log(msg, time, Log.Fail)

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
  
  def flush(force: Boolean = false): Unit = writers.foreach(_.flush(force))

  def await(success: Boolean = true): ExitStatus = {
    flush(force = true)
    if(success) Done else Abort
  }

}
