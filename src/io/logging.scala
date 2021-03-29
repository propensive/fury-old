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
package fury.io

import fury.text._

import jovian._

import scala.collection.mutable.HashMap
import scala.annotation.tailrec

import java.io._
import java.nio.channels._
import java.text.SimpleDateFormat
import java.util.Date
import java.time._

import language.higherKinds

case class BasicConfig(theme: Theme = Theme.Basic, timestamps: Boolean = false, trace: Boolean = false)

object Logger {
  final val dateTimeFormat = new SimpleDateFormat("yyyy-MM-dd.HH:mm:ss.SSS ")
  final val dateFormat = new SimpleDateFormat("yyyy-MM-dd")
  final val dp4: java.text.DecimalFormat = new java.text.DecimalFormat("0.0000 ")
  
  def apply(debug: Boolean): Logger = {
    val config = Ogdl.read[BasicConfig](Installation.userConfig, identity(_)).toOption.getOrElse(BasicConfig())
    val timestamps = if(config.timestamps) Some(false) else None
    val logLevel = if(debug) Fine else Info
    
    Logger(timestamps, false, false, true, config.theme, logLevel, true)
  }
}

case class Logger(timestamps: Option[Boolean], showLevel: Boolean, showSession: Boolean, raw: Boolean,
                    theme: Theme, minLevel: LogLevel, autoflush: Boolean) {

  private[this] val startTime: Timestamp = Timestamp.precise()

  private[this] def paddedTime(time: Timestamp): String = timestamps match {
    case None        => ""
    case Some(true)  => theme.time(Logger.dateTimeFormat.format(new Date()))
    case Some(false) => theme.time(Logger.dp4.format((time - startTime).value/1000000000.0).reverse.padTo(8, ' ').reverse)
  }

  private[this] final val fineString: String = msg"${'['}${theme.gray("note")}${']'} ".string(theme)
  private[this] final val infoString: String = msg"${'['}${theme.module("info")}${']'} ".string(theme)
  private[this] final val warnString: String = msg"${'['}${theme.ongoing("warn")}${']'} ".string(theme)
  private[this] final val failString: String = msg"${'['}${theme.failure("fail")}${']'} ".string(theme)
  private[this] final val haltString: String = msg"${'['}${theme.hazard("halt")}${']'} ".string(theme)
  
  private[this] def optionalLogLevel(level: LogLevel): String = if(!showLevel) "" else (level match {
    case Fine => fineString
    case Info => infoString
    case Warn => warnString
    case Fail => failString
    case Halt => haltString
  })

  private[this] val wipe = if(theme.name == Theme.NoColor.name) "" else theme.wipe()

  def format(msg: Message, timestamp: Timestamp, level: LogLevel, pid: Option[Pid] = None): String =
    if(level >= minLevel) {
      val fTime = paddedTime(timestamp)
      val fLevel = optionalLogLevel(level)
      val paddedSession: String = pid.fold("") { pid => msg"$pid".string(theme)+" " }
      val bold: String = if(level >= Warn) theme.bold() else ""

      msg.string(theme).split("\n").map { line =>
        s"$wipe$fTime$fLevel$paddedSession$bold$line${theme.reset()}"
      }.mkString("", "\n", "\n")
    } else ""

  def streamTo(session: Session): Unit = Bus.listen(_ => true) { stream =>
    @tailrec def write(stream: Stream[Event]): Unit = if(stream.isEmpty) session.out.flush() else {
      stream.head match {
        case Event(msg, _, timestamp, level, _) => session.out.write(format(msg, timestamp, level, None))
      }
      
      write(stream.tail)
    }

    write(stream.takeWhile(!_.termination.contains(session)))
  }
}

object Log {
  def printStream(fn: String => Unit): PrintStream = {
    val pipe = Pipe.open()
    val in = Channels.newInputStream(pipe.source())
    val out = Channels.newOutputStream(pipe.sink())

    new PrintStream(new BufferedOutputStream(out) {
      override def write(array: Array[Byte], off: Int, len: Int): Unit =
        new String(array, off, len).split("\n").foreach(fn)

      override def write(char: Int): Unit = ()
    })
  }
}