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
package fury.text

import scala.collection.mutable._
import scala.concurrent._, duration._
import scala.util._

sealed abstract class LogLevel(val level: Int) {
  def >(that: LogLevel) = level > that.level
  def >=(that: LogLevel) = level >= that.level
  def <(that: LogLevel) = level < that.level
  def <=(that: LogLevel) = level <= that.level
}

case object Fine extends LogLevel(1)
case object Info extends LogLevel(2)
case object Warn extends LogLevel(3)
case object Fail extends LogLevel(4)
case object Halt extends LogLevel(5)

case class Event(msg: Message, action: State => State, timestamp: Timestamp, level: LogLevel,
    termination: Option[Session])

object Bus {
  implicit val execCtx: ExecutionContext = ExecutionContext.global
  private[this] val listeners: HashSet[Listener] = new HashSet()
  private[this] val closed: Promise[Unit] = Promise()
  private[this] var state: State = State()

  def listen[T](pred: Event => Boolean)(block: Stream[Event] => T) = {
    val listener = Listener(10)
    Bus.synchronized { listeners += listener }
    try block(listener.stream.filter(pred)) finally Bus.synchronized { listeners -= listener }
  }

  def listen[T](interval: Int)(pred: Event => Boolean)(block: Stream[List[Event]] => T) = {
    val listener = Listener(interval)
    Bus.synchronized { listeners += listener }
    
    try block(listener.chunkedStream.map(_.filter(pred)).filter(_.nonEmpty))
    finally Bus.synchronized { listeners -= listener }
  }

  def apply(): State = Bus.synchronized(state)
  def close(): Unit = Bus.synchronized(closed.complete(Success(())))

  def terminate(session: Session): State = Bus()
    // putEvent(Event(msg"Terminating session $session", identity, Timestamp.precise(), Warn,
    //     Some(session))).endSession(session)

  def enqueue(msg: Message, action: State => State = identity, level: LogLevel = Info): State =
    putEvent(Event(msg, action, Timestamp.precise(), Info, None))

  private def putEvent(event: Event): State = Bus.synchronized {
    if(!closed.isCompleted) {
      listeners.foreach(_.buffer.append(event))
      state = event.action(state)
    }

    state
  }

  private case class Listener(interval: Int) {
    private[Bus] val buffer: Buffer[Event] = new ArrayBuffer()
    
    private[this] def next(): List[Event] = Bus.synchronized { try buffer.to[List] finally buffer.clear() }

    final def chunkedStream: Stream[List[Event]] =
      if(closed.isCompleted) Stream(next()) else if(!buffer.isEmpty) next() #:: chunkedStream else {
        Thread.sleep(interval)
        chunkedStream
      }

    final def stream: Stream[Event] =
      if(closed.isCompleted) next().to[Stream] else if(!buffer.isEmpty) next().to[Stream] #::: stream else {
        Thread.sleep(interval)
        stream
      }
  }
}