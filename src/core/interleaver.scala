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

import fury.strings._, fury.model._

object Interleaver {
  case class MessageBuffer(first: Long, last: Long, messages: Vector[(Long, UserMsg)], terminated: Boolean) {
    def record(msg: UserMsg): MessageBuffer = {
      val now = System.currentTimeMillis
      copy(last = now, messages = messages :+ (now, msg))
    }

    def terminate(): MessageBuffer = copy(terminated = true)
  }
}

class Interleaver(lag: Long)(implicit log: Log) {
  
  import Interleaver._

  private[this] var current: Option[(Long, ModuleRef)] = None

  private[this] var buffer: Map[ModuleRef, MessageBuffer] = {
    val now = System.currentTimeMillis
    Map().withDefault { ref => MessageBuffer(now, now, Vector(), false) }
  }

  private[this] def switchable: Boolean = current match {
    case Some((t, ref2)) => System.currentTimeMillis > t
    case None => true
  }

  private[this] def isCurrent(ref: ModuleRef): Boolean = current match {
    case Some((_, `ref`)) => true
    case _ => false
  }

  private[this] def record(ref: ModuleRef, msg: UserMsg): Unit =
    buffer = buffer.updated(ref, buffer(ref).record(msg))

  private[this] def flush(ref: ModuleRef): Unit = {
    val msgs = buffer(ref).messages
    if(!msgs.isEmpty) log.info(msg"Output for $ref:")
    msgs.foreach { case (t, msg) => log.info(msg, time = t) }
    buffer = buffer - ref
  }
    
  private[this] def makeSwitch(): Unit = {
    // flush and remove all terminated output
    val terminated = buffer.collect { case (ref, MessageBuffer(_, _, _, true)) => ref }
    terminated.foreach { ref =>
      flush(ref)
    }
    best().foreach { newRef =>
      flush(newRef)
      current = Some((System.currentTimeMillis + lag, newRef))
    }
  }

  def best(): Option[ModuleRef] =
    if(buffer.isEmpty) None
    else Some(buffer.to[List].maxBy { case (ref, MessageBuffer(first, last, msgs, _)) => -first }._1)

  def println(ref: ModuleRef, msg: UserMsg, noTime: Boolean = false): Unit =
    if(isCurrent(ref)) log.info(msg)
    else {
      record(ref, msg)
      tick()
    }

  def tick(): Unit = if(switchable) makeSwitch()

  def terminate(ref: ModuleRef): Unit = {
    buffer = buffer.updated(ref, buffer(ref).terminate)
  }
}
