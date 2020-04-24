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
package fury.ogdl

import java.nio.ByteBuffer
import java.nio.channels.FileChannel

import fury.io._
import fury.strings._

import scala.language.experimental.macros
import scala.language.higherKinds
import scala.language.dynamics

import scala.util._
import scala.util.control.NonFatal

import java.util.NoSuchElementException

case class OgdlException(error: String) extends FuryException
case class OgdlReadException(path: Path, cause: Throwable) extends FuryException {
  initCause(cause)
}

final case class Ogdl(values: Vector[(String, Ogdl)]) extends Dynamic {
  def apply(): String                     = values.head._1
  def applyDynamic(key: String)(): String = selectDynamic(key).apply()
  
  def selectDynamic(key: String): Ogdl    = try values.find(_._1 == key).get._2 catch {
    case e: NoSuchElementException => throw OgdlException(str"Element $key not found")
  }

  // FIXME: Change the type of `set` to `"set"` when upgrading to Scala 2.13.x
  def applyDynamicNamed(set: String)(updates: (String, Ogdl)*): Ogdl =
    Ogdl(updates.foldLeft(values) {
      case (vs, (key, value)) =>
        vs.indexWhere(_._1 == key) match {
          case -1 =>
            vs :+ (key, value)
          case idx =>
            vs.patch(idx, Vector((key, value)), 1)
        }
    })

  def map(fn: Ogdl => Ogdl) =
    Ogdl(values.map {
      case ("", v) => ("", v)
      case (k, v)  => (k, fn(v))
    })
}

object Ogdl {

  def apply[T: OgdlWriter](value: T): Ogdl = implicitly[OgdlWriter[T]].write(value)

  def serialize(node: Ogdl): String = {
    val sb = new StringBuilder()
    serialize(sb, node)
    sb.append('\n')
    sb.toString
  }

  def serialize(sb: StringBuilder, node: Ogdl, i: Int = 0, current: Int = 0): Int = node match {
    case Ogdl(Vector()) => current
    case Ogdl((k, v) +: t) =>
      var c = current
      if(c > i) {
        sb.append('\n')
        c = 0
      }
      while (i > c) {
        sb.append('\t')
        c += 1
      }
      sb.append(k)
      c = serialize(sb, v, i + 1, c)
      serialize(sb, Ogdl(t), i, c)
  }

  def write[T: OgdlWriter](value: T, path: Path): Try[Unit] =
    Try {
      val bak = path.rename { f => s".$f.bak" }
      if(path.exists()) path.copyTo(bak)
      val sb = new StringBuilder()
      Ogdl.serialize(sb, implicitly[OgdlWriter[T]].write(value))
      sb.append('\n')
      path.writeSync(sb.toString)
    }.flatten.recoverWith { case e: Exception => Failure(FileWriteError(path, e)) }

  def read[T: OgdlReader](string: String, preprocessor: Ogdl => Ogdl): T = {
    val buffer = ByteBuffer.wrap(string.bytes)
    val ogdl   = OgdlParser.parse(buffer)

    implicitly[OgdlReader[T]].read(preprocessor(ogdl))
  }

  def read[T: OgdlReader](path: Path, preprocessor: Ogdl => Ogdl): Try[T] =
    Try {
      val buffer = readToBuffer(path)
      val ogdl   = OgdlParser.parse(buffer)
      implicitly[OgdlReader[T]].read(preprocessor(ogdl))
    }.recoverWith {
      case NonFatal(e) => Failure(OgdlReadException(path, e))
    }

  private[this] def readToBuffer(path: Path): ByteBuffer = {
    val inChannel = FileChannel.open(path.javaPath)

    try {
      val size   = inChannel.size.toInt
      val buffer = ByteBuffer.allocate(size)

      inChannel.read(buffer)
      buffer.flip()

      buffer
    } finally inChannel.close()
  }
}
