/*
 *
 *   Fury, version 0.2.2. Copyright 2019 Jon Pretty, Propensive Ltd.
 *
 *   The primary distribution site is: https://propensive.com/
 *
 *   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 *   in compliance with the License. You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required  by applicable  law or  agreed to  in writing,  software  distributed  under the
 *   License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
 *   express  or  implied.  See  the  License for  the specific  language  governing  permissions and
 *   limitations under the License.
 *
 */

package fury.ogdl

import java.io.IOException
import java.nio.ByteBuffer
import java.nio.channels.FileChannel

import fury._, error._, io._
import fury.ogdl.OgdlParser.parse

import scala.collection.JavaConverters._
import scala.language.experimental.macros
import scala.language.higherKinds

final case class Ogdl(values: Vector[(String, Ogdl)]) {
  def only: String = values.head._1
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
      if (c > i) {
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

  def write[T: OgdlWriter](value: T, path: Path): Outcome[Unit] =
    Outcome.rescue[IOException](FileWriteError(path)) {
      val bak = path.rename { f =>
        s".$f.bak"
      }
      if (path.exists()) path.copyTo(bak)
      val sb = new StringBuilder()
      Ogdl.serialize(sb, implicitly[OgdlWriter[T]].write(value))
      sb.append('\n')
      path.writeSync(sb.toString).unit
    }

  def read[T: OgdlReader](path: Path): Outcome[T] =
    Outcome.rescue[IOException] { _: IOException =>
      FileNotFound(path)
    } {
      val buffer = readToBuffer(path)
      val ogdl   = parse(buffer)

      implicitly[OgdlReader[T]].read(ogdl)
    }

  private[this] def readToBuffer(path: Path): ByteBuffer = {
    val inChannel = FileChannel.open(path.javaPath)

    try {
      val size   = inChannel.size.toInt
      val buffer = ByteBuffer.allocate(size)

      inChannel.read(buffer)
      buffer.flip()

      buffer
    } finally {
      inChannel.close()
    }
  }
}
