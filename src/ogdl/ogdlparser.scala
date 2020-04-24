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

import scala.annotation.{switch, tailrec}

object OgdlParser {

  def parse(buffer: ByteBuffer): Ogdl = {
    def readString(mark: Int): String = {
      val array = new Array[Byte](buffer.position() - mark - 1)
      buffer.position(mark)
      buffer.get(array)
      buffer.get()
      val out = new String(array, "UTF-8")
      out
    }

    @tailrec
    def readIndent(i: Int): Int =
      if(buffer.remaining == 0) i
      else
        (buffer.get(): @switch) match {
          case '#' =>
            if(i == 0) {
              readComment()
              readIndent(i)
            } else {
              buffer.position(buffer.position() - 1)
              i
            }
          case '\t' =>
            readIndent(i + 1)
          case _ =>
            buffer.position(buffer.position() - 1)
            i
        }

    @tailrec
    def readComment(): Unit =
      if(buffer.remaining == 0) ()
      else (buffer.get(): @switch) match {
        case '\n' => ()
        case _ => readComment()
      }
    
    def append(ogdl: Ogdl, string: String, index: Int): Ogdl = ogdl match {
      case Ogdl(Vector()) =>
        if(index == 0) Ogdl(Vector((string, Ogdl(Vector()))))
        else {
          throw new Exception(s"Attempt to access '$string', index $index in $ogdl")
        }
      case Ogdl(lm) =>
        if(index == 0) Ogdl(lm :+ ((string, Ogdl(Vector()))))
        else Ogdl(lm.init :+ ((lm.last._1, append(lm.last._2, string, index - 1))))
    }

    @tailrec
    def parse(root: Ogdl, focus: Int, mark: Int): Ogdl =
      if(buffer.remaining == 0) root
      else
        (buffer.get(): @switch) match {
          case '#' =>
            readComment()
            parse(root, focus, buffer.position)
          case '\n' =>
            val key: String = readString(mark)
            val cur         = readIndent(0)
            parse(append(root, key, focus), cur, buffer.position)
          case '\t' =>
            val key: String = readString(mark)
            parse(append(root, key, focus), focus + 1, buffer.position)
          case _ => parse(root, focus, mark)
        }

    parse(Ogdl(Vector()), 0, 0)
  }
}
