package fury

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

    def readIndent(i: Int): Int =
      if(buffer.remaining == 0) i
      else (buffer.get(): @switch) match {
        case '\t' =>
          readIndent(i + 1)
        case _ =>
          buffer.position(buffer.position() - 1)
          i
      }

    def append(ogdl: Ogdl, string: String, index: Int): Ogdl = ogdl match {
      case Ogdl(Vector()) =>
        if(index == 0) Ogdl(Vector((string, Ogdl(Vector())))) else {
          throw new Exception(s"Attempt to access '$string', index $index in $ogdl")
        }
      case Ogdl(lm) =>
        if(index == 0) Ogdl(lm :+ ((string, Ogdl(Vector()))))
        else Ogdl(lm.init :+ ((lm.last._1, append(lm.last._2, string, index - 1))))
    }

    @tailrec
    def parse(root: Ogdl, focus: Int, mark: Int): Ogdl =
      if(buffer.remaining == 0) root
      else (buffer.get(): @switch) match {
        case '\n' =>
          val key: String = readString(mark)
          val cur = readIndent(0)
          parse(append(root, key, focus), cur, buffer.position)
        case '\t' =>
          val key: String = readString(mark)
          parse(append(root, key, focus), focus + 1, buffer.position)
        case _ => parse(root, focus, mark)
      }

    parse(Ogdl(Vector()), 0, 0)
  }
}
