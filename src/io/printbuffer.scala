package fury.io

import java.io._

object LogWriter {
  def apply(log: String => Unit) = new PrintStream(new OutputStream {
    private[this] val buf = new StringBuffer()

    override def write(bytes: Array[Byte], offset: Int, length: Int): Unit = {
      log(buf.toString)
      buf.delete(0, buf.length)
      log(new String(bytes.slice(offset, offset + length), "UTF-8"))
    }

    override def write(char: Int): Unit =
      if(char.toChar == '\n') write(Array(), 0, 0) else buf.append(char.toChar)
  })
}