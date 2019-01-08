package fury

import java.io.IOException
import java.nio.ByteBuffer
import java.nio.channels.FileChannel

import fury.OgdlParser.parse
import mitigation._

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
      if(c > i) {
        sb.append('\n')
        c = 0
      }
      while(i > c) {
        sb.append('\t')
        c += 1
      }
      sb.append(k)
      c = serialize(sb, v, i + 1, c)
      serialize(sb, Ogdl(t), i, c)
  }

  def write[T: OgdlWriter](value: T, path: Path): Result[Unit, ~ | FileWriteError] =
    Result.rescue[IOException](_ => FileWriteError(path)){
      val bak = path.rename{ f => s".$f.bak" }
      if(path.exists()) path.copyTo(bak)
      val sb = new StringBuilder()
      Ogdl.serialize(sb, implicitly[OgdlWriter[T]].write(value))
      sb.append('\n')
      path.writeSync(sb.toString).unit
    }

  def read[T: OgdlReader](path: Path): Result[T, ~ | FileNotFound | ConfigFormatError] = {

    Result.rescue[IOException]{ _: IOException => FileNotFound(path) }{
      val buffer = readToBuffer(path)
      val ogdl = parse(buffer)

      implicitly[OgdlReader[T]].read(ogdl)
    }
  }

  private[this] def readToBuffer(path: Path): ByteBuffer = {
    val inChannel = FileChannel.open(path.javaPath)

    try {
      val size = inChannel.size.toInt
      val buffer = ByteBuffer.allocate(size)

      inChannel.read(buffer)
      buffer.flip()

      buffer
    } finally {
      inChannel.close()
    }
  }
}