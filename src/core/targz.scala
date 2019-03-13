package fury

import java.io._
import annotation._
import scala.util.Try
import org.kamranzafar.jtar._

object TarGz {

  @tailrec
  private def transfer(
      in: InputStream,
      out: OutputStream,
      data: Array[Byte] = new Array(2048)
    ): Unit = {
    val count = in.read(data)
    if (count != -1) {
      out.write(data, 0, count)
      transfer(in, out, data)
    } else {
      out.flush()
      in.close()
    }
  }

  def store(files: List[Path], destination: Path, layout: Layout): Try[Unit] = Try {
    val fos = new FileOutputStream(destination.javaFile)
    val out = new TarOutputStream(fos)
    files.foreach { path =>
      out.putNextEntry(new TarEntry(path.javaFile, path.relativizeTo(layout.pwd).value))
      val in = new BufferedInputStream(new FileInputStream(path.javaFile))
      transfer(in, out)
    }
    out.close()
  }
}
