package fury.core

import fury.io._

import java.io._
import annotation._
import scala.util.Try
import org.kamranzafar.jtar._

import java.util.zip._

object TarGz {

  @tailrec
  private def transfer(
      in: InputStream,
      out: OutputStream,
      data: Array[Byte] = new Array(65536),
      keepOpen: Boolean = false
    ): Unit = {
    val count = in.read(data)
    if (count != -1) {
      out.write(data, 0, count)
      transfer(in, out, data, keepOpen)
    } else {
      out.flush()
      if (!keepOpen) in.close()
    }
  }

  def store(files: List[Path], destination: Path, layout: Layout): Try[Unit] = Try {
    val fos  = new FileOutputStream(destination.javaFile)
    val gzos = new GZIPOutputStream(fos)
    val out  = new TarOutputStream(gzos)
    files.foreach { path =>
      out.putNextEntry(new TarEntry(path.javaFile, path.relativizeTo(layout.pwd).value))
      val in = new BufferedInputStream(new FileInputStream(path.javaFile))
      transfer(in, out)
    }
    out.close()
  }

  def extract(file: Path, destination: Path, layout: Layout): Try[Unit] = Try {
    val fis  = new FileInputStream(file.javaFile)
    val gzis = new GZIPInputStream(fis)
    val in   = new TarInputStream(gzis)
    Iterator.continually(in.getNextEntry).takeWhile(_ != null).foreach { entry =>
      val path = Path(entry.getName) in destination
      path.mkParents()
      val fos = new FileOutputStream(path.javaFile)
      val out = new BufferedOutputStream(fos)
      transfer(in, out, keepOpen = true)
      out.close()
    }
    in.close()
  }
}
