/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.7.5. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
   ║                                                                                                           ║
   ║ The primary distribution site is: https://propensive.com/                                                 ║
   ║                                                                                                           ║
   ║ Licensed under  the Apache License,  Version 2.0 (the  "License"); you  may not use  this file  except in ║
   ║ compliance with the License. You may obtain a copy of the License at                                      ║
   ║                                                                                                           ║
   ║     http://www.apache.org/licenses/LICENSE-2.0                                                            ║
   ║                                                                                                           ║
   ║ Unless required  by applicable law  or agreed to in  writing, software  distributed under the  License is ║
   ║ distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. ║
   ║ See the License for the specific language governing permissions and limitations under the License.        ║
   ╚═══════════════════════════════════════════════════════════════════════════════════════════════════════════╝
*/
package fury.core

import fury.io._, fury.model._

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

  def store(files: Map[Path, Path], destination: Path): Try[Unit] = Try {
    val fos  = new FileOutputStream(destination.javaFile)
    val gzos = new GZIPOutputStream(fos)
    val out  = new TarOutputStream(gzos)
    files.foreach { case (name, path) =>
      out.putNextEntry(new TarEntry(path.javaFile, name.value))
      val in = new BufferedInputStream(new FileInputStream(path.javaFile))
      transfer(in, out)
    }
    out.close()
  }

  def extract(file: Path, destination: Path): Try[Unit] = Try {
    val fis  = new FileInputStream(file.javaFile)
    val gzis = new GZIPInputStream(fis)
    val in   = new TarInputStream(gzis)
    Iterator.continually(in.getNextEntry).takeWhile(_ != null).foreach { entry =>
      val path = Path(entry.getName) in destination
      //println("Extracting to "+path)
      path.mkParents()
      val fos = new FileOutputStream(path.javaFile)
      val out = new BufferedOutputStream(fos)
      transfer(in, out, keepOpen = true)
      out.close()
    }
    in.close()
  }
}

