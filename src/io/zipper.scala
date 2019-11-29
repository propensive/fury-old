/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.7.14. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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
package fury.io

import java.io.InputStream
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Paths, SimpleFileVisitor, Path => JavaPath}
import java.nio.file.StandardCopyOption._
import java.util.stream.Collectors
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}

import fury.strings._
import org.apache.commons.compress.archivers.zip.ParallelScatterZipCreator

import scala.util.Try
import org.apache.commons.compress.parallel.InputStreamSupplier
import org.apache.commons.compress.archivers.zip.UnixStat
import org.apache.commons.compress.archivers.zip.ZipArchiveEntry
import mercator._
import scala.collection.JavaConverters._


object Zipper {

  def pack(root: Path, destination: Path, zipCreator: ParallelScatterZipCreator): Try[Path] = pack(root.javaPath, destination.javaPath, zipCreator).map(Path(_))
  def unpack(source: Path, destination: Path): Try[Zip] = unpack(source.javaPath, destination.javaPath)

  private def packfs(zip: Zip, destination: JavaPath, zipCreator: ParallelScatterZipCreator): Try[JavaPath] = Try {
      zip.entries.toList.sorted.foreach { case (name, source) =>
        val za = createZipArchiveEntry(name)
        val iss = new InputStreamSupplier() {
          override def get: InputStream = {
            Files.newInputStream(source)
          }
        }
        if(!source.toFile.isDirectory){
          zipCreator.addArchiveEntry(za, iss)
        }
      }
    }.map(_ => destination)




  private def createZipArchiveEntry(name: String) = {
    val za = new ZipArchiveEntry(name)
    za.setMethod(ZipEntry.DEFLATED)
    za.setSize(name.length)
    za.setUnixMode(UnixStat.FILE_FLAG | 0x664)
    za
  }

  private def pack(root: JavaPath, destination: JavaPath, zipCreator: ParallelScatterZipCreator) = {
    val javaPaths = if(Files.isDirectory(root)){
      val javaPathWalker = Files.walk(root)
      val toPack = javaPathWalker.collect(Collectors.toList[JavaPath]).asScala.toList//.tail
      javaPathWalker.close()
      toPack
    } else List(root)

    packfs(Zip(javaPaths.map { javaPath => root.getParent.relativize(javaPath).toString -> javaPath }.toMap),
        destination, zipCreator)
  }

  private def unpack(source: JavaPath, destination: JavaPath): Try[Zip] = for {
    zipFile    <- Try(new ZipFile(source.toFile))
    zipEntries = zipFile.entries.asScala.toList
    entries    <- zipEntries.traverse { zipEntry =>
      val name = zipEntry.getName
        val in = zipFile.getInputStream(zipEntry)
        val result = Try {
          val target = destination.resolve(name)
          if (name.endsWith("/")) Files.createDirectories(target)
          else {
            Files.createDirectories(target.getParent)
            Files.copy(in, target, REPLACE_EXISTING)
          }
          (name, target)
        }
        in.close()
        result
    }
  } yield {
    zipFile.close()
    Zip(entries.toMap)
  }

  private class ZippingFileVisitor(sourceJavaPath: JavaPath, out: ZipOutputStream)
      extends SimpleFileVisitor[JavaPath] {

    override def visitFile(file: JavaPath, attrs: BasicFileAttributes): FileVisitResult = {
      val entry = new ZipEntry(sourceJavaPath.relativize(file).toString)
      val content = Files.readAllBytes(file)
      out.putNextEntry(entry)
      out.write(content, 0, content.length)
      out.closeEntry()
      FileVisitResult.CONTINUE
    }
  }
}

case class Zip(entries: Map[String, JavaPath] = Map.empty)
