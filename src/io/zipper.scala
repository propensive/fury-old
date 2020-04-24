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

  def pack(root: Path, creator: ParallelScatterZipCreator): Try[Unit] = {
    val tree = root.walkTree.map { path => path.relativizeTo(root.parent).value -> path }.toMap
    packfs(Zip(tree), creator)
  }

  def unpack(source: Path, destination: Path): Try[Zip] = for {
    zipFile <- Try(new ZipFile(source.javaPath.toFile))
    entries <- zipFile.entries.asScala.to[List].traverse(unpackEntry(destination, zipFile, _))
    _       =  zipFile.close()
  } yield Zip(entries.toMap)

  private def packfs(zip: Zip, creator: ParallelScatterZipCreator): Try[Unit] = Try {
    zip.entries.to[List].sortBy(_._1).foreach { case (name, source) =>
      val za = createZipArchiveEntry(name)
      val iss = new InputStreamSupplier() { override def get: InputStream = source.inputStream() }
      if(!source.directory) creator.addArchiveEntry(za, iss)
    }
  }

  def pack(zip: ZipFile, creator: ParallelScatterZipCreator)
          (filter: ZipEntry => Boolean = _ => true)
          : Try[Unit] = Try {

    zip.entries.asScala.to[List].sortBy(_.getName).filter(filter).foreach { case entry =>
      val za = createZipArchiveEntry(entry.getName)
      val iss = new InputStreamSupplier() { override def get: InputStream = zip.getInputStream(entry) }
      creator.addArchiveEntry(za, iss)
    }
  }

  private def createZipArchiveEntry(name: String) = {
    val za = new ZipArchiveEntry(name)
    za.setMethod(ZipEntry.DEFLATED)
    za.setSize(name.length)
    za.setUnixMode(UnixStat.FILE_FLAG | 0x664)
    za
  }

  private def unpackEntry(destination: Path, zipFile: ZipFile, entry: ZipEntry): Try[(String, Path)] = Try {
    val target = destination / java.util.UUID.randomUUID().toString
    if(entry.getName.endsWith("/")) target.mkdir()
    else {
      val in = zipFile.getInputStream(entry)
      target.parent.mkdir()
      Files.copy(in, target.javaPath, REPLACE_EXISTING)
      in.close()
    }
    (entry.getName, target)
  }
}

case class Zip(entries: Map[String, Path] = Map.empty)
