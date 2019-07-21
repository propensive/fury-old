/*
  Fury, version 0.4.0. Copyright 2018-19 Jon Pretty, Propensive Ltd.

  The primary distribution site is: https://propensive.com/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required  by applicable  law or  agreed to  in writing,  software  distributed  under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
  express  or  implied.  See  the  License for  the specific  language  governing  permissions and
  limitations under the License.
 */
package fury.io

import java.net.URI
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileSystems, FileVisitResult, Files, Path => JPath, SimpleFileVisitor, StandardCopyOption}
import java.util.stream.Collectors
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}

import scala.collection.JavaConverters._
import scala.util.Try

import mercator._

object Zipper {

  private def packfs(zip: Zip, destination: JPath): Try[JPath] = {
    val fs = FileSystems.newFileSystem(
      URI.create("jar:" + destination.toUri.toString),
      Map("create" -> "true").asJava,
      null
    )
    val result = zip.entries.toList.sorted.traverse { case (name, source) =>
      Try {
        Files.copy(source, fs.getPath("/").resolve(name), StandardCopyOption.REPLACE_EXISTING)
      }
    }.map{ _ => destination }
    fs.close()
    result
  }

  def pack(root: Path, destination: Path): Try[Path] = {
    pack(root.javaPath, destination.javaPath).map(Path(_))
  }

  private def pack(root: JPath, destination: JPath): Try[JPath] = {
    val JPaths = if(Files.isDirectory(root)){
      val JPathWalker = Files.walk(root)
      val toPack = JPathWalker.collect(Collectors.toList[JPath]).asScala.toList//.tail
      JPathWalker.close()
      toPack
    } else List(root)

    packfs(Zip(JPaths.map{ JPath =>
      root.getParent.relativize(JPath).toString -> JPath
    }.toMap), destination)
  }

  def unpack(source: Path, destination: Path): Try[Zip] = unpack(source.javaPath, destination.javaPath)

  private def unpack(source: JPath, destination: JPath): Try[Zip] = {
    for{
      zipFile  <- Try(new ZipFile(source.toFile))
      zipEntries = zipFile.entries().asScala.toList
      entries <- zipEntries.traverse{ zipEntry =>
        val name = zipEntry.getName
        val in = zipFile.getInputStream(zipEntry)
        val result = Try{
          val target = destination.resolve(name)
          if(name.endsWith("/")){
            Files.createDirectories(target)
          } else{
            Files.createDirectories(target.getParent)
            Files.copy(in, target, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
          }
          (name, target)
        }
        in.close()
        result
      }
    } yield Zip(entries.toMap)
  }

  private class ZippingFileVisitor(sourceJPath: JPath, out: ZipOutputStream) extends SimpleFileVisitor[JPath] {

    override def visitFile(file: JPath, attrs: BasicFileAttributes): FileVisitResult = {
      val entry = new ZipEntry(sourceJPath.relativize(file).toString)
      val content = Files.readAllBytes(file)
      out.putNextEntry(entry)
      out.write(content, 0, content.length)
      out.closeEntry()
      FileVisitResult.CONTINUE
    }

  }

}

case class Zip(entries: Map[String, JPath] = Map.empty)
