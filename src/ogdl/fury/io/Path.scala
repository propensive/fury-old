/*
 *
 *   Fury, version 0.2.2. Copyright 2019 Jon Pretty, Propensive Ltd.
 *
 *   The primary distribution site is: https://propensive.com/
 *
 *   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 *   in compliance with the License. You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required  by applicable  law or  agreed to  in writing,  software  distributed  under the
 *   License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
 *   express  or  implied.  See  the  License for  the specific  language  governing  permissions and
 *   limitations under the License.
 *
 */

package fury.io

import fury._, error._

import java.io.FileNotFoundException
import java.io.FileOutputStream
import java.nio.file.{Files, Paths, Path => JPath}
import java.util.zip.ZipFile

import kaleidoscope._

import scala.collection.JavaConverters._
import scala.language.experimental.macros
import scala.language.higherKinds
import scala.util._

object Path {

  def unapply(str: String): Option[Path] = str match {
    case r"""$dir@([^*?:;,&|"\%<>]*)""" =>
      Some(Path(if (dir.endsWith("/")) dir.dropRight(1) else dir))
    case _ => None
  }
}

case class Path(value: String) {
  def filename: String = value.replaceAll("/$", "")

  def javaPath: JPath = Paths.get(value)

  def name: String = javaPath.getFileName.toString

  def zipfileEntries: Outcome[List[ZipfileEntry]] =
    for {
      zipFile     <- Outcome.rescue[FileNotFoundException](FileNotFound(this))(new ZipFile(filename))
      entries     <- ~zipFile.entries
      entriesList = entries.asScala.to[List]
    } yield
      entriesList.map { entry =>
        ZipfileEntry(entry.getName, () => zipFile.getInputStream(entry))
      }

  def /(child: String): Path = Path(s"$filename/$child")

  def in(root: Path): Path = Path(s"${root.value}/$value")

  def fileCount(pred: String => Boolean): Int =
    Option(javaPath.toFile.listFiles).map { files =>
      val found = files.count { f =>
        pred(f.getName)
      }
      found + files
        .filter(_.isDirectory)
        .map { f =>
          Path(f.getAbsolutePath).fileCount(pred)
        }
        .sum
    }.getOrElse(0)

  def touch(): Outcome[Unit] = Outcome.rescue[java.io.IOException](FileWriteError(this)) {
    if (!exists()) new java.io.FileOutputStream(javaPath.toFile).close()
    else javaPath.toFile.setLastModified(System.currentTimeMillis())
  }

  def describe(pred: String => Boolean): String = {
    val size  = fileSize(pred)
    val count = fileCount(pred)
    val sizeStr =
      if (size < 1024) s"${size}B"
      else if (size < 1024 * 1024) s"${size / 1024}kiB"
      else s"${size / (1024 * 1024)}MiB"

    s"$count source files, $sizeStr"
  }

  def fileSize(pred: String => Boolean): Long =
    Option(javaPath.toFile.listFiles).map { files =>
      val found = files.map { f =>
        if (pred(f.getName)) f.length else 0
      }.sum
      found + files
        .filter(_.isDirectory)
        .map { f =>
          Path(f.getAbsolutePath).fileSize(pred)
        }
        .sum
    }.getOrElse(0)

  def moveTo(path: Path): Outcome[Unit] =
    Outcome.rescue[java.io.IOException](FileWriteError(this)) {
      java.nio.file.Files.move(javaPath, path.javaPath).unit
    }

  def relativeSubdirsContaining(predicate: String => Boolean): Set[Path] = {
    val prefix = value.length + 1
    findSubdirsContaining(predicate).map { p =>
      Path(p.value.drop(prefix))
    }
  }

  def findSubdirsContaining(predicate: String => Boolean): Set[Path] =
    Option(javaPath.toFile.listFiles).map { files =>
      val found = if (files.exists { f =>
                        predicate(f.getName)
                      }) Set(this)
      else Set()

      val subdirs = files
        .filter(_.isDirectory)
        .filterNot(_.getName.startsWith("."))
        .map { p =>
          Path(p.toString)
        }
        .to[Set]

      subdirs.flatMap(_.findSubdirsContaining(predicate)) ++ found
    }.getOrElse(Set())

  def delete(): Outcome[Boolean] = {
    def delete(file: java.io.File): Boolean =
      if (file.isDirectory) file.listFiles.forall(delete) && file.delete()
      else file.delete()

    Outcome.rescue[java.io.IOException](FileWriteError(this)) {
      delete(javaPath.toFile)
    }
  }

  def children: List[String] = {
    val f = javaPath.toFile
    if (f.exists) f.listFiles.to[List].map(_.getName) else Nil
  }

  def writeSync(content: String): Outcome[Unit] =
    try {
      val writer = new java.io.BufferedWriter(new java.io.FileWriter(javaPath.toFile))
      writer.write(content)
      Success(writer.close())
    } catch {
      case e: java.io.IOException => Failure(FileWriteError(this))
    }

  def appendSync(content: String): Outcome[Unit] =
    try {
      val writer = new java.io.BufferedWriter(new java.io.FileWriter(javaPath.toFile))
      writer.append(content)
      Success(writer.close())
    } catch {
      case e: java.io.IOException => Failure(FileWriteError(this))
    }

  def exists(): Boolean = Files.exists(javaPath)

  def directory: Outcome[Path] = {
    val file = javaPath.toFile
    if (!file.exists()) {
      mkdir()
      if (file.exists()) Success(this) else Failure(FileWriteError(this))
    } else if (file.isDirectory) Success(this)
    else Failure(FileWriteError(this))
  }

  def copyTo(path: Path): Outcome[Path] =
    Outcome.rescue[java.io.IOException](FileWriteError(path)) {
      Files.copy(javaPath, path.javaPath, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
      path
    }

  def mkdir(): Unit = java.nio.file.Files.createDirectories(javaPath).unit

  def parent = Path(javaPath.getParent.toString)

  def rename(fn: String => String): Path = parent / fn(name)

  def mkParents(): Outcome[Path] =
    Outcome.rescue[java.io.IOException](FileWriteError(parent)) {
      java.nio.file.Files.createDirectories(parent.javaPath)
      this
    }
}

case class FileNotFound(path: Path)      extends FuryException
case class FileWriteError(path: Path)    extends FuryException
case class ConfigFormatError(path: Path) extends FuryException
case class ZipfileEntry(name: String, inputStream: () => java.io.InputStream)
