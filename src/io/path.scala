/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.5.0. Copyright 2018-19 Jon Pretty, Propensive Ltd.                                        ║
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

import fury.strings._

import kaleidoscope._

import scala.language.experimental.macros
import scala.language.higherKinds
import scala.util._

import java.net.URI
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Paths, SimpleFileVisitor, StandardOpenOption, Path => JavaPath}
import java.nio.file.StandardCopyOption._
import java.io.{File => JavaFile}

object Path {

  implicit val stringShow: StringShow[Path] = _.value

  def apply(jpath: JavaPath): Path = Path(jpath.toString)
  def apply(file: JavaFile): Path = Path(file.getAbsolutePath)
  def apply(uri: URI): Path = Path(Paths.get(uri))

  def unapply(str: String): Option[Path] = str match {
    case r"""$dir@([^*?:;,&|"\%<>]*)""" => Some(Path(if(dir.endsWith("/")) dir.dropRight(1) else dir))
    case _ => None
  }

  def getTempDir(prefix: String): Try[Path] = Try(Path(Files.createTempDirectory(prefix).toString))

  // Rewritten from https://stackoverflow.com/a/10068306
  private class CopyFileVisitor(sourcePath: JavaPath, targetPath: JavaPath)
      extends SimpleFileVisitor[JavaPath] {

    override def preVisitDirectory(dir: JavaPath, attrs: BasicFileAttributes): FileVisitResult = {
      Files.createDirectories(targetPath.resolve(sourcePath.relativize(dir)))
      FileVisitResult.CONTINUE
    }

    override def visitFile(file: JavaPath, attrs: BasicFileAttributes): FileVisitResult = {
      Files.copy(file, targetPath.resolve(sourcePath.relativize(file)), REPLACE_EXISTING);
      FileVisitResult.CONTINUE
    }
  }
}

case class Path(value: String) {
  def filename: String = value.replaceAll("/$", "")
  lazy val javaPath: JavaPath = Paths.get(value)
  lazy val javaFile: JavaFile = javaPath.toFile
  def uriString: String = javaFile.toURI.toString
  def name: String = javaPath.getFileName.toString
  def /(child: String): Path = Path(s"$filename/$child")
  def in(root: Path): Path = Path(s"${root.value}/$value")

  def fileCount(pred: String => Boolean): Int =
    Option(javaFile.listFiles).map { files =>
      val current = files.count { f => pred(f.getName) }
      val descendants = files.filter(_.isDirectory).map(Path(_).fileCount(pred))sum

      current + descendants
    }.getOrElse(0)

  def empty: Boolean = 0 == fileCount(_ => true)

  def touch(): Try[Unit] = Outcome.rescue[java.io.IOException](FileWriteError(this)) {
    if(!exists()) new java.io.FileOutputStream(javaFile).close()
    else javaFile.setLastModified(System.currentTimeMillis())
  }

  def extant(): Path = {
    mkdir()
    this
  }

  def sizeString(count: Long): String = {
    def findMagnitude(count: Long, suffixes: List[String]): String = suffixes match {
      case Nil => count.toString
      case last :: Nil => s"${count}$last"
      case head :: tail => if(count < 1024) s"$count$head" else findMagnitude(count/1024, tail)
    }

    findMagnitude(count, List("B", "kiB", "MiB", "GiB", "TiB", "PiB"))
  }

  def describe(pred: String => Boolean): String =
    s"${fileCount(pred)} source files, ${sizeString(fileSize(pred))}"

  def fileSize(pred: String => Boolean): Long =
    Option(javaFile.listFiles).map { files =>
      val found = files.map { f => if(pred(f.getName)) f.length else 0 }.sum
      found + files.filter(_.isDirectory).map { f => Path(f).fileSize(pred) }.sum
    }.getOrElse(0)

  def moveTo(path: Path): Try[Unit] =
    Outcome.rescue[java.io.IOException](FileWriteError(this))(Files.move(javaPath, path.javaPath))

  def relativeSubdirsContaining(pred: String => Boolean): Set[Path] =
    findSubdirsContaining(pred).map { p => Path(p.value.drop(value.length + 1)) }

  def findChildren(pred: String => Boolean): Set[Path] = {
    def search(dir: JavaFile): Set[JavaFile] = {
      val files = dir.listFiles.to[Set]
      files.filter(_.isDirectory).flatMap(search(_)) ++ files.filter { f => !f.isDirectory && pred(f.getName) }
    }

    search(javaFile).map(Path(_))
  }

  def findSubdirsContaining(pred: String => Boolean): Set[Path] =
    Option(javaFile.listFiles).map { files =>
      val found = if(files.exists { f => pred(f.getName) }) Set(this) else Set()
      val subdirs = files.filter(_.isDirectory).filterNot(_.getName.startsWith(".")).map(Path(_)).to[Set]

      subdirs.flatMap(_.findSubdirsContaining(pred)) ++ found
    }.getOrElse(Set())

  def delete(): Try[Boolean] = {
    def delete(file: JavaFile): Boolean =
      if(file.isDirectory) file.listFiles.forall(delete) && file.delete() else file.delete()

    Outcome.rescue[java.io.IOException](FileWriteError(this))(delete(javaFile))
  }

  def writeSync(content: String): Try[Unit] = Try {
    val writer = new java.io.BufferedWriter(new java.io.FileWriter(javaPath.toFile))
    writer.write(content)
    Success(writer.close())
  }.transform(identity, _ => Failure(FileWriteError(this)))

  def appendSync(content: String): Try[Unit] = Try {
    val writer = new java.io.BufferedWriter(new java.io.FileWriter(javaPath.toFile))
    writer.append(content)
    Success(writer.close())
  }.transform(identity, _ => Failure(FileWriteError(this)))

  def directory: Try[Path] = if(!exists()) {
    mkdir()
    if(exists()) Success(this) else Failure(FileWriteError(this))
  } else if(javaFile.isDirectory) Success(this) else Failure(FileWriteError(this))

  def copyTo(path: Path): Try[Path] = Try {
    Files.walkFileTree(javaPath, new Path.CopyFileVisitor(javaPath, path.javaPath))
    path
  }

  def children: List[String] = if(exists()) javaFile.listFiles.to[List].map(_.getName) else Nil
  def exists(): Boolean = Files.exists(javaPath)
  def ifExists(): Option[Path] = if(exists) Some(this) else None
  def absolutePath(): Try[Path] = Try(this.javaPath.toAbsolutePath.normalize.toString).map(Path(_))
  def mkdir(): Unit = java.nio.file.Files.createDirectories(javaPath)
  def relativizeTo(dir: Path) = Path(dir.javaPath.relativize(this.javaPath))
  def parent = Path(javaPath.getParent.toString)
  def rename(fn: String => String): Path = parent / fn(name)

  def mkParents(): Try[Path] =
    Outcome.rescue[java.io.IOException](FileWriteError(parent)) {
      java.nio.file.Files.createDirectories(parent.javaPath)
      this
    }

  def linksTo(target: Path): Try[Path] = Try {
    Files.createSymbolicLink(javaPath, target.javaPath)
    this
  }.recover { case e: java.io.IOException => this }
}

case class FileNotFound(path: Path)      extends FuryException
case class FileWriteError(path: Path)    extends FuryException
case class ConfigFormatError(path: Path) extends FuryException
case class ZipfileEntry(name: String, inputStream: () => java.io.InputStream)
