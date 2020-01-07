/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.8.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.                                         ║
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
import java.nio.file.{FileVisitResult, Files, Paths, SimpleFileVisitor, StandardCopyOption, Path => JavaPath}
import java.nio.file.StandardCopyOption._
import java.io.{InputStream, File => JavaFile}

object Path {

  implicit val stringShow: StringShow[Path] = _.value
  implicit val diff: Diff[Path] = (l, r) => Diff.stringDiff.diff(l.value, r.value)
  implicit val parser: Parser[Path] = unapply(_)

  def apply(jpath: JavaPath): Path = Path(jpath.toString)
  def apply(file: JavaFile): Path = Path(file.getAbsolutePath)
  def apply(uri: URI): Path = Path(Paths.get(uri))

  def unapply(str: String): Option[Path] = str.only { case r"""$dir@([^*?:;,&|"\%<>]*)""" => Path(dir) }

  // Rewritten from https://stackoverflow.com/a/10068306
  private class CopyFileVisitor(sourcePath: JavaPath, targetPath: JavaPath)
      extends SimpleFileVisitor[JavaPath] {

    override def preVisitDirectory(dir: JavaPath, attrs: BasicFileAttributes): FileVisitResult = {
      createDirectories(targetPath.resolve(sourcePath.relativize(dir)))
      FileVisitResult.CONTINUE
    }

    override def visitFile(file: JavaPath, attrs: BasicFileAttributes): FileVisitResult = {
      Files.copy(file, targetPath.resolve(sourcePath.relativize(file)), REPLACE_EXISTING)
      FileVisitResult.CONTINUE
    }
  }

  def createDirectories(path: JavaPath): Boolean = path.toFile.mkdirs()
}

case class Path(input: String) {
  val value: String = {
    def canonicalize(str: List[String], drop: Int = 0): List[String] = str match {
      case ".." :: tail => canonicalize(tail, drop + 1)
      case head :: tail => if(drop > 0) canonicalize(tail, drop - 1) else head :: canonicalize(tail)
      case Nil          => List.fill(drop)("..")
    }
    
    canonicalize((input.split("/").to[List] match {
      case "" :: xs => "" :: xs.filter { p => p != "." && p != "" }
      case xs       => xs.filter { p => p != "." && p != "" }
    }).reverse).reverse match {
      case Nil => "."
      case xs  => xs.mkString("/")
    }
  }

  def filename: String = value
  lazy val javaPath: JavaPath = Paths.get(value)
  lazy val javaFile: JavaFile = javaPath.toFile
  def uriString: String = javaFile.toURI.toString
  def name: String = javaPath.getFileName.toString
  def /(child: String): Path = Path(s"$filename/$child")
  def in(root: Path): Path = Path(s"${root.value}/$value")

  def empty: Boolean = {
    val filesStream = Files.walk(javaPath)
    try filesStream.allMatch(p => Files.isDirectory(p))
    finally filesStream.close()
  }

  def hardLink(path: Path): Try[Unit] = Try(Files.createLink(javaPath, path.javaPath)).map { _ => () }.recoverWith {
    case ex: java.nio.file.NoSuchFileException => copyTo(path).map { _ => () }
  }

  def touch(): Try[Unit] = Outcome.rescue[java.io.IOException](FileWriteError(this, _)) {
    if(!exists()) new java.io.FileOutputStream(javaFile).close()
    else javaFile.setLastModified(System.currentTimeMillis())
  }

  def extant(): Path = {
    mkdir()
    this
  }

  def directory: Boolean = Files.isDirectory(javaPath)

  def extantParents(): Path = {
    parent.mkdir()
    this
  }

  def setExecutable(exec: Boolean): Try[Unit] = Try(javaFile.setExecutable(exec))

  def resolve(rel: Path) = Path(javaPath.resolve(rel.javaPath))

  def moveTo(path: Path): Try[Unit] =
    Outcome.rescue[java.io.IOException](FileWriteError(this, _)){
      path.parent.extant()
      Files.move(javaPath, path.javaPath, StandardCopyOption.REPLACE_EXISTING)
    }

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

    Outcome.rescue[java.io.IOException](FileWriteError(this, _))(delete(javaFile))
  }

  def writeSync(content: String): Try[Unit] = Try {
    val writer = new java.io.BufferedWriter(new java.io.FileWriter(javaPath.toFile))
    writer.write(content)
    Success(writer.close())
  }.transform(identity, e => Failure(FileWriteError(this, e)))

  def appendSync(content: String): Try[Unit] = Try {
    val writer = new java.io.BufferedWriter(new java.io.FileWriter(javaPath.toFile))
    writer.append(content)
    Success(writer.close())
  }.transform(identity, e => Failure(FileWriteError(this, e)))

  def copyTo(path: Path): Try[Path] = Try {
    Files.walkFileTree(javaPath, new Path.CopyFileVisitor(javaPath, path.javaPath))
    path
  }


  def walkTree: Iterator[Path] =
    if(directory) Iterator(this) ++ childPaths.to[Iterator].flatMap(_.walkTree) else Iterator(this)

  def children: List[String] = if(exists()) javaFile.listFiles.to[List].map(_.getName) else Nil
  def childPaths: List[Path] = children.map(this / _)
  def exists(): Boolean = Files.exists(javaPath)
  def ifExists(): Option[Path] = if(exists) Some(this) else None
  def absolutePath(): Try[Path] = Try(this.javaPath.toAbsolutePath.normalize.toString).map(Path(_))
  def mkdir(): Unit =  Path.createDirectories(javaPath)
  def relativizeTo(dir: Path) = Path(dir.javaPath.relativize(this.javaPath))
  def parent = Path(javaPath.getParent.toString)
  def rename(fn: String => String): Path = parent / fn(name)
  
  def mkParents(): Try[Path] =
    Outcome.rescue[java.io.IOException](FileWriteError(parent, _)) {
      Path.createDirectories(parent.javaPath)
      this
    }

  def linksTo(target: Path): Try[Path] = Try {
    Files.createSymbolicLink(javaPath, target.javaPath)
    this
  }.recover { case e: java.io.IOException => this }

  def inputStream(): InputStream = Files.newInputStream(javaPath)
}

case class FileNotFound(path: Path)      extends FuryException
case class FileWriteError(path: Path, e: Throwable) extends FuryException
case class ConfigFormatError(path: Path) extends FuryException
case class ZipfileEntry(name: String, inputStream: () => java.io.InputStream)
