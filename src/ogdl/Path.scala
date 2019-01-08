package fury

import java.io.FileNotFoundException
import java.nio.file.{Files, Paths, Path => JPath}
import java.util.zip.ZipFile

import kaleidoscope._
import mitigation._

import scala.collection.JavaConverters._
import scala.language.experimental.macros
import scala.language.higherKinds

object Path {
  def unapply(str: String): Option[Path] = str match {
    case r"""$dir@([^*?:;,&|"\%<>]*)/""" =>
      Some(Path(if(dir.endsWith("/")) dir.dropRight(1) else dir))
    case _ => None
  }
}

case class Path(value: String) {
  def filename: String = value.replaceAll("/$", "")

  def javaPath: JPath = Paths.get(value)

  def name: String = javaPath.getFileName.toString

  def zipfileEntries: Result[List[ZipfileEntry], ~ | FileNotFound] = for {
    zipFile <- Result.rescue[FileNotFoundException](FileNotFound(this))(new ZipFile(filename))
    entries <- ~zipFile.entries
    entriesList = entries.asScala.to[List]
  } yield entriesList.map{ entry =>
    ZipfileEntry(entry.getName, () => zipFile.getInputStream(entry))
  }

  def /(child: String): Path = Path(s"$filename/$child")

  def in(root: Path): Path = Path(s"${root.value}/$value")

  def fileCount(pred: String => Boolean): Int = {
    Option(javaPath.toFile.listFiles).map{ files =>
      val found = files.count{ f => pred(f.getName) }
      found + files.filter(_.isDirectory).map{ f => Path(f.getAbsolutePath).fileCount(pred) }.sum
    }.getOrElse(0)
  }

  def describe(pred: String => Boolean): String = {
    val size = fileSize(pred)
    val count = fileCount(pred)
    val sizeStr = if(size < 1024) s"${size}B"
    else if(size < 1024 * 1024) s"${size / 1024}kiB"
    else s"${size / (1024 * 1024)}MiB"

    s"$count source files, $sizeStr"
  }

  def fileSize(pred: String => Boolean): Long = {
    Option(javaPath.toFile.listFiles).map{ files =>
      val found = files.map{ f => if(pred(f.getName)) f.length else 0 }.sum
      found + files.filter(_.isDirectory).map{ f => Path(f.getAbsolutePath).fileSize(pred) }.sum
    }.getOrElse(0)
  }

  def moveTo(path: Path): Result[Unit, ~ | FileWriteError] =
    Result.rescue[java.io.IOException](FileWriteError(this)){
      java.nio.file.Files.move(javaPath, path.javaPath).unit()
    }

  def findSubdirsContaining(predicate: String => Boolean): Set[Path] = {
    Option(javaPath.toFile.listFiles).map{ files =>
      val found = if(files.exists{ f => predicate(f.getName) }) Set(this) else Set()

      val subdirs = files
        .filter(_.isDirectory)
        .filterNot(_.getName.startsWith("."))
        .map(p => Path(p.getAbsolutePath))
        .to[Set]

      subdirs.flatMap(_.findSubdirsContaining(predicate)) ++ found
    }.getOrElse(Set())
  }

  def delete(): Result[Boolean, ~ | FileWriteError] = {
    def delete(file: java.io.File): Boolean =
      if(file.isDirectory) file.listFiles.forall(delete) && file.delete()
      else file.delete()

    Result.rescue[java.io.IOException](FileWriteError(this)){
      delete(javaPath.toFile)
    }
  }
  def children: List[String] = {
    val f = javaPath.toFile
    if(f.exists) f.listFiles.to[List].map(_.getName) else Nil
  }

  def writeSync(content: String): Result[Unit, ~ | FileWriteError] = try {
    val writer = new java.io.BufferedWriter(new java.io.FileWriter(javaPath.toFile))
    writer.write(content)
    Answer(writer.close())
  } catch {
    case e: java.io.IOException => Result.abort(FileWriteError(this))
  }

  def appendSync(content: String): Result[Unit, ~ | FileWriteError] = try {
    val writer = new java.io.BufferedWriter(new java.io.FileWriter(javaPath.toFile))
    writer.append(content)
    Answer(writer.close())
  } catch {
    case e: java.io.IOException => Result.abort(FileWriteError(this))
  }

  def exists(): Boolean = Files.exists(javaPath)

  def directory: Result[Path, ~ | FileWriteError] = {
    val file = javaPath.toFile
    if(!file.exists()) {
      mkdir()
      if(file.exists()) Answer(this) else Result.abort(FileWriteError(this))
    } else if(file.isDirectory) Answer(this) else Result.abort(FileWriteError(this))
  }

  def copyTo(path: Path): Result[Path, ~ | FileWriteError] =
    Result.rescue[java.io.IOException](FileWriteError(path)){
      Files.copy(javaPath, path.javaPath, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
      path
    }

  def mkdir(): Unit = java.nio.file.Files.createDirectories(javaPath).unit()

  def parent = Path(javaPath.getParent.toString)

  def rename(fn: String => String): Path = parent / fn(name)

  def mkParents(): Result[Path, ~ | FileWriteError] = {
    Result.rescue[java.io.IOException](_ => FileWriteError(parent)){
      java.nio.file.Files.createDirectories(parent.javaPath)
      this
    }
  }
}