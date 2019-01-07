/*
  Fury, version 0.2.2. Copyright 2019 Jon Pretty, Propensive Ltd.

  The primary distribution site is: https://propensive.com/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required  by applicable  law or  agreed to  in writing,  software  distributed  under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
  express  or  implied.  See  the  License for  the specific  language  governing  permissions and
  limitations under the License.
                                                                                                  */
package fury

import java.io.IOException
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.Files

import kaleidoscope._
import magnolia._
import mitigation._

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.SortedSet
import scala.language.experimental.macros
import scala.language.higherKinds

case class FileNotFound(path: Path) extends Exception
case class FileWriteError(path: Path) extends Exception
case class ConfigFormatError(path: Path) extends Exception

final case class Ogdl(values: Vector[(String, Ogdl)]) {
  def only: String = values.head._1
}

object Ogdl {

  def apply[T: OgdlWriter](value: T): Ogdl = implicitly[OgdlWriter[T]].write(value)
 
  def serialize(node: Ogdl): String = {
    val sb = new StringBuilder()
    serialize(sb, node)
    sb.append('\n')
    sb.toString
  }

  def serialize(sb: StringBuilder, node: Ogdl, i: Int = 0, current: Int = 0): Int = node match {
    case Ogdl(Vector()) => current
    case Ogdl((k, v) +: t) =>
      var c = current
      if(c > i) {
        sb.append('\n')
        c = 0
      }
      while(i > c) {
        sb.append('\t')
        c += 1
      }
      sb.append(k)
      c = serialize(sb, v, i + 1, c)
      serialize(sb, Ogdl(t), i, c)
  }

  def write[T: OgdlWriter](value: T, path: Path): Result[Unit, ~ | FileWriteError] =
    Result.rescue[IOException](_ => FileWriteError(path)) {
      val bak = path.rename { f => s".$f.bak" }
      if(path.exists()) path.copyTo(bak)
      val sb = new StringBuilder()
      Ogdl.serialize(sb, implicitly[OgdlWriter[T]].write(value))
      sb.append('\n')
      path.writeSync(sb.toString).unit
    }

  def read[T: OgdlReader](path: Path): Result[T, ~ | FileNotFound | ConfigFormatError] = {

    Result.rescue[IOException]{ _: IOException => FileNotFound(path) }{
      val buffer = readToBuffer(path)
      val ogdl = OgdlParser.parse(buffer)

      implicitly[OgdlReader[T]].read(ogdl)
    }
  }

  private[this] def readToBuffer(path: Path): ByteBuffer = {
    val inChannel = FileChannel.open(path.javaPath)

    try {
      val size = inChannel.size.toInt
      val buffer = ByteBuffer.allocate(size)

      inChannel.read(buffer)
      buffer.flip()

      buffer
    } finally {
      inChannel.close()
    }
  }
}

object OgdlWriter {

  type Typeclass[T] = OgdlWriter[T]

  def combine[T](caseClass: CaseClass[OgdlWriter, T]): OgdlWriter[T] = { value =>
    if(caseClass.isValueClass || caseClass.parameters.length == 1) {
      val param = caseClass.parameters.head
      param.typeclass.write(param.dereference(value))
    } else Ogdl(caseClass.parameters.to[Vector].map { param =>
      (param.label, param.typeclass.write(param.dereference(value)))
    })
  }

  def dispatch[T](sealedTrait: SealedTrait[OgdlWriter, T]): OgdlWriter[T] = { value =>
    sealedTrait.dispatch(value) { subtype =>
      subtype.typeclass.write(subtype.cast(value)) match {
        case Ogdl(map) => Ogdl(Vector((subtype.typeName.short -> Ogdl(map))))
      }
    }
  }

  implicit val string: OgdlWriter[String] = string => Ogdl(Vector((string, Ogdl(Vector()))))
  implicit val int: OgdlWriter[Int] = i => Ogdl(i.toString)
  implicit val boolean: OgdlWriter[Boolean] = b => Ogdl(b.toString)

  implicit def list[T: OgdlWriter: StringShow]: OgdlWriter[List[T]] = coll => Ogdl {
    if(coll.isEmpty) Vector(("", Ogdl(Vector())))
    else (coll.to[Vector].map { e => implicitly[StringShow[T]].show(e) -> Ogdl(e) })
  }
  
  implicit def treeSet[T: OgdlWriter: StringShow]: OgdlWriter[SortedSet[T]] = coll => Ogdl {
    if(coll.isEmpty) Vector(("", Ogdl(Vector())))
    else (coll.to[Vector].map { e => implicitly[StringShow[T]].show(e) -> Ogdl(e) })
  }
  
  implicit def gen[T]: OgdlWriter[T] = macro Magnolia.gen[T]
}

trait OgdlWriter[T] { def write(value: T): Ogdl }

object OgdlReader {
  type Typeclass[T] = OgdlReader[T]

  def combine[T](caseClass: CaseClass[OgdlReader, T]): OgdlReader[T] = {
    case ogdl @ Ogdl(list) =>
      val map = list.toMap
      if(caseClass.isValueClass || caseClass.parameters.length == 1)
        caseClass.construct(_.typeclass.read(ogdl))
      else caseClass.construct { param =>
        if(map.contains(param.label)) param.typeclass.read(map(param.label))
        else param.default.getOrElse(throw new RuntimeException(s"missing value ${param.label}"))
      }
  }

  def dispatch[T](sealedTrait: SealedTrait[OgdlReader, T]): OgdlReader[T] = {
    case Ogdl(Vector((typeName, map))) =>
      sealedTrait.subtypes.find(_.typeName.short == typeName).getOrElse{
        throw new RuntimeException(s"type $typeName not recognized")
      }.typeclass.read(map)
  }

  implicit val string: OgdlReader[String] = _.only
  implicit val int: OgdlReader[Int] = _.only.toInt
  implicit val boolean: OgdlReader[Boolean] = _.only.toBoolean

  implicit def traversable[Coll[t] <: Traversable[t], T: OgdlReader](
    implicit cbf: CanBuildFrom[Nothing, T, Coll[T]]
  ): OgdlReader[Coll[T]] = {
    case ogdl @ Ogdl(vector) =>
      if(vector.head._1 == "") Vector[T]().to[Coll]
      else vector.map { v =>
        implicitly[OgdlReader[T]].read(v._2)
      }.to[Coll]
  }

  implicit def gen[T]: OgdlReader[T] = macro Magnolia.gen[T]
}

trait OgdlReader[T] { def read(ogdl: Ogdl): T }

case class ZipfileEntry(name: String, inputStream: () => java.io.InputStream)

object Path {
  def temp(): Path = Path(java.nio.file.Files.createTempFile("", "").toAbsolutePath().toString())
  def unapply(str: String): Option[Path] = str match {
    case r"""$dir@([^*?:;,&|"\%<>]*)/""" =>
      Some(Path(if(dir.endsWith("/")) dir.dropRight(1) else dir))
    case _ => None
  }
}

case class Path(value: String) {
  def filename: String = value.replaceAll("/$", "")
  def javaPath = java.nio.file.FileSystems.getDefault().getPath(value)

  def name = javaPath.toFile.getName

  import java.io.FileNotFoundException
  import java.util.zip._

  import scala.collection.JavaConverters._
  def zipfileEntries: Result[List[ZipfileEntry], ~ | FileNotFound] = for {
    zipFile <- Result.rescue[FileNotFoundException](FileNotFound(this))(new ZipFile(filename))
    entries <- ~zipFile.entries
    entriesList = entries.asScala.to[List]
  } yield entriesList.map { entry =>
    ZipfileEntry(entry.getName, () => zipFile.getInputStream(entry))
  }

  def in(root: Path): Path = Path(s"${root.value}/$value")

  def move(newPath: Path) = javaPath.toFile.renameTo(newPath.javaPath.toFile)

  def relativize(root: Path): Path = Path(value.drop(root.value.length + 1))

  def /(child: String): Path = Path(s"$filename/$child")

  def fileCount(pred: String => Boolean): Int = {
    Option(javaPath.toFile.listFiles).map { files =>
      val found = files.count { f => pred(f.getName) }
      found + files.filter(_.isDirectory).map { f => Path(f.getAbsolutePath).fileCount(pred) }.sum
    }.getOrElse(0)
  }

  def describe(pred: String => Boolean): String = {
    val size = fileSize(pred)
    val count = fileCount(pred)
    val sizeStr = if(size < 1024) s"${size}B"
    else if(size < 1024*1024) s"${size/1024}kiB"
    else s"${size/(1024*1024)}MiB"

    s"$count source files, $sizeStr"
  }

  def fileSize(pred: String => Boolean): Long = {
    Option(javaPath.toFile.listFiles).map { files =>
      val found = files.map { f => if(pred(f.getName)) f.length else 0 }.sum
      found + files.filter(_.isDirectory).map { f => Path(f.getAbsolutePath).fileSize(pred) }.sum
    }.getOrElse(0)
  }

  def moveTo(path: Path): Result[Unit, ~ | FileWriteError] =
    Result.rescue[java.io.IOException](FileWriteError(this)) {
      java.nio.file.Files.move(javaPath, path.javaPath).unit()
    }

  def findSubdirsContaining(predicate: String => Boolean): Set[Path] = {
    Option(javaPath.toFile.listFiles).map { files =>
      val found = if(files.exists { f => predicate(f.getName) }) Set(this) else Set()
      
      val subdirs = files.filter(_.isDirectory).filter(!_.getName.startsWith(".")).map { f =>
        Path(f.getAbsolutePath)
      }.to[Set]
      
      subdirs.flatMap(_.findSubdirsContaining(predicate)) ++ found
    }.getOrElse(Set())
  }

  def delete(): Result[Boolean, ~ | FileWriteError] =
    Result.rescue[java.io.IOException](FileWriteError(this)) {
      recursiveDelete(javaPath.toFile)
    }

  def children: List[String] = {
    val f = javaPath.toFile
    if(f.exists) f.listFiles.to[List].map(_.getName) else Nil
  }

  private[this] def recursiveDelete(file: java.io.File): Boolean =
    if(file.isDirectory) file.listFiles.foldLeft(true)(_ && recursiveDelete(_)) && file.delete()
    else file.delete()

  def writeSync(content: String): Result[Unit, ~ | FileWriteError] = try {
    val writer = new java.io.BufferedWriter(new java.io.FileWriter(javaPath.toFile))
    writer.write(content)
    Answer(writer.close())
  } catch { case e: java.io.IOException => Result.abort(FileWriteError(this)) }
  
  def appendSync(content: String): Result[Unit, ~ | FileWriteError] = try {
    val writer = new java.io.BufferedWriter(new java.io.FileWriter(javaPath.toFile))
    writer.append(content)
    Answer(writer.close())
  } catch { case e: java.io.IOException => Result.abort(FileWriteError(this)) }
  
  def exists(): Boolean = javaPath.toFile.exists()

  def directory: Result[Path, ~ | FileWriteError] = {
    val file = javaPath.toFile
    if(!file.exists()) {
      mkdir()
      if(file.exists()) Answer(this) else Result.abort(FileWriteError(this))
    } else if(file.isDirectory) Answer(this) else Result.abort(FileWriteError(this))
  }

  def copyTo(path: Path): Result[Path, ~ | FileWriteError] =
    Result.rescue[java.io.IOException](FileWriteError(path)) {
      Files.copy(javaPath, path.javaPath, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
      path
    }

  def existing(): Result[Path, ~ | FileNotFound] =
    if(exists()) Answer(this) else Result.abort(FileNotFound(this))

  def mkdir(): Unit = java.nio.file.Files.createDirectories(javaPath).unit()
  
  def parent = Path(javaPath.getParent.toString)
 
  def rename(fn: String => String) = parent / fn(name)

  def mkParents(): Result[Path, ~ | FileWriteError] = {
    Result.rescue[java.io.IOException](_ => FileWriteError(parent)) {
      java.nio.file.Files.createDirectories(parent.javaPath)
      this
    }
  }
}



