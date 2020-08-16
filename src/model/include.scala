/*

    Fury, version 0.17.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.model

import fury.text._, fury.io._

import kaleidoscope._

import scala.reflect.ClassTag

sealed abstract class IncludeType(val key: String) extends scala.Product with scala.Serializable {
  def as[T: ClassTag]: Option[T] = this.only { case t: T => t }
  def is[T: ClassTag]: Boolean = this match { case t: T => true case _ => false }

  def name: IncludeType.Id = this match {
    case ClassesDir(deep) => ClassesDir
    case DirRef(source)   => DirRef
    case FileRef(source)  => FileRef
    case Jarfile(fat)     => Jarfile
    case TarFile(gzip)    => TarFile
  }
}

object IncludeType {
  implicit val stringShow: StringShow[IncludeType] = _.key
  implicit val ord: Ordering[IncludeType] = Ordering[String].on[IncludeType](_.key)
  implicit val msgShow: MsgShow[IncludeType] = e => UserMsg { theme => stringShow.show(e) }

  val ids: List[IncludeType.Id] = List(Jarfile, TarFile, ClassesDir, FileRef, DirRef)

  sealed abstract class Id(val key: String)

  object Id {
    def unapply(str: String): Option[Id] = ids.find(_.key == str)
    implicit val parser: Parser[Id] = unapply(_)
    implicit val stringShow: StringShow[Id] = _.key
    implicit val msgShow: MsgShow[Id] = v => UserMsg(_.param(stringShow.show(v)))
  }
}

object Jarfile extends IncludeType.Id("jar")
case class Jarfile(fat: Boolean) extends IncludeType("jar")

object TarFile extends IncludeType.Id("tar")
case class TarFile(gzip: Boolean) extends IncludeType("tar")

object ClassesDir extends IncludeType.Id("classes")
case class ClassesDir(deep: Boolean) extends IncludeType("classes")

object FileRef extends IncludeType.Id("file")
case class FileRef(source: Source) extends IncludeType("file")

object DirRef extends IncludeType.Id("dir")
case class DirRef(source: Source) extends IncludeType("dir")

object IncludeId {
  implicit val msgShow: MsgShow[IncludeId] = m => UserMsg(_.layer(m.key))
  implicit val stringShow: StringShow[IncludeId] = _.key
  implicit val diff: Diff[IncludeId] = (l, r) => Diff.stringDiff.diff(l.key, r.key)
  implicit val parser: Parser[IncludeId] = unapply(_)

  def unapply(name: String): Option[IncludeId] = name.only { case r"[a-z](-?[a-z0-9]+)*" => IncludeId(name) }
}

case class IncludeId(key: String) extends Key("include")

object Include {
  implicit val ord: Ordering[Include] = Ordering[String].on(_.id.key)
  implicit val msgShow: MsgShow[Include] = include => msg"${include.kind}:${include.path}"
  implicit val stringShow: StringShow[Include] = include => str"${include.kind}:${include.path}"
  implicit val diff: Diff[Include] = Diff.gen[Include]
  implicit val keyName: KeyName[Include] = () => msg"include"
}

case class Include(id: IncludeId, ref: ModuleRef, kind: IncludeType, path: Path)
