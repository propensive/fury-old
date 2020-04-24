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
package fury.ogdl

import fury.strings._

import magnolia.{CaseClass, Magnolia, SealedTrait}

import scala.collection.immutable.SortedSet
import scala.language.experimental.macros
import scala.language.higherKinds

trait OgdlWriter[T] {
  def write(value: T): Ogdl
}

object OgdlWriter {
  type Typeclass[T] = OgdlWriter[T]

  def structurallyEqual(a: Any, b: Any): Boolean = a match {
    case a: SortedSet[_] => b match {
      case b: SortedSet[_] => a.size == b.size && a.zip(b).forall { case (a, b) => structurallyEqual(a, b) }
      case _ => false
    }
    case a => a == b
  }

  def combine[T](caseClass: CaseClass[OgdlWriter, T]): OgdlWriter[T] = { value =>
    if(caseClass.isValueClass || caseClass.parameters.length == 1) {
      val param = caseClass.parameters.head
      param.typeclass.write(param.dereference(value))
    } else
      Ogdl(caseClass.parameters.to[Vector].flatMap { param =>
        if(param.default.isDefined && structurallyEqual(param.default.get, param.dereference(value))) Vector()
        else Vector((param.label, param.typeclass.write(param.dereference(value))))
      })
  }

  def dispatch[T](sealedTrait: SealedTrait[OgdlWriter, T]): OgdlWriter[T] = { value =>
    sealedTrait.dispatch(value) { subtype =>
      subtype.typeclass.write(subtype.cast(value)) match {
        case Ogdl(map) => Ogdl(Vector(subtype.typeName.short -> Ogdl(map)))
      }
    }
  }

  implicit val string: OgdlWriter[String] = string => Ogdl(Vector((string, Ogdl(Vector()))))
  implicit val int: OgdlWriter[Int] = int => Ogdl(int.toString)
  implicit val long: OgdlWriter[Long] = long => Ogdl(long.toString)
  implicit val boolean: OgdlWriter[Boolean] = boolean => Ogdl(boolean.toString)
  implicit val theme: OgdlWriter[Theme] = theme => Ogdl(theme.name)

  implicit def list[T: OgdlWriter: Index]: OgdlWriter[List[T]] = implicitly[Index[T]] match {
    case fi@FieldIndex(_) => complexTraversable[T](implicitly[OgdlWriter[T]], fi).write(_)
    case si@SelfIndexed() => simpleTraversable[T](implicitly[OgdlWriter[T]], si).write(_)
  }

  implicit def treeSet[T: OgdlWriter: Index, X[T] <: SortedSet[T]]: OgdlWriter[X[T]] =
    implicitly[Index[T]] match {
      case fi@FieldIndex(_) => complexTraversable[T](implicitly[OgdlWriter[T]], fi).write(_)
      case si@SelfIndexed() => simpleTraversable[T](implicitly[OgdlWriter[T]], si).write(_)
    }

  private def complexTraversable[T: OgdlWriter: FieldIndex]: OgdlWriter[Traversable[T]] = coll =>
    Ogdl {
      if(coll.isEmpty) Vector(("", Ogdl(Vector())))
      else coll.to[Vector].map(implicitly[Index[T]].writeCollectionItem)
    }

  private def simpleTraversable[T: OgdlWriter: SelfIndexed]: OgdlWriter[Traversable[T]] = coll =>
    coll.to[Vector].foldRight(Ogdl(Vector())) {
      case (element, tail) => Ogdl(Vector(implicitly[Index[T]].writeCollectionItem(element)._1 -> tail))
    }

  implicit def gen[T]: OgdlWriter[T] = macro Magnolia.gen[T]
}

sealed trait Index[T] {
  def writeCollectionItem(value: T): (String, Ogdl)
}
case class FieldIndex[T: OgdlWriter](field: String) extends Index[T] {
  override def writeCollectionItem(value: T): (String, Ogdl) = {
    val data = Ogdl(value)
    val dedup = data.values.filterNot(_._1 == field)
    val key = data.selectDynamic(field)
    val rest = Ogdl(if(dedup.isEmpty) Vector("" -> Ogdl(Vector())) else dedup)
    tuple(key, rest)
  }

  private def tuple(key: Ogdl, value: Ogdl): (String, Ogdl) = key.values match {
    case Vector((single, rest)) if rest.values.isEmpty => single -> value
    case _ => "kvp" -> Ogdl(Vector(field -> key, "value" -> value))
  }
}
case class SelfIndexed[T: OgdlWriter: StringShow]() extends Index[T] {
  override def writeCollectionItem(value: T): (String, Ogdl) = {
    implicitly[StringShow[T]].show(value) -> Ogdl(value)
  }
}
object Index {
  implicit val string = SelfIndexed[String]
  implicit val int = SelfIndexed[Int]
  implicit val long = SelfIndexed[Long]
  //implicit val boolean = SelfIndexed[Boolean]
  implicit def index[T <: {val id: AnyRef}: OgdlWriter]: FieldIndex[T] = FieldIndex[T]("id")
}
