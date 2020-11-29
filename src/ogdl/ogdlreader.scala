/*

    Fury, version 0.31.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.ogdl

import fury.text._

import magnolia._

import scala.collection.generic.CanBuildFrom
import scala.language.experimental.macros
import scala.language.higherKinds

trait OgdlReader[T] { def read(ogdl: Ogdl): T }

case class OgdlException(path: List[String], message: UserMsg) extends FuryException

object OgdlReader {
  type Typeclass[T] = OgdlReader[T]

  private def prefix[T](label: String)(fn: => T): T = try fn catch {
    case OgdlException(path, msg) => throw OgdlException(label :: path, msg)
  }

  def combine[T](caseClass: CaseClass[OgdlReader, T]): OgdlReader[T] = {
    case ogdl@Ogdl(list) =>
      val map = list.toMap
      if(caseClass.isValueClass || caseClass.parameters.length == 1) caseClass.construct { p =>
        try p.typeclass.read(ogdl) catch {
          case OgdlException(path, message) => throw OgdlException(p.label :: path, message)
        }
      }
      else caseClass.construct { param =>
        if(map.contains(param.label)) prefix(param.label)(param.typeclass.read(map(param.label)))
        else param.default.getOrElse(throw OgdlException(List(param.label), msg"Missing value in $ogdl"))
      }
  }

  def dispatch[T](sealedTrait: SealedTrait[OgdlReader, T]): OgdlReader[T] = {
    case Ogdl(Vector((typeName, map))) =>
      prefix(typeName) { sealedTrait.subtypes.find(_.typeName.short == typeName).getOrElse {
        throw OgdlException(List(str"[$typeName]"), msg"Subtype not recognized")
      }.typeclass.read(map) }

    case other =>
      throw OgdlException(List(
          str"[${sealedTrait.typeName.short}]"), msg"Expected a valid subtype, but found $other.")
  }

  implicit val string: OgdlReader[String] = _()
  implicit val int: OgdlReader[Int] = _().toInt
  implicit val long: OgdlReader[Long] = _().toLong
  implicit val boolean: OgdlReader[Boolean] = _().toBoolean
  implicit val theme: OgdlReader[Theme] = ogdl => Theme.unapply(ogdl()).getOrElse(Theme.Full)
  
  implicit def traversable[Coll[t] <: Traversable[t], T: OgdlReader: Index]
                          (implicit cbf: CanBuildFrom[Nothing, T, Coll[T]])
                          : OgdlReader[Coll[T]] =
    implicitly[Index[T]] match {
      case fi@FieldIndex(idx) =>
        prefix(idx)(complexTraversable[Coll, T](implicitly[OgdlReader[T]], fi, cbf).read(_))

      case si@SelfIndexed() =>
        simpleTraversable[Coll, T](implicitly[OgdlReader[T]], si, cbf).read(_)
    }

  private def simpleTraversable[Coll[t] <: Traversable[t], T: OgdlReader: SelfIndexed]
                               (implicit cbf: CanBuildFrom[Nothing, T, Coll[T]])
                               : OgdlReader[Coll[T]] = {
    case Ogdl(vector) =>
      if(vector.isEmpty) Vector[T]().to[Coll]
      else vector.head match { case (f, r) =>
        val first = implicitly[OgdlReader[T]].read(Ogdl(f))
        val rest = simpleTraversable[Coll, T].read(r)
        (first +: rest.toSeq).to[Coll]
      }
  }

  private def complexTraversable[Coll[t] <: Traversable[t], T: OgdlReader: FieldIndex]
                                (implicit cbf: CanBuildFrom[Nothing, T, Coll[T]])
                                : OgdlReader[Coll[T]] = {
    case Ogdl(vector) =>
      if(vector.head._1 == "") Vector[T]().to[Coll]
      else vector.map { element =>
        val index = implicitly[FieldIndex[T]]
        
        val data: Ogdl = element match {
          case ("kvp", kvp) =>
            val key = Ogdl(Vector(index.field -> kvp.selectDynamic(index.field)))
            val rest = kvp.selectDynamic("value")
            Ogdl(key.values ++ rest.values)

          case (single, rest) =>
            val key = Ogdl(Vector(index.field -> Ogdl(Vector(single -> Ogdl(Vector())))))
            Ogdl(key.values ++ rest.values)
        }

        prefix(element._1)(implicitly[OgdlReader[T]].read(data))
      }.to[Coll]
  }

  implicit def gen[T]: OgdlReader[T] = macro Magnolia.gen[T]
}
