package fury

import magnolia.{CaseClass, Magnolia, SealedTrait}

import scala.collection.generic.CanBuildFrom
import scala.language.experimental.macros
import scala.language.higherKinds

trait OgdlReader[T] {
  def read(ogdl: Ogdl): T
}

object OgdlReader {
  type Typeclass[T] = OgdlReader[T]

  def combine[T](caseClass: CaseClass[OgdlReader, T]): OgdlReader[T] = {
    case ogdl@Ogdl(list) =>
      val map = list.toMap
      if(caseClass.isValueClass || caseClass.parameters.length == 1)
        caseClass.construct(_.typeclass.read(ogdl))
      else caseClass.construct{ param =>
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
    case ogdl@Ogdl(vector) =>
      if(vector.head._1 == "") Vector[T]().to[Coll]
      else vector.map{ v =>
        implicitly[OgdlReader[T]].read(v._2)
      }.to[Coll]
  }

  implicit def gen[T]: OgdlReader[T] = macro Magnolia.gen[T]
}