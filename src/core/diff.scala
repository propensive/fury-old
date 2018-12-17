/*
  Fury, version 0.1.2. Copyright 2018 Jon Pretty, Propensive Ltd.

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

import magnolia._
import language.experimental.macros

object Diff extends Diff_1 {

  type Typeclass[T] = Diff[T]

  def combine[T: StringShow](caseClass: CaseClass[Diff, T]): Diff[T] = (l, r) =>
    caseClass.parameters.flatMap { param =>
      param.typeclass.diff(param.dereference(l), param.dereference(r)).map { d =>
        d.copy(label = if(d.label.string(Theme.NoColor).isEmpty) msg"${param.label}" else msg"${d.label}")
      }
    }

  def dispatch[T: StringShow](sealedTrait: SealedTrait[Diff, T]): Diff[T] = { (l, r) =>
    val leftSubtype = sealedTrait.subtypes.find(_.cast.isDefinedAt(l)).get
    val rightSubtype = sealedTrait.subtypes.find(_.cast.isDefinedAt(r)).get
    if(leftSubtype == rightSubtype) leftSubtype.typeclass.diff(leftSubtype.cast(l), leftSubtype.cast(r))
    else List(Difference(msg"${sealedTrait.typeName.short.toLowerCase}", msg"", msg"${leftSubtype.typeName.short.toLowerCase}", msg"${rightSubtype.typeName.short.toLowerCase}"))
  }

  implicit def traversableDiff[T: Diff: MsgShow: EntityName](implicit stringShow: StringShow[T]): Diff[Traversable[T]] = { (l, r) =>
    val leftSet: Set[T] = l.to[Set]
    val rightSet: Set[T] = r.to[Set]
    val leftOnly: Set[T] = (leftSet.map(stringShow.show) -- rightSet.map(stringShow.show)).flatMap { id =>
        leftSet.find(stringShow.show(_) == id).to[Set] }
    
    val rightOnly: Set[T] = (rightSet.map(stringShow.show) -- leftSet.map(stringShow.show)).flatMap { id =>
        rightSet.find(stringShow.show(_) == id).to[Set] }
    
    val common = (leftSet -- leftOnly).to[List].sortBy(stringShow.show).zip((rightSet -- rightOnly).to[List].sortBy(stringShow.show))
    
    (common.flatMap { case (l, r) => implicitly[Diff[T]].diff(l, r).map { d => d.copy(label = msg"${implicitly[MsgShow[T]].show(l)} » ${d.label}") } }) ++
    (leftOnly.map { e => Difference(implicitly[EntityName[T]].name, msg"$e", msg"", msg"${Ansi.red("missing")}") }) ++
    (rightOnly.map { e => Difference(implicitly[EntityName[T]].name, msg"$e", msg"${Ansi.red("missing")}", msg"") })
    
  }
 
  implicit def optionDiff[T: Diff: MsgShow: EntityName]: Diff[Option[T]] = { (l, r) =>
    if(l.isDefined && r.isDefined) implicitly[Diff[T]].diff(l.get, r.get)
    else if(l.isEmpty && r.isEmpty) Nil
    else if(l.isEmpty) List(Difference(msg"optional ${implicitly[EntityName[T]].name}", msg"", msg"${Ansi.yellow("none")}", implicitly[MsgShow[T]].show(r.get)))
    else List(Difference(msg"optional ${implicitly[EntityName[T]].name}", msg"", implicitly[MsgShow[T]].show(l.get), msg"${Ansi.yellow("none")}"))
  }

  implicit val stringDiff: Diff[String] = (l, r) =>
    if(l == r) Nil else List(Difference(msg"value", msg"", msg"$l", msg"$r"))

  implicit val intDiff: Diff[Int] = (l, r) => stringDiff.diff(l.toString, r.toString)
  implicit val booleanDiff: Diff[Boolean] = (l, r) => stringDiff.diff(l.toString, r.toString)
  implicit val parameterDiff: Diff[Parameter] = (l, r) => stringDiff.diff(l.name, r.name)
  
  implicit val moduleRefDiff: Diff[ModuleRef] =
    (l, r) => if(l == r) Nil else List(Difference(msg"ref", msg"", msg"$l", msg"$r"))
  
  implicit val sourceDiff: Diff[Source] =
    (l, r) => if(l == r) Nil else List(Difference(msg"source", msg"", msg"$l", msg"$r"))
 
}

trait Diff_1 {
  implicit def gen[T]: Diff[T] = macro Magnolia.gen[T]
}

trait Diff[-T] { def diff(left: T, right: T): Seq[Difference] }
case class Difference(entity: UserMsg, label: UserMsg, left: UserMsg, right: UserMsg)

object EntityName extends EntityName_1 {
  type Typeclass[T] = EntityName[T]
  
  def combine[T](caseClass: CaseClass[EntityName, T]): EntityName[T] =
    EntityName(msg"${caseClass.typeName.short.toLowerCase}")

  implicit def gen[T]: EntityName[T] = macro Magnolia.gen[T]
}

trait EntityName_1 {
  implicit def fallback[T]: EntityName[T] = EntityName[T]("")
}

case class EntityName[T](name: UserMsg)
