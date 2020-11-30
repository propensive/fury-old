/*

    Fury, version 0.32.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

sealed trait Uniqueness[Ref, Origin] {
  def +(other: Uniqueness[Ref, Origin]): Uniqueness[Ref, Origin]
  def allOrigins: Set[Origin]
  def one: Option[Origin]
  def any: Option[Origin]
}

object Uniqueness {
  case class Unique[Ref, Origin](ref: Ref, origins: Set[Origin]) extends Uniqueness[Ref, Origin] {
    override def +(other: Uniqueness[Ref, Origin]): Uniqueness[Ref, Origin] = other match {
      case Unique(ref, origins) if ref == this.ref => Unique(ref, this.origins ++ origins)
      case Unique(ref, origins) =>
        val theseOrigins = this.origins.map { k => k -> this.ref }
        val thoseOrigins = origins.map { k => k -> ref }
        Ambiguous((theseOrigins ++ thoseOrigins).toMap)
      case Ambiguous(origins) => Ambiguous(origins ++ this.origins.map(i => i -> this.ref))
    }

    override def allOrigins: Set[Origin] = origins
    def one: Option[Origin] = Some(origins.head)
    def any: Option[Origin] = None
  }

  case class Ambiguous[Ref, Origin] private[Uniqueness](origins: Map[Origin, Ref]) extends Uniqueness[Ref, Origin] {
    override def +(other: Uniqueness[Ref, Origin]): Uniqueness[Ref, Origin] = other match {
      case Unique(ref, origins) => Ambiguous(this.origins ++ origins.map(i => i -> ref))
      case Ambiguous(origins) => Ambiguous(this.origins ++ origins)
    }

    override def allOrigins: Set[Origin] = origins.keySet
    def one: Option[Origin] = None
    def any: Option[Origin] = Some(origins.head._1)
  }
}

