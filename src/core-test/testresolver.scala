/*

    Fury, version 0.18.12. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.test

import probably._

import fury.core._
import fury.model.ItemNotFound
import fury.text._

import scala.collection.immutable.SortedSet
import scala.util.{Failure, Success}

object ResolverTest extends Suite() {

  case class TestEntity(id: TestEntityId)
  case class TestEntityId(key: String) extends fury.model.Key(msg"foo")

  implicit val msgShow: MsgShow[TestEntityId] = p => UserMsg(_ => p.key)
  implicit val resolver: Resolver[TestEntity, TestEntityId] = _ == _.id

  def run(test: Runner): Unit = {
    val entities = Seq(entity("abc"), entity("def"), entity("ghi"))

    test("Seq findBy") {
      entities.findBy(id("def"))
    }.assert(_ == Success(entity("def")))

    test("Seq missing") {
      entities.findBy(id("xyz"))
    }.assert(_.isFailure)

    test("Seq contains") {
      // FIXME The method ResolverExt.contains is obscured by SeqOps.contains
      (
        entities.contains(id("ghi")),
        (new ResolverExt(entities)).contains(id("ghi"))
      )
    }.assert(_ == (false, true))

    test("Seq unique") {
      entities.unique(id("abc"))
    }.assert(_.isFailure)

    test("Set evict") {
      implicit val ordering: Ordering[TestEntity] = Ordering.by(_.id.key)
      entities.to[SortedSet].evict(id("def"))
    }.assert(_ == Set(entity("abc"), entity("ghi")))

    test("Map findBy") {
      val entries = Map(id("foo") -> 12, id("bar") -> 34, id("baz") -> 56)
      entries.findBy(id("bar"))
    }.assert(_ == Success(34))

    test("Map missing") {
      val entries = Map(id("foo") -> 12, id("bar") -> 34, id("baz") -> 56)
      entries.findBy(id("xxx"))
    }.assert(_.isFailure)

  }

  private[this] def id(key: String) = TestEntityId(key)
  private[this] def entity(key: String) = TestEntity(id(key))

}
