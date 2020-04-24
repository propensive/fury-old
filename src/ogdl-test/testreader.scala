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

import probably._

import scala.collection.immutable.{SortedSet, TreeSet}
import scala.language.implicitConversions
import scala.util.{Try, Success}

object OgdlReaderTest extends TestApp {
  private[this] val empty = Ogdl(Vector())

  private[this] case class Foo(bar: String, baz: Int)
  private[this] implicit val ord: Ordering[Foo] = Ordering[Int].on[Foo](_.baz)

  private[this] case class Foo2(bar: Option[String], baz: Int)
  private[this] implicit val ord2: Ordering[Foo2] = Ordering[Int].on[Foo2](_.baz)

  private[this] case class Quux(handle: String, data: SortedSet[String])

  override def tests(): Unit = {
    test("string") {
      val ogdl = Ogdl(Vector(("Hello World!", empty)))
      Try(implicitly[OgdlReader[String]].read(ogdl))
    }.assert(_ == Success("Hello World!"))

    test("option") {
      val ogdl = Ogdl(Vector(("Some",Ogdl(Vector(("false",empty))))))
      Try(implicitly[OgdlReader[Option[Boolean]]].read(ogdl))
    }.assert(_ == Success(Some(false)))

    test("case class") {
      val ogdl = Ogdl(Vector(
        ("bar",Ogdl(Vector(("1", empty)))),
        ("baz",Ogdl(Vector(("2", empty))))
      ))
      Try(implicitly[OgdlReader[Foo]].read(ogdl))
    }.assert(_ == Success( Foo(bar = "1", baz = 2)))

    test("list of strings") {
      val ogdl = Ogdl(Vector(("foo", Ogdl(Vector(("bar", Ogdl(Vector(("baz", empty)))))))))
      Try(implicitly[OgdlReader[List[String]]].read(ogdl))
    }.assert(_ == Success(List("foo", "bar", "baz")))

    test("sorted set of integers") {
      val ogdl = Ogdl(Vector(("1", Ogdl(Vector(("2", Ogdl(Vector(("3", empty)))))))))
      Try(implicitly[OgdlReader[SortedSet[Int]]].read(ogdl))
    }.assert(_ == Success(TreeSet(1, 2, 3)))

    test("sorted set of case classes") {
      implicit val index: Index[Foo] = FieldIndex("bar")
      val ogdl = Ogdl(Vector(
        ("B",Ogdl(Vector(("baz",Ogdl(Vector(("1",empty))))))),
        ("A",Ogdl(Vector(("baz",Ogdl(Vector(("2",empty))))))),
        ("B",Ogdl(Vector(("baz",Ogdl(Vector(("3",empty)))))))
      ))
      Try(implicitly[OgdlReader[SortedSet[Foo]]].read(ogdl))
    }.assert(_ == Success(TreeSet(Foo(bar = "B", baz = 1), Foo(bar = "A", baz = 2), Foo(bar = "B", baz = 3))))

    test("case class with a sorted set") {
      implicit val index: Index[Quux] = FieldIndex("handle")
      val ogdl = Ogdl(Vector(
        ("handle",Ogdl(Vector(("Q",empty)))),
        ("data",Ogdl(Vector(("A", Ogdl(Vector(("B", Ogdl(Vector(("C", empty))))))))))
      ))
      Try(implicitly[OgdlReader[Quux]].read(ogdl))
    }.assert(_ == Success(Quux("Q", TreeSet("A", "B", "C"))))

    test("sorted set of case classes with complex index") {
      implicit val index: Index[Foo2] = FieldIndex("bar")
      val ogdl = Ogdl(Vector(
        ("None",Ogdl(Vector(("baz",Ogdl(Vector(("1",empty))))))),
        ("kvp",Ogdl(Vector(("bar",Ogdl(Vector(("Some",Ogdl(Vector(("A",empty))))))), ("value",Ogdl(Vector(("baz",Ogdl(Vector(("2",empty)))))))))),
        ("kvp",Ogdl(Vector(("bar",Ogdl(Vector(("Some",Ogdl(Vector(("B",empty))))))), ("value",Ogdl(Vector(("baz",Ogdl(Vector(("3",empty)))))))))),
        ("None",Ogdl(Vector(("baz",Ogdl(Vector(("4",empty)))))))
      ))
      Try(implicitly[OgdlReader[SortedSet[Foo2]]].read(ogdl))
    }.assert(_ == Success(TreeSet(
      Foo2(bar = Some("A"), baz = 2),
      Foo2(bar = Some("B"), baz = 3),
      Foo2(bar = None, baz = 1),
      Foo2(bar = None, baz = 4)
    )))

  }
}
