/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.8.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.                                         ║
   ║                                                                                                           ║
   ║ The primary distribution site is: https://propensive.com/                                                 ║
   ║                                                                                                           ║
   ║ Licensed under  the Apache License,  Version 2.0 (the  "License"); you  may not use  this file  except in ║
   ║ compliance with the License. You may obtain a copy of the License at                                      ║
   ║                                                                                                           ║
   ║     http://www.apache.org/licenses/LICENSE-2.0                                                            ║
   ║                                                                                                           ║
   ║ Unless required  by applicable law  or agreed to in  writing, software  distributed under the  License is ║
   ║ distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. ║
   ║ See the License for the specific language governing permissions and limitations under the License.        ║
   ╚═══════════════════════════════════════════════════════════════════════════════════════════════════════════╝
*/
package fury.ogdl

import probably._

import scala.collection.immutable.{SortedSet, TreeSet}
import scala.language.implicitConversions
import scala.util.{Try, Success}

object OgdlReaderTest extends TestApp {
  private[this] val empty = Ogdl(Vector())

  private case class Foo(bar: String, baz: Int)

  override def tests(): Unit = {
    test("string") {
      val ogdl = Ogdl(Vector(("Hello World!", empty)))
      Try(implicitly[OgdlReader[String]].read(ogdl))
    }.assert(_ == Success("Hello World!"))

    test("case class") {
      val ogdl = Ogdl(Vector(
        ("bar",Ogdl(Vector(("1", empty)))),
        ("baz",Ogdl(Vector(("2", empty))))
      ))
      Try(implicitly[OgdlReader[Foo]].read(ogdl))
    }.assert(_ == Success( Foo(bar = "1", baz = 2)))

    test("sorted set of integers") {
      val ogdl = Ogdl(Vector(("1",Ogdl(Vector(("1",empty)))), ("2",Ogdl(Vector(("2",empty)))), ("3",Ogdl(Vector(("3",empty))))))
      Try(implicitly[OgdlReader[SortedSet[Int]]].read(ogdl))
    }.assert(_ == Success(TreeSet(1, 2, 3)))

    test("list of strings") {
      val ogdl = Ogdl(
        Vector(
          ("::",Ogdl(Vector(
            ("head",Ogdl(Vector(("3",empty)))),
            ("tl$access$1",Ogdl(Vector(
              ("::",Ogdl(Vector(
                ("head",Ogdl(Vector(("2",empty)))),
                ("tl$access$1",Ogdl(Vector(
                  ("::",Ogdl(Vector(
                    ("head",Ogdl(Vector(("1",empty)))),
                    ("tl$access$1",Ogdl(Vector(
                      ("Nil",empty)
                    )
                    )))
                  )))
                )))
              )))
            )))
          )))
      )
      Try(implicitly[OgdlReader[List[String]]].read(ogdl))
    }.assert(_ == Success(List("3", "2", "1")))

  }
}
