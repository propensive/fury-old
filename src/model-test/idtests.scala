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
package fury

import fury.model._
import probably._

import scala.language.implicitConversions
import scala.util.{Success, Try}

object IdTests extends TestApp {

  override def tests(): Unit = {

    test("binary ids") {
      BinaryId.unapply("foo.bar:baz-quux_2.12:1.2.3-RC4").isEmpty &&
      BinaryId.unapply("foo.bar:baz-quux_2.12").isEmpty &&
      BinaryId.unapply("baz-quux_2.12").isDefined
    }.assert(_ == true)

    test("binary specs") {
      BinSpec.unapply("foo.bar:baz-quux_2.12:1.2.3-RC4").isDefined &&
      BinSpec.unapply("foo.bar:baz-quux_2.12").isEmpty &&
      BinSpec.unapply("baz-quux_2.12").isEmpty
    }.assert(_ == true)

    test("repo ids") {
      RepoId.unapply("foo").isEmpty
    }.assert(_ == true)

  }
}
