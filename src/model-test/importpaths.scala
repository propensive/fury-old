/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.7.14. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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

object ImportPathTests extends TestApp {

  override def tests(): Unit = {
    val root = ImportPath("/")
    val here = ImportPath(".")
    val parent = ImportPath("..")

    test("root path") {
      root.isEmpty &&
      Try(root.head).isFailure &&
      Try(root.tail).isFailure
    }.assert(_ == true)

    test("current path") {
      here.isEmpty &&
        Try(here.head).isFailure &&
        Try(here.tail).isFailure
    }.assert(_ == true)

    test("parent path") {
      parent.isEmpty &&
        Try(parent.head).isFailure &&
        Try(parent.tail).isFailure
    }.assert(_ == true)

    test("absolute path") {
      val absolutePath = ImportPath("/foo")
      !absolutePath.isEmpty &&
        Try(absolutePath.head) == Success(ImportId("foo")) &&
        Try(absolutePath.tail) == Success(ImportPath("/"))
    }.assert(_ == true)

    test("relative path") {
      val relativePath = ImportPath("bar")
      relativePath.isEmpty &&
        Try(relativePath.head).isFailure &&
        Try(relativePath.tail).isFailure
    }.assert(_ == true)

    test("nested absolute path") {
      val absolutePath = ImportPath("/foo/bar")
      !absolutePath.isEmpty &&
        Try(absolutePath.head) == Success(ImportId("foo")) &&
        Try(absolutePath.tail) == Success(ImportPath("/bar"))
    }.assert(_ == true)

    test("nested relative path") {
      val relativePath = ImportPath("baz/quux")
      relativePath.isEmpty &&
        Try(relativePath.head).isFailure &&
        Try(relativePath.tail).isFailure
    }.assert(_ == true)

  }
}
