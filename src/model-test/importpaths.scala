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
    val absolutePath = ImportPath("/foo")
    val nestedAbsolutePath = ImportPath("/foo/bar")
    val relativePath = ImportPath("bar")
    val nestedRelativePath = ImportPath("baz/quux")

    test("root path head & tail") {
      root.isEmpty &&
      Try(root.head).isFailure &&
      Try(root.tail).isFailure
    }.assert(_ == true)

    test("current path head & tail") {
      here.isEmpty &&
        Try(here.head).isFailure &&
        Try(here.tail).isFailure
    }.assert(_ == true)

    test("parent path head & tail") {
      parent.isEmpty &&
        Try(parent.head).isFailure &&
        Try(parent.tail).isFailure
    }.assert(_ == true)

    test("absolute path head & tail") {
      !absolutePath.isEmpty &&
        Try(absolutePath.head) == Success(ImportId("foo")) &&
        Try(absolutePath.tail) == Success(ImportPath("/"))
    }.assert(_ == true)

    test("relative path head & tail") {
      relativePath.isEmpty &&
        Try(relativePath.head).isFailure &&
        Try(relativePath.tail).isFailure
    }.assert(_ == true)

    test("nested absolute path head & tail") {
      !nestedAbsolutePath.isEmpty &&
        Try(nestedAbsolutePath.head) == Success(ImportId("foo")) &&
        Try(nestedAbsolutePath.tail) == Success(ImportPath("/bar"))
    }.assert(_ == true)

    test("nested relative path head & tail") {
      nestedRelativePath.isEmpty &&
        Try(nestedRelativePath.head).isFailure &&
        Try(nestedRelativePath.tail).isFailure
    }.assert(_ == true)

    test("root path as prefix") {
      root.prefix(ImportId("xxx")) == ImportPath("/xxx") &&
      root.prefix(ImportId("/xxx")) == ImportPath("//xxx") &&
      root.prefix(ImportId("/")) == ImportPath("//") &&
      root.prefix(ImportId("")) == ImportPath("/")
    }.assert(_ == true)

    test("current path as prefix") {
      here.prefix(ImportId("xxx")) == ImportPath("/xxx") &&
        here.prefix(ImportId("/xxx")) == ImportPath("//xxx") &&
        here.prefix(ImportId("/")) == ImportPath("//") &&
        here.prefix(ImportId("")) == ImportPath("/")
    }.assert(_ == true)

    test("parent path as prefix") {
      parent.prefix(ImportId("xxx")) == ImportPath("/xxx") &&
        parent.prefix(ImportId("/xxx")) == ImportPath("//xxx") &&
        parent.prefix(ImportId("/")) == ImportPath("//") &&
        parent.prefix(ImportId("")) == ImportPath("/")
    }.assert(_ == true)

    test("absolute path as prefix") {
      absolutePath.prefix(ImportId("xxx")) == ImportPath("/xxx/foo") &&
        absolutePath.prefix(ImportId("/xxx")) == ImportPath("//xxx/foo") &&
        absolutePath.prefix(ImportId("/")) == ImportPath("///foo") &&
        absolutePath.prefix(ImportId("")) == ImportPath("//foo")
    }.assert(_ == true)

    test("relative path as prefix") {
      relativePath.prefix(ImportId("xxx")) == ImportPath("/xxx") &&
        relativePath.prefix(ImportId("/xxx")) == ImportPath("//xxx") &&
        relativePath.prefix(ImportId("/")) == ImportPath("//") &&
        relativePath.prefix(ImportId("")) == ImportPath("/")
    }.assert(_ == true)

  }
}
