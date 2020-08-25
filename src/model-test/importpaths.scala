/*

    Fury, version 0.18.6. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.test

import fury.model._
import probably._

import scala.util.{Success, Try}

object PointerTests extends Suite() {

  def run(test: Runner): Unit = {
    val root = Pointer("/")
    val here = Pointer(".")
    val parent = Pointer("..")
    val absolutePath = Pointer("/foo")
    val nestedAbsolutePath = Pointer("/foo/bar")
    val relativePath = Pointer("bar")
    val nestedRelativePath = Pointer("baz/quux")

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
        Try(absolutePath.tail) == Success(Pointer("/"))
    }.assert(_ == true)

    test("relative path head & tail") {
      relativePath.isEmpty &&
        Try(relativePath.head).isFailure &&
        Try(relativePath.tail).isFailure
    }.assert(_ == true)

    test("nested absolute path head & tail") {
      !nestedAbsolutePath.isEmpty &&
        Try(nestedAbsolutePath.head) == Success(ImportId("foo")) &&
        Try(nestedAbsolutePath.tail) == Success(Pointer("/bar"))
    }.assert(_ == true)

    test("nested relative path head & tail") {
      nestedRelativePath.isEmpty &&
        Try(nestedRelativePath.head).isFailure &&
        Try(nestedRelativePath.tail).isFailure
    }.assert(_ == true)

    test("root path as prefix") {
      root.prefix(ImportId("xxx")) == Pointer("/xxx") &&
      root.prefix(ImportId("/xxx")) == Pointer("//xxx") &&
      root.prefix(ImportId("/")) == Pointer("//") &&
      root.prefix(ImportId("")) == Pointer("/")
    }.assert(_ == true)

    test("current path as prefix") {
      here.prefix(ImportId("xxx")) == Pointer("/xxx") &&
        here.prefix(ImportId("/xxx")) == Pointer("//xxx") &&
        here.prefix(ImportId("/")) == Pointer("//") &&
        here.prefix(ImportId("")) == Pointer("/")
    }.assert(_ == true)

    test("parent path as prefix") {
      parent.prefix(ImportId("xxx")) == Pointer("/xxx") &&
        parent.prefix(ImportId("/xxx")) == Pointer("//xxx") &&
        parent.prefix(ImportId("/")) == Pointer("//") &&
        parent.prefix(ImportId("")) == Pointer("/")
    }.assert(_ == true)

    test("absolute path as prefix") {
      absolutePath.prefix(ImportId("xxx")) == Pointer("/xxx/foo") &&
        absolutePath.prefix(ImportId("/xxx")) == Pointer("//xxx/foo") &&
        absolutePath.prefix(ImportId("/")) == Pointer("///foo") &&
        absolutePath.prefix(ImportId("")) == Pointer("//foo")
    }.assert(_ == true)

    test("relative path as prefix") {
      relativePath.prefix(ImportId("xxx")) == Pointer("/xxx") &&
        relativePath.prefix(ImportId("/xxx")) == Pointer("//xxx") &&
        relativePath.prefix(ImportId("/")) == Pointer("//") &&
        relativePath.prefix(ImportId("")) == Pointer("/")
    }.assert(_ == true)

    test("root path as dereference base") {
      root.dereference(Pointer("xxx")) == Success(Pointer("/xxx")) &&
        root.dereference(Pointer("/xxx")) == Success(Pointer("/xxx")) &&
        root.dereference(Pointer("/")) == Success(Pointer("/")) &&
        root.dereference(Pointer("")) == Success(Pointer("/"))
    }.assert(_ == true)

    test("current path as dereference base") {
      here.dereference(Pointer("xxx")) == Success(Pointer("xxx")) &&
        here.dereference(Pointer("/xxx")) == Success(Pointer("/xxx")) &&
        here.dereference(Pointer("/")) == Success(Pointer("/")) &&
        here.dereference(Pointer("")) == Success(Pointer(""))
    }.assert(_ == true)

    test("parent path as dereference base") {
      parent.dereference(Pointer("xxx")).isFailure &&
        parent.dereference(Pointer("/xxx")) == Success(Pointer("/xxx")) &&
        parent.dereference(Pointer("/")) == Success(Pointer("/")) &&
        parent.dereference(Pointer("")).isFailure
    }.assert(_ == true)

    test("absolute path as dereference base") {
      absolutePath.dereference(Pointer("xxx"))== Success(Pointer("/foo/xxx")) &&
        absolutePath.dereference(Pointer("/xxx")) == Success(Pointer("/xxx")) &&
        absolutePath.dereference(Pointer("/")) == Success(Pointer("/")) &&
        absolutePath.dereference(Pointer(""))== Success(Pointer("/foo"))
    }.assert(_ == true)

  }
}
