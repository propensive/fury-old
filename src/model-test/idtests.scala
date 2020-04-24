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
package fury

import fury.model._, fury.strings._
import probably._

import scala.language.implicitConversions
import scala.util.{Success, Try}

object IdTests extends TestApp {

  override def tests(): Unit = {

    test("binary ids") {
      "foo.bar:baz-quux_2.12:1.2.3-RC4".as[BinaryId].isFailure &&
      "foo.bar:baz-quux_2.12".as[BinaryId].isFailure &&
      //"baz-quux_2.12".as[BinaryId].isFailure &&
      "baz-quux".as[BinaryId].isSuccess
    }.assert(_ == true)

    test("binary specs") {
      "foo.bar:baz-quux_2.12:1.2.3-RC4".as[BinSpec].isSuccess &&
      "foo.bar:baz-quux_2.12".as[BinSpec].isFailure &&
      "baz-quux_2.12".as[BinSpec].isFailure
    }.assert(_ == true)

    test("repo ids") {
      "foo".as[RepoId].isSuccess &&
      "foo-bar-baz".as[RepoId].isSuccess &&
      "foo-bar_baz".as[RepoId].isFailure
    }.assert(_ == true)

    test("build target URIs") {
      "file:///foo/bar/baz/fury_model?id=fury_model".as[TargetId]
    }.assert(_ == Success(TargetId("fury_model")))

  }
}
