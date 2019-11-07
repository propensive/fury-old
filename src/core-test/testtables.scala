/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.7.1. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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

import java.nio.file.Files
import java.nio.file.Path

import fury.core._, fury.strings._, fury.model._

import probably._

object TablesTest extends TestApp {
  override def tests(): Unit = {
    test("contextString with showSchema=true") {
      val tables = Tables(Config())

      tables.contextString(msg"foo", true, msg"bar", msg"baz").string(Theme.NoColor)
    }.assert {
      _ == "foo/bar/baz"
    }

    test("contextString with showSchema=false") {
      val tables = Tables(Config())

      tables.contextString(msg"foo", false, msg"bar", msg"baz").string(Theme.NoColor)
    }.assert {
      _ == "foo/baz"
    }
  }
}
