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

import fury.io._
import probably._

import scala.language.implicitConversions

object PathTests extends TestApp {

  override def tests(): Unit = {

    test("fail when trying to write to a directory") {
      tmpDir{ dir =>
        dir.writeSync("Writing to a directory...")
      }
    }.assert(_.isFailure)

    test("mark a file as executable") {
      tmpDir{ dir =>
        val file = (dir / "foo")
        file.touch()
        file.setExecutable(true)
      }
    }.assert(_.isSuccess)

    test("fail to mark a system file as executable") {
      val file = Path("/etc") / "passwd"
      file.touch()
      file.setExecutable(true)
    }.assert(_.isFailure)

  }

  def tmpDir[T](fn: Path => T): T = {
    val file = java.io.File.createTempFile("fury-test", "dir")
    file.delete()
    file.mkdir()
    val path = Path(file)
    val result = fn(path)
    path.delete()
    result
  }

}
