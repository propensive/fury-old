/*

    Fury, version 0.33.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.lsp

import java.nio.file.Paths
import java.io.PrintWriter
import java.nio.file.Files
import java.nio.file.StandardOpenOption

object MessageTracer {

  private val home = Paths.get(".").toAbsolutePath
  private val tracePath = home.resolve(".fury").resolve("fury.lsp.log")

  def apply(): PrintWriter = {
    Files.createDirectories(home)
    new PrintWriter(
      Files.newOutputStream(
        tracePath,
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING
      )
    )
  }
}