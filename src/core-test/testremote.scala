/*

    Fury, version 0.18.14. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.test

import probably._

import fury.core._

object RemoteNameTest extends Suite() {

  def run(test: Runner): Unit = {
    test("Simplified form of HTTPS remote") {
      Remote("https://github.com/propensive/fury.git").simplified
    }.assert(_ == "gh:propensive/fury")

    test("Simplified form of SSH remote") {
      Remote("git@github.com:propensive/fury.git").simplified
    }.assert(_ == "gh:propensive/fury")

    test("SSH address for a HTTPS remote") {
      Remote("https://gitlab.com/propensive/fury.git").ssh
    }.assert(_ == Some("git@gitlab.com:propensive/fury.git"))

    test("HTTPS address for a SSH remote") {
      Remote("git@bitbucket.com:propensive/fury.git").https
    }.assert(_ == Some("https://bitbucket.com/propensive/fury.git"))
  }

}
