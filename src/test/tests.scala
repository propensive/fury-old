/*

    Fury, version 0.18.18. Copyright 2018-20 Jon Pretty, Propensive OÜ.

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
import mercator._

import scala.concurrent._, duration._

object AllTests extends Suite() {
  def run(test: Runner): Unit = {
    implicit val ec: ExecutionContext = ExecutionContext.global
    Await.result(List(
      Future(test.suite("Model tests")(ModelTests.run)),
      Future(test.suite("I/O tests")(IoTests.run)),
      Future(test.suite("Core tests")(CoreTests.run)),
      Future(test.suite("OGDL tests")(OgdlTests.run)),
      Future(test.suite("Text tests")(TextTests.run))
    ).sequence, Duration.Inf)
  }
}