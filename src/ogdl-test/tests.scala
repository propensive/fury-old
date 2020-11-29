/*

    Fury, version 0.31.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.test

import fury.ogdl._
import probably._

object OgdlTests extends Suite() {
  def run(test: Runner): Unit = {
    test.suite("OGDL Parser tests")(OgdlParserTest.run(_))
    test.suite("OGDL Serializer tests")(OgdlSerializerTest.run(_))
    test.suite("OGDL Writer tests")(OgdlWriterTest.run(_))
    test.suite("OGDL Reader tests")(OgdlReaderTest.run(_))
  }
}
