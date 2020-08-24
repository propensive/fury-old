/*

    Fury, version 0.18.2. Copyright 2018-20 Jon Pretty, Propensive OÜ.

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
import fury.core.Uniqueness._

object UniquenessTest extends Suite() {

  private[this] case class Ref(id: String)
  private[this] case class Origin(id: String)

  def run(test: Runner): Unit = {
    test("Unique + Unique = Unique") {
      uniq("foo", "bar") + uniq("foo", "baz")
    }.assert(_ == uniq("foo", "bar", "baz"))

    test("Unique + Unique = Ambiguous") {
      uniq("foo", "bar") + uniq("quux", "baz")
    }.assert(_ == amb("bar" -> "foo", "baz" -> "quux"))

    test("Ambiguous + Unique = Ambiguous") {
      uniq("foo", "bar") + uniq("quux", "baz") + uniq("foo", "zzz")
    }.assert(_ == amb("bar" -> "foo", "baz" -> "quux", "zzz" -> "foo"))

  }

  private[this] def uniq(ref: String, origins: String*): Uniqueness[Ref, Origin] = Unique(Ref(ref), origins.map(Origin(_)).to[Set])
  private[this] def amb(origins: (String, String)*): Uniqueness[Ref, Origin] = Ambiguous(origins.toMap.map { case (o, r) => (Origin(o), Ref(r)) })

}
