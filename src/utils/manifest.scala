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
package fury.utils

import fury.strings._

object JarManifest {
  case class Entry(key: String, value: String) {
    def content: String = str"$key: $value".drop(1).grouped(70).to[List].join(key.take(1), "\n ", "")
  }

  def apply(classpath: Set[String], mainClass: Option[String]): JarManifest = JarManifest(List(
    List(Entry("Manifest-Version", "1.0")),
    List(
      Entry("Class-Path", classpath.to[List].sorted.join(" ")),
      Entry("Created-By", str"Fury ${FuryVersion.current}")
    ),
    mainClass.to[List].map(Entry("Main-Class", _))
  ).flatten: _*)
}

case class JarManifest(entries: JarManifest.Entry*) {
  def content: String = entries.map(_.content).join("", "\n", "\n")
}