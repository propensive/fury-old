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
package fury

import fury.text._

import contextual._
import jovian._

package object io {
  implicit val stringShowPath: StringShow[Path] = _.value
  implicit val diffPath: Diff[Path] = (l, r) => Diff.stringDiff.diff(l.value, r.value)
  implicit val parserPath: Parser[Path] = Path.unapply(_)
  implicit val msgShowGlob: MsgShow[Glob] = glob => Message(_.path(glob.pattern))
  implicit val stringShowGlob: StringShow[Glob] = _.pattern
  implicit val diffGlob: Diff[Glob] = (l, r) => Diff.stringDiff.diff(str"$l", str"$r")
  implicit val msgShowByteSize: MsgShow[ByteSize] = fs => Message(_.number(str"${Strings.magnitude(fs.bytes, "B")}"))
  implicit val stringShowByteSize: StringShow[ByteSize] = fs => str"${Strings.magnitude(fs.bytes, "B")}"
}