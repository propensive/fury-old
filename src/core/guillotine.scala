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

package fury.core

import fury.io._, fury.model._

import guillotine._
import contextual._

trait GuillotineExtensions {
  implicit def embedShellParam[T](implicit sp: ShellParam[T]) = ShellInterpolator.embed[T](
    Case(Awaiting, Unquoted)(sp.embed(_).args.map('"'+esc1(_)+'"').mkString(" ")),
    Case(Unquoted, Unquoted)(sp.embed(_).args.map('"'+esc1(_)+'"').mkString(" ")),
    Case(SingleQuoted, SingleQuoted)(sp.embed(_).args.map(esc2(_)).mkString(" ")),
    Case(DoubleQuoted, DoubleQuoted)(sp.embed(_).args.map(esc1(_)).mkString(" "))
  )
}

object ShellParam {
  implicit def option[T: ShellParam]: ShellParam[Option[T]] =
    _.fold(Command())(implicitly[ShellParam[T]].embed(_))

  implicit val branch: ShellParam[Branch] = branch => Command(branch.id)
  implicit val commit: ShellParam[Commit] = commit => Command(commit.id)
  implicit val tag: ShellParam[Tag] = tag => Command(tag.id)
  implicit val path: ShellParam[Path] = path => Command(path.value)
}

trait ShellParam[T] { def embed(value: T): Command }