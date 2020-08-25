/*

    Fury, version 0.18.7. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.model

import fury.text._
import scala.reflect.ClassTag

object Kind {
  object Id {
    def unapply(string: String): Option[Id] = ids.find(_.name == string)
    implicit val parser: Parser[Id] = unapply(_)
    implicit val stringShow: StringShow[Id] = _.name
    implicit val msgShow: MsgShow[Id] = v => UserMsg(_.param(stringShow.show(v)))
  }

  sealed abstract class Id(val name: String)
  
  val ids: List[Id] = List(Lib, App, Plugin, Compiler, Bench)
  implicit def stringShow: StringShow[Kind] = msgShow.show(_).string(Theme.NoColor)

  implicit def msgShow: MsgShow[Kind] = {
    case Lib()                => msg"lib"
    case App(main, timeout)   => msg"app${':'}$main"
    case Plugin(id, main)     => msg"plugin${':'}$id${'@'}$main"
    case Compiler(spec, repl) => msg"compiler${':'}$spec"
    case Bench(main)          => msg"bench${':'}$main"
  }
}

sealed abstract class Kind(val needsExec: Boolean = false) {
  def as[T: ClassTag]: Option[T] = this.only { case t: T => t }
  def is[T: ClassTag]: Boolean = this match { case t: T => true case _ => false }
  
  def name: Kind.Id = this match {
    case Lib()          => Lib
    case App(_, _)      => App
    case Plugin(_, _)   => Plugin
    case Compiler(_, _) => Compiler
    case Bench(_)       => Bench
  }
}

object Lib extends Kind.Id("lib")
case class Lib() extends Kind()

object App extends Kind.Id("app")
case class App(main: ClassRef, timeout: Int) extends Kind(needsExec = true)

object Plugin extends Kind.Id("plugin")
case class Plugin(id: PluginId, main: ClassRef) extends Kind()

object Compiler extends Kind.Id("compiler")
case class Compiler(spec: BloopSpec, repl: ClassRef = ClassRef("scala.tools.nsc.MainGenericRunner")) extends
    Kind()

object Bench extends Kind.Id("bench")
case class Bench(main: ClassRef) extends Kind(needsExec = true)
