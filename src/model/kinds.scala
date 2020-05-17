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

package fury.model

import fury.strings._
import scala.reflect.ClassTag

object KindName {
  val all: List[KindName] = List(Lib, App, Plugin, Compiler, Bench)
  def unapply(string: String): Option[KindName] = all.find(_.name == string)
  implicit val parser: Parser[KindName] = unapply(_)
  implicit val stringShow: StringShow[KindName] = _.name
  implicit val msgShow: MsgShow[KindName] = v => UserMsg(_.param(stringShow.show(v)))
}

sealed abstract class KindName(val name: String)

object Kind {

  def parse(kindName: KindName,
            main: Option[ClassRef] = None,
            spec: Option[BloopSpec] = None,
            plugin: Option[PluginId] = None)
           : Option[Kind] =
    kindName match {
      case Lib      => Some(Lib())
      case App      => main.map(App(_))
      case Bench    => main.map(Bench(_))
      case Compiler => spec.map(Compiler(_))
      case Plugin   => for(m <- main; p <- plugin) yield Plugin(p, m)
      case _ => None
    }

  implicit def msgShow: MsgShow[Kind] = {
    case Lib()            => msg"lib"
    case App(main)        => msg"app${':'}$main"
    case Plugin(id, main) => msg"plugin${':'}$main"
    case Compiler(spec)   => msg"compiler${':'}$spec"
    case Bench(main)      => msg"bench${':'}$main"
  }

  implicit def stringShow: StringShow[Kind] = msgShow.show(_).string(Theme.NoColor)

  def needsExec(kind: Kind): Boolean = kind match {
    case App(_) | Bench(_) => true
    case _                 => false
  }
}
sealed trait Kind {
  def as[T: ClassTag]: Option[T] = this.only { case t: T => t }
  def is[T: ClassTag]: Boolean = this match { case t: T => true case _ => false }
  
  def name: KindName = this match {
    case Lib()        => Lib
    case App(_)       => App
    case Plugin(_, _) => Plugin
    case Compiler(_)  => Compiler
    case Bench(_)     => Bench
  }
}

object Lib extends KindName("lib")
case class Lib() extends Kind

object App extends KindName("app")
case class App(main: ClassRef) extends Kind

object Plugin extends KindName("plugin")
case class Plugin(id: PluginId, main: ClassRef) extends Kind

object Compiler extends KindName("compiler")
case class Compiler(spec: BloopSpec) extends Kind

object Bench extends KindName("bench")
case class Bench(main: ClassRef) extends Kind
