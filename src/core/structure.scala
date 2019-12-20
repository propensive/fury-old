/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.7.14. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
   ║                                                                                                           ║
   ║ The primary distribution site is: https://propensive.com/                                                 ║
   ║                                                                                                           ║
   ║ Licensed under  the Apache License,  Version 2.0 (the  "License"); you  may not use  this file  except in ║
   ║ compliance with the License. You may obtain a copy of the License at                                      ║
   ║                                                                                                           ║
   ║     http://www.apache.org/licenses/LICENSE-2.0                                                            ║
   ║                                                                                                           ║
   ║ Unless required  by applicable law  or agreed to in  writing, software  distributed under the  License is ║
   ║ distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. ║
   ║ See the License for the specific language governing permissions and limitations under the License.        ║
   ╚═══════════════════════════════════════════════════════════════════════════════════════════════════════════╝
*/
package fury.core

import fury.strings._, fury.model._

import scala.util._

sealed trait MenuStructure[T] {
  def command: Symbol
  def description: UserMsg
  def show: Boolean
  def shortcut: Char
}

case class Action[T](
    command: Symbol,
    description: UserMsg,
    action: T => Try[ExitStatus],
    show: Boolean = true,
    shortcut: Char = '\u0000')
    extends MenuStructure[T]

case class Menu[T, S](
    command: Symbol,
    description: UserMsg,
    action: T => Try[S],
    default: Symbol,
    show: Boolean = true,
    shortcut: Char = '\u0000'
  )(val items: MenuStructure[S]*)
    extends MenuStructure[T] {

  def apply(cli: Cli[CliParam[_]], ctx: T): Try[ExitStatus] =
    cli.args.prefix.headOption match {
      case None =>
        if(cli.completion) cli.completeCommand(this)
        else apply(cli.prefix(default.name), ctx)
      case Some(next) =>
        val cmd = items.find(_.command.name == next.value).orElse {
          if(next.value.length <= 2) items.find(_.shortcut == next.value(0))
          else None
        }

        cmd match {
          case None =>
            if(cli.completion) cli.completeCommand(this)
            else Failure(UnknownCommand(next.value))
          case Some(item @ Menu(_, _, _, _, _, _)) =>
            action(ctx).flatMap(item(cli.tail, _))
          case Some(item @ Action(_, _, _, _, _)) =>
            action(ctx).flatMap(item.action)
        }
    }

  def reference(implicit theme: Theme): Seq[String] = {

    def highlight(str: String, char: Char): UserMsg = if(char == '\u0000') msg"$str" else {
      val idx = str.indexOf(str)
      val left = str.substring(0, idx)
      val right = str.substring(idx + 1)

      UserMsg { theme => msg"$left${theme.underline(char.toString)}$right".string(theme) }
    }

    val shownItems = items.filter(_.show)
    val width      = 12
    shownItems.sortBy { case _: Action[_] => 0; case _ => 1 }.flatMap {
      case item: Action[_] =>
        List(msg"  ${highlight(item.command.name.padTo(width, ' '), item.shortcut)} ${item.description}".string(theme))
      case item: Menu[_, _] =>
        "" +: msg"  ${highlight(item.command.name.padTo(width, ' '), item.shortcut)} ${item.description}".string(theme) +:
          item.reference.map("  " + _)
    }
  }
}
