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

import fury.strings._, fury.model._

import scala.util._

sealed trait MenuStructure {
  def command: Symbol
  def description: UserMsg
  def show: Boolean
  def shortcut: Char
  def needsLayer: Boolean
}

case class Action(
    command: Symbol,
    description: UserMsg,
    action: Cli => Try[ExitStatus],
    show: Boolean = true,
    shortcut: Char = '\u0000',
    needsLayer: Boolean = true)
    extends MenuStructure

case class Menu(
    command: Symbol,
    description: UserMsg,
    default: Symbol,
    show: Boolean = true,
    shortcut: Char = '\u0000',
    needsLayer: Boolean = true
  )(val items: MenuStructure*)
    extends MenuStructure {

  def apply(cli: Cli, ctx: Cli, reentrant: Boolean = false): Try[ExitStatus] = {
    val hasLayer: Boolean = cli.layout.map(_.confFile.exists).getOrElse(false)
    if(cli.args.args == Seq("interrupt")) {
      Success(Done)
    } else {
      if(FuryVersion.current != Installation.installVersion && !reentrant) {
        cli.forceLog(msg"The running Fury server version (${FuryVersion.current}) differs from the shell"+
            msg"script version (${Installation.installVersion}) calling it.")
        cli.forceLog(msg"This is probably because a newer version of Fury has been installed while the old "+
            msg"version is still running.")
      }

      cli.args.prefix.headOption match {
        case None =>
          if (cli.completion) cli.completeCommand(this, hasLayer)
          else {
            apply(cli.prefix(default.name), ctx)
          }

        case Some(next) =>
          val cmd = items.find(_.command.name == next.value).orElse {
            if (next.value.length <= 2 && !cli.command.exists(_ < 4)) items.find(_.shortcut == next.value(0))
            else None
          }

          cmd match {
            case None =>
              if (cli.completion) cli.completeCommand(this, hasLayer)
              else Failure(UnknownCommand(next.value))
            case Some(item@Menu(_, _, _, _, _, _)) =>
              item(cli.tail, cli, true)
            case Some(item@Action(_, _, _, _, _, _)) =>
              item.action(cli)
          }
      }
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
    shownItems.sortBy { case _: Action => 0; case _ => 1 }.flatMap {
      case item: Action =>
        List(msg"  ${highlight(item.command.name.padTo(width, ' '), item.shortcut)} ${item.description}".string(
            theme))
      case item: Menu =>
        "" +: msg"  ${highlight(item.command.name.padTo(width, ' '),
            item.shortcut)} ${item.description}".string(theme) +: item.reference.map("  " + _)
    }
  }
}
