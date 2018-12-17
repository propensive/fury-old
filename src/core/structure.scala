/*
  Fury, version 0.1.2. Copyright 2018 Jon Pretty, Propensive Ltd.

  The primary distribution site is: https://propensive.com/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required  by applicable  law or  agreed to  in writing,  software  distributed  under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
  express  or  implied.  See  the  License for  the specific  language  governing  permissions and
  limitations under the License.
                                                                                                  */
package fury

import mitigation._
import guillotine._
import exoskeleton._

sealed trait MenuStructure[T] {
  def command: Symbol
  def description: UserMsg
  def show: Boolean
}

case class Action[T](command: Symbol, description: UserMsg, action: T => Result[ExitStatus, ~ |
                    FileWriteError |
                    FileNotFound | MissingArg | UnknownCommand | UnknownCompiler | ItemNotFound |
                    UnspecifiedProject | UnspecifiedModule | InvalidArgValue | ConfigFormatError |
                    ShellFailure | ModuleAlreadyExists | ProjectAlreadyExists | AlreadyInitialized |
                    InvalidValue | InitFailure | SchemaDifferences | EarlyCompletions | ProjectConflict], show: Boolean = true) extends MenuStructure[T]

case class Menu[T, S](command: Symbol, description: UserMsg, action: T => Result[S, ~ |
                         FileWriteError | FileNotFound | MissingArg |
                         UnknownCommand | UnknownCompiler | ItemNotFound | UnspecifiedProject |
                         UnspecifiedModule | InvalidArgValue | ConfigFormatError | ShellFailure |
                         ModuleAlreadyExists | ProjectAlreadyExists | AlreadyInitialized |
                         InvalidValue | InitFailure | SchemaDifferences | EarlyCompletions | ProjectConflict], default: Symbol, show: Boolean = true)
                     (val items: MenuStructure[S]*)
                     extends MenuStructure[T] {

  def apply(cli: Cli[CliParam[_]], ctx: T)
           : Result[ExitStatus, ~ | UnknownCommand | FileWriteError | MissingArg |
               FileNotFound | UnknownCompiler | ItemNotFound | UnspecifiedProject |
               UnspecifiedModule | InvalidArgValue | ConfigFormatError | ShellFailure |
               ModuleAlreadyExists | ProjectAlreadyExists | AlreadyInitialized | InvalidValue |
               InitFailure | SchemaDifferences | EarlyCompletions | ProjectConflict] =
    cli.args.prefix.headOption match {
      case None =>
        if(cli.completion) cli.completeCommand(this)
        else apply(cli.prefix(default.name), ctx)
      case Some(next) => items.find(_.command.name == next.value) match {
        case None =>
          if(cli.completion) cli.completeCommand(this) else Result.abort(UnknownCommand(next.value))
        case Some(item@Menu(_, _, _, _, _)) =>
          action(ctx).flatMap(item(cli.tail, _))
        case Some(item@Action(_, _, _, _)) =>
          action(ctx).flatMap(item.action)
      }
    }

  def reference(implicit theme: Theme): Seq[String] = {
    val shownItems = items.filter(_.show)
    val width = 12
    shownItems.sortBy { case _: Action[_] => 0; case _ => 1 }.flatMap {
      case item: Action[_] =>
        List(msg"  ${item.command.name.padTo(width, ' ')} ${item.description}".string(theme))
      case item: Menu[_, _] =>
        "" +: msg"  ${item.command.name.padTo(width, ' ')} ${item.description}".string(theme) +:
            item.reference.map("  "+_)
    }
  }
}

