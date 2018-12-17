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

case class FileNotFound(path: Path) extends Exception
case class FileWriteError(path: Path) extends Exception
case class ConfigFormatError() extends Exception
case class NotInitialized(dir: Path) extends Exception
case class ModuleAlreadyExists(module: ModuleId) extends Exception
case class ProjectAlreadyExists(project: ProjectId) extends Exception
case class MissingCommand() extends Exception
case class UnknownCommand(command: String) extends Exception
case class AlreadyInitialized() extends Exception
case class UnknownCompiler() extends Exception
case class InvalidValue(value: String) extends Exception
case class InitFailure() extends Exception
case class UnspecifiedProject() extends Exception
case class UnspecifiedModule() extends Exception
case class UnspecifiedRepo() extends Exception
case class ProjectConflict(ids: Set[ProjectId]) extends Exception
case class SchemaDifferences() extends Exception

object ItemNotFound {
  def apply[K <: Key: MsgShow](key: K): ItemNotFound =
    ItemNotFound(implicitly[MsgShow[K]].show(key), key.kind)
}

case class ItemNotFound(item: UserMsg, kind: UserMsg) extends Exception

