/*
  Fury, version 0.4.0. Copyright 2018-19 Jon Pretty, Propensive Ltd.

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

case class NotInitialized(dir: Path)                    extends FuryException
case class ModuleAlreadyExists(module: ModuleId)        extends FuryException
case class ProjectAlreadyExists(project: ProjectId)     extends FuryException
case class MissingCommand()                             extends FuryException
case class UnknownCommand(command: String)              extends FuryException
case class AlreadyInitialized()                         extends FuryException
case class UnknownCompiler()                            extends FuryException
case class InvalidValue(value: String)                  extends FuryException
case class InitFailure()                                extends FuryException
case class UnspecifiedProject()                         extends FuryException
case class UnspecifiedModule()                          extends FuryException
case class UnspecifiedMain(module: ModuleId)            extends FuryException
case class GraalVMError(message: String)                extends FuryException
case class InvalidKind(expected: Kind)                  extends FuryException
case class UnspecifiedRepo()                            extends FuryException
case class ProjectConflict(ids: Set[ProjectId])         extends FuryException
case class SchemaDifferences()                          extends FuryException
case class CyclesInDependencies(cycle: List[ModuleRef]) extends FuryException

object ItemNotFound {

  def apply[K <: Key: MsgShow](key: K): ItemNotFound =
    ItemNotFound(implicitly[MsgShow[K]].show(key), key.kind)
}

case class ItemNotFound(item: UserMsg, kind: UserMsg) extends FuryException
