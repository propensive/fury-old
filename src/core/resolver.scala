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

object Resolver {
  implicit val moduleResolver: Resolver[Module, ModuleId] = _ == _.id
  implicit val projectResolver: Resolver[Project, ProjectId] = _ == _.id
  implicit val schemaResolver: Resolver[Schema, SchemaId] = _ == _.id
  implicit val sourceRepoResolver: Resolver[SourceRepo, RepoId] = _ == _.id
}

trait Resolver[-T, I] { def matchOn(id: I, value: T): Boolean }

class ResolverExt[T](items: Traversable[T]) {
  def findBy[I <: Key: MsgShow](id: I)(implicit resolver: Resolver[T, I]): Result[T, ~ | ItemNotFound] =
    items.find(resolver.matchOn(id, _)).ascribe(ItemNotFound(id))
}
