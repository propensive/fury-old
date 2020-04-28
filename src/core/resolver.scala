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
import scala.collection.immutable.SortedSet

object Resolver {
  implicit val module: Resolver[Module, ModuleId] = _ == _.id
  implicit val project: Resolver[Project, ProjectId] = _ == _.id
  implicit val repo: Resolver[Repo, RepoId] = _ == _.id
  implicit val imports: Resolver[Import, ImportId] = _ == _.id
  implicit val binary: Resolver[Binary, BinaryId] = _ == _.id
  implicit val optDef: Resolver[OptDef, OptId] = _ == _.id
  implicit val source: Resolver[Source, Source] = _ == _
}

trait Resolver[-T, I] { def matchOn(id: I, value: T): Boolean }

class ResolverExt[T](items: Traversable[T]) {

  def findBy[I <: Key: MsgShow](id: I)(implicit resolver: Resolver[T, I]): Try[T] =
    items.find(resolver.matchOn(id, _)).ascribe(ItemNotFound(id))

  def unique[I <: Key: MsgShow](id: I)(implicit resolver: Resolver[T, I]): Try[I] =
    if(contains(id)) Failure(NotUnique(id)) else Success(id)
  
  def contains[I <: Key: MsgShow](id: I)(implicit resolver: Resolver[T, I]): Boolean =
    items.exists(resolver.matchOn(id, _))
}

class SortedSetExt[T](set: SortedSet[T]) {
  def evict[I <: Key: MsgShow](id: I)(implicit resolver: Resolver[T, I]): SortedSet[T] =
    set.filterNot(resolver.matchOn(id, _))
}
