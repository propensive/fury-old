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

import fury.model._, fury.strings._, fury.io._

import scala.util._
import scala.collection.immutable._

object Module {
  implicit val msgShow: MsgShow[Module] = v => UserMsg(_.module(v.id.key))
  implicit val stringShow: StringShow[Module] = _.id.key
  implicit val diff: Diff[Module] = Diff.gen[Module]
  implicit val keyName: KeyName[Module] = () => msg"module"

  def available(id: ModuleId, project: Project): Try[ModuleId] =
    project.modules.find(_.id == id).fold(Try(id)) { module => Failure(ModuleAlreadyExists(module.id)) }
}

case class Module(id: ModuleId,
                  kind: Kind = Library,
                  main: Option[ClassRef] = None,
                  plugin: Option[PluginId] = None,
                  manifest: List[ManifestEntry] = List(),
                  compiler: ModuleRef = ModuleRef.JavaRef,
                  dependencies: SortedSet[ModuleRef] = TreeSet(),
                  opts: SortedSet[Opt] = TreeSet(),
                  sources: SortedSet[Source] = TreeSet(),
                  binaries: SortedSet[Binary] = TreeSet(),
                  resources: SortedSet[Source] = TreeSet(),
                  bloopSpec: Option[BloopSpec] = None,
                  environment: SortedSet[EnvVar] = TreeSet(),
                  properties: SortedSet[JavaProperty] = TreeSet(),
                  policy: SortedSet[Permission] = TreeSet(),
                  hidden: Boolean = false,
                  optDefs: SortedSet[OptDef] = TreeSet(),
                  deterministic: Boolean = false) {

  def allBinaries: SortedSet[Binary] = if(kind == Benchmarks) binaries + Binary.Jmh else binaries
  def compilerDependencies: Set[ModuleRef] = Set(compiler).filter(_ != ModuleRef.JavaRef).map(_.hide)
  def ref(project: Project): ModuleRef = ModuleRef(project.id, id, false, hidden = hidden)
  def externalSources: SortedSet[ExternalSource] = sources.collect { case src: ExternalSource => src }
  def sharedSources: SortedSet[SharedSource] = sources.collect { case src: SharedSource => src }
  def localSources: SortedSet[Path] = sources.collect { case src: LocalSource => src.dir }
 
  def localResources: Stream[Path] =
    resources.to[Stream].collect { case src: LocalSource => src.glob(src.dir, src.dir.walkTree) }.flatten

  def sharedResources: Stream[SharedSource] = resources.to[Stream].collect { case src: SharedSource => src }
  
  def externalResources: Stream[ExternalSource] =
    resources.to[Stream].collect { case src: ExternalSource => src }

  def policyEntries: Set[PermissionEntry] = {
    val prefixLength = Compare.uniquePrefixLength(policy.map(_.hash)).max(3)
    policy.map { p => PermissionEntry(p, PermissionHash(p.hash.take(prefixLength))) }
  }
}
