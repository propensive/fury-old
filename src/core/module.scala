/*

    Fury, version 0.18.14. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

import fury.model._, fury.text._, fury.io._

import scala.util._
import scala.collection.immutable._

object Module {
  implicit val msgShow: MsgShow[Module] = v => UserMsg(_.module(v.id.key))
  implicit val stringShow: StringShow[Module] = _.id.key
  implicit val diff: Diff[Module] = Diff.gen[Module]
  implicit val keyName: KeyName[Module] = () => msg"module"
}

case class Module(id: ModuleId,
                  kind: Kind = Lib(),
                  manifest: List[ManifestEntry] = List(),
                  compiler: CompilerRef = Javac(8),
                  dependencies: SortedSet[Dependency] = TreeSet(),
                  opts: SortedSet[Opt] = TreeSet(),
                  sources: SortedSet[Source] = TreeSet(),
                  binaries: SortedSet[Binary] = TreeSet(),
                  resources: SortedSet[Source] = TreeSet(),
                  includes: SortedSet[Include] = TreeSet(),
                  environment: SortedSet[EnvVar] = TreeSet(),
                  properties: SortedSet[JavaProperty] = TreeSet(),
                  policy: SortedSet[Permission] = TreeSet(),
                  hidden: Boolean = false,
                  optDefs: SortedSet[OptDef] = TreeSet(),
                  deterministic: Boolean = false) {

  def allBinaries: SortedSet[Binary] = if(kind.is[Bench]) binaries + Binary.Jmh else binaries
  
  def compilerDependencies: Set[Dependency] = compiler match  {
    case BspCompiler(ref) => Set(Dependency(ref.hide))
    case _                => Set()
  }
  
  def ref(project: Project): ModuleRef = ModuleRef(project.id, id, false, hidden = hidden)
  def externalSources: SortedSet[RepoSource] = sources.collect { case src: RepoSource => src }
  
  def externalResources: Stream[RepoSource] =
    resources.to[Stream].collect { case src: RepoSource => src }

  def policyEntries: Set[PermissionEntry] = {
    val prefixLength = Compare.uniquePrefixLength(policy.map(_.hash)).max(3)
    policy.map { p => PermissionEntry(p, PermissionHash(p.hash.take(prefixLength))) }
  }
}
