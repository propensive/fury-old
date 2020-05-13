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

import fury.model._, fury.io._, fury.strings._

object Target {
  case class Graph(dependencies: Map[TargetId, Set[TargetId]], targets: Map[TargetId, Target]) {
    def links: Map[UiGraph.Key, Set[UiGraph.Key]] = dependencies.map { case (k, ds) =>
      (UiGraph.Key(k.ref), ds.map { d => UiGraph.Key(d.ref, Kind.name(targets(d).kind) == Compiler) })
    }.toMap
  }
}

case class Target(ref: ModuleRef,
                  kind: Kind,
                  repos: List[Remote],
                  checkouts: List[Checkout],
                  binaries: List[Path],
                  dependencies: List[TargetId],
                  compiler: Option[Target],
                  params: List[Opt],
                  permissions: List[Permission],
                  intransitive: Boolean,
                  sourcePaths: List[Path],
                  environment: Map[String, String],
                  properties: Map[String, String],
                  optDefs: Set[OptDef],
                  resources: List[Source]) {

  def id: TargetId = TargetId(ref.projectId, ref.moduleId)
  
  def plugin: Option[PluginDef] = kind.only { case Plugin(id, main) => PluginDef(id, ref, main) }
  def compilerDef: Option[Compiler] = kind.only { case c@Compiler(spec) => c }
  def app: Option[App] = kind.only { case a@App(_) => a }

  def impliedCompiler: ModuleRef =
    if(Kind.name(kind) == Compiler) ref else compiler.fold(ModuleRef.JavaRef)(_.ref)
}