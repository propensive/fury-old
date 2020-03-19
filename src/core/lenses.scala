/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.8.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.                                         ║
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

import fury.model._

import optometry._
import scala.collection.immutable.SortedSet

import scala.util._

object on {
  type Id[T] = T

  def apply[A, AId](id: AId)(implicit resolver: Resolver[A, AId]): Optic[SortedSet, Id, A] =
    new Optic[SortedSet, Id, A]("focus") {
      def map[B](v: SortedSet[A])(fn: A => B): B     = fn(v.find(resolver.matchOn(id, _)).get)
      def comap(f: SortedSet[A], g: A): SortedSet[A] = f.filterNot(resolver.matchOn(id, _)) + g
    }
}

object Lenses {
  def set[T](newValue: T)(layer: Layer, lens: Lens[Layer, T, T]): Layer = lens(layer) = newValue
  def project(projectId: ProjectId) = Lens[Layer](_.projects(on(projectId)))
  
  def module(projectId: ProjectId, moduleId: ModuleId) =
    Lens[Layer](_.projects(on(projectId)).modules(on(moduleId)))
  
  object module extends Lens.Partial[Module]() {
    val dependencies = lens(_.dependencies)
  }

  object project extends Lens.Partial[Project]() {
    def module(moduleId: ModuleId) = lens(_.modules(on(moduleId)))

    val modules    = lens(_.modules)
    val mainModule = lens(_.main)
  }

  object config extends Lens.Partial[Config]() {
    val theme = lens(_.theme)
  }

  object layer extends Lens.Partial[Layer]() {
    def imports = lens(_.imports)
    def importRemote(importId: ImportId) = lens(_.imports(on(importId)).remote)
    def projects = lens(_.projects)
    def project(projectId: ProjectId) = lens(_.projects(on(projectId)))
    def mainProject = lens(_.main)
    def repos = lens(_.repos)
    def modules(projectId: ProjectId) = lens(_.projects(on(projectId)).modules)
    def mainModule(projectId: ProjectId) = lens(_.projects(on(projectId)).main)
    def compiler(projectId: ProjectId) = lens(_.projects(on(projectId)).compiler)
    def module(projectId: ProjectId, moduleId: ModuleId) = lens(_.projects(on(projectId)).modules(on(moduleId)))
    
    def moduleKind(projectId: ProjectId, moduleId: ModuleId) =
      lens(_.projects(on(projectId)).modules(on(moduleId)).kind)
    
    def moduleBloopSpec(projectId: ProjectId, moduleId: ModuleId) =
      lens(_.projects(on(projectId)).modules(on(moduleId)).bloopSpec)
    
    def moduleMainClass(projectId: ProjectId, moduleId: ModuleId) =
      lens(_.projects(on(projectId)).modules(on(moduleId)).main)
    
    def modulePluginName(projectId: ProjectId, moduleId: ModuleId) =
      lens(_.projects(on(projectId)).modules(on(moduleId)).plugin)

    def moduleCompiler(projectId: ProjectId, moduleId: ModuleId) =
      lens(_.projects(on(projectId)).modules(on(moduleId)).compiler)

    def moduleId(projectId: ProjectId, moduleId: ModuleId) =
      lens(_.projects(on(projectId)).modules(on(moduleId)).id)

    def dependencies(projectId: ProjectId, moduleId: ModuleId) =
      lens(_.projects(on(projectId)).modules(on(moduleId)).dependencies)

    def sources(projectId: ProjectId, moduleId: ModuleId) =
      lens(_.projects(on(projectId)).modules(on(moduleId)).sources)

    def resources(projectId: ProjectId, moduleId: ModuleId) =
      lens(_.projects(on(projectId)).modules(on(moduleId)).resources)

    def environment(projectId: ProjectId, moduleId: ModuleId)
                   : Lens[Layer, SortedSet[EnvVar], SortedSet[EnvVar]] =
      lens(_.projects(on(projectId)).modules(on(moduleId)).environment)

    def policy(projectId: ProjectId, moduleId: ModuleId)
                   : Lens[Layer, SortedSet[Permission], SortedSet[Permission]] =
      lens(_.projects(on(projectId)).modules(on(moduleId)).policy)

    def properties(projectId: ProjectId, moduleId: ModuleId)
                   : Lens[Layer, SortedSet[JavaProperty], SortedSet[JavaProperty]] =
      lens(_.projects(on(projectId)).modules(on(moduleId)).properties)

    def binaries(projectId: ProjectId, moduleId: ModuleId) =
      lens(_.projects(on(projectId)).modules(on(moduleId)).binaries)

    def opts(projectId: ProjectId, moduleId: ModuleId) =
      lens(_.projects(on(projectId)).modules(on(moduleId)).opts)

    def optDefs(projectId: ProjectId, moduleId: ModuleId) =
      lens(_.projects(on(projectId)).modules(on(moduleId)).optDefs)

    def repoId(repoId: RepoId) =
      lens(_.repos(on(repoId)).id)

    def repoDir(repoId: RepoId) =
      lens(_.repos(on(repoId)).local)

    def repoRepo(repoId: RepoId) =
      lens(_.repos(on(repoId)).repo)

    def repoTrack(repoId: RepoId) =
      lens(_.repos(on(repoId)).track)

    val aliases = lens(_.aliases)

  }
}
