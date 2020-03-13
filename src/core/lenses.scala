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

  def project(projectId: ProjectId) = Lens[Layer](_.schemas(on(SchemaId.default)).projects(on(projectId)))

  def module(projectId: ProjectId, moduleId: ModuleId) =
    Lens[Layer](_.schemas(on(SchemaId.default)).projects(on(projectId)).modules(on(moduleId)))
  
  def updateSchemas[A]
                   (layer: Layer)
                   (lens: SchemaId => Lens[Layer, A, A])
                   (modify: (Lens[Layer, A, A], Layer) => Layer)
                   : Try[Layer] = Try(modify(lens(SchemaId.default), layer))

  def focus() = new Focus()

  class Focus() {

    def update[A](
        layer: Layer,
        partialLens: Lens.Partial[Schema] => Lens[Schema, A, A],
        value: Option[A]
      ): Layer = value match {
        case None => layer
        case Some(value) =>
          val lens = Optic.identity.compose(Lenses.layer.schema(SchemaId.default), partialLens(Lenses.schema))
          lens(layer) = value
      }
  }

  object module extends Lens.Partial[Module]() {
    val dependencies = lens(_.dependencies)
  }

  object project extends Lens.Partial[Project]() {
    def module(moduleId: ModuleId) = lens(_.modules(on(moduleId)))

    val modules    = lens(_.modules)
    val mainModule = lens(_.main)
  }

  object schema extends Lens.Partial[Schema]() {
    val mainProject = lens(_.main)
    val projects    = lens(_.projects)
  }

  object config extends Lens.Partial[Config]() {
    val theme = lens(_.theme)
  }

  object layer extends Lens.Partial[Layer]() {
    val schemas    = lens(_.schemas)
    val mainSchema = lens(_.main)

    def schema(schemaId: SchemaId) = lens(_.schemas(on(schemaId)))

    def imports(schemaId: SchemaId) =
      lens(_.schemas(on(schemaId)).imports)

    def importRemote(schemaId: SchemaId, importId: ImportId) =
      lens(_.schemas(on(schemaId)).imports(on(importId)).remote)

    def projects(schemaId: SchemaId) =
      Optic.identity.compose(schema(schemaId), Lenses.schema.projects)

    def project(schemaId: SchemaId, projectId: ProjectId) =
      lens(_.schemas(on(schemaId)).projects(on(projectId)))

    def mainProject(schemaId: SchemaId) =
      lens(_.schemas(on(schemaId)).main)

    def repos(schemaId: SchemaId) =
      lens(_.schemas(on(schemaId)).repos)

    def modules(schemaId: SchemaId, projectId: ProjectId) =
      lens(_.schemas(on(schemaId)).projects(on(projectId)).modules)

    def mainModule(schemaId: SchemaId, projectId: ProjectId) =
      lens(_.schemas(on(schemaId)).projects(on(projectId)).main)

    def compiler(schemaId: SchemaId, projectId: ProjectId) =
      lens(_.schemas(on(schemaId)).projects(on(projectId)).compiler)

    def module(schemaId: SchemaId, projectId: ProjectId, moduleId: ModuleId) =
      lens(_.schemas(on(schemaId)).projects(on(projectId)).modules(on(moduleId)))

    def moduleKind(schemaId: SchemaId, projectId: ProjectId, moduleId: ModuleId) =
      lens(_.schemas(on(schemaId)).projects(on(projectId)).modules(on(moduleId)).kind)

    def moduleBloopSpec(schemaId: SchemaId, projectId: ProjectId, moduleId: ModuleId) =
      lens(_.schemas(on(schemaId)).projects(on(projectId)).modules(on(moduleId)).bloopSpec)

    def moduleMainClass(schemaId: SchemaId, projectId: ProjectId, moduleId: ModuleId) =
      lens(_.schemas(on(schemaId)).projects(on(projectId)).modules(on(moduleId)).main)

    def modulePluginName(schemaId: SchemaId, projectId: ProjectId, moduleId: ModuleId) =
      lens(_.schemas(on(schemaId)).projects(on(projectId)).modules(on(moduleId)).plugin)

    def moduleCompiler(schemaId: SchemaId, projectId: ProjectId, moduleId: ModuleId) =
      lens(_.schemas(on(schemaId)).projects(on(projectId)).modules(on(moduleId)).compiler)

    def moduleId(schemaId: SchemaId, projectId: ProjectId, moduleId: ModuleId) =
      lens(_.schemas(on(schemaId)).projects(on(projectId)).modules(on(moduleId)).id)

    def dependencies(schemaId: SchemaId, projectId: ProjectId, moduleId: ModuleId) =
      lens(_.schemas(on(schemaId)).projects(on(projectId)).modules(on(moduleId)).dependencies)

    def sources(schemaId: SchemaId, projectId: ProjectId, moduleId: ModuleId) =
      lens(_.schemas(on(schemaId)).projects(on(projectId)).modules(on(moduleId)).sources)

    def resources(schemaId: SchemaId, projectId: ProjectId, moduleId: ModuleId) =
      lens(_.schemas(on(schemaId)).projects(on(projectId)).modules(on(moduleId)).resources)

    def environment(schemaId: SchemaId, projectId: ProjectId, moduleId: ModuleId)
                   : Lens[Layer, SortedSet[EnvVar], SortedSet[EnvVar]] =
      lens(_.schemas(on(schemaId)).projects(on(projectId)).modules(on(moduleId)).environment)

    def policy(schemaId: SchemaId, projectId: ProjectId, moduleId: ModuleId)
                   : Lens[Layer, SortedSet[Permission], SortedSet[Permission]] =
      lens(_.schemas(on(schemaId)).projects(on(projectId)).modules(on(moduleId)).policy)

    def properties(schemaId: SchemaId, projectId: ProjectId, moduleId: ModuleId)
                   : Lens[Layer, SortedSet[JavaProperty], SortedSet[JavaProperty]] =
      lens(_.schemas(on(schemaId)).projects(on(projectId)).modules(on(moduleId)).properties)

    def binaries(schemaId: SchemaId, projectId: ProjectId, moduleId: ModuleId) =
      lens(_.schemas(on(schemaId)).projects(on(projectId)).modules(on(moduleId)).binaries)

    def opts(schemaId: SchemaId, projectId: ProjectId, moduleId: ModuleId) =
      lens(_.schemas(on(schemaId)).projects(on(projectId)).modules(on(moduleId)).opts)

    def optDefs(schemaId: SchemaId, projectId: ProjectId, moduleId: ModuleId) =
      lens(_.schemas(on(schemaId)).projects(on(projectId)).modules(on(moduleId)).optDefs)

    def repoId(schemaId: SchemaId, repoId: RepoId) =
      lens(_.schemas(on(schemaId)).repos(on(repoId)).id)

    def repoDir(schemaId: SchemaId, repoId: RepoId) =
      lens(_.schemas(on(schemaId)).repos(on(repoId)).local)

    def repoRepo(schemaId: SchemaId, repoId: RepoId) =
      lens(_.schemas(on(schemaId)).repos(on(repoId)).repo)

    def repoTrack(schemaId: SchemaId, repoId: RepoId) =
      lens(_.schemas(on(schemaId)).repos(on(repoId)).track)

    val aliases = lens(_.aliases)

  }
}
