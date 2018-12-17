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

import optometry._
import mitigation._
import scala.collection.immutable.SortedSet

object Lenses {

  def updateSchemas[A](schemaId: Option[SchemaId], layer: Layer, force: Boolean)
             (lens: SchemaId => Lens[Layer, A, A])
             (modify: (Lens[Layer, A, A], Layer) => Layer)
             : Result[Layer, ~ | SchemaDifferences] = {
    val lenses = schemaId match {
      case Some(schemaId) => List(lens(schemaId))
      case None => layer.schemas.map(_.id).to[List].map(lens(_))
    }

    for(lenses <- if(force || lenses.map(_(layer)).to[Set].size == 1) Answer(lenses) else Result.abort(SchemaDifferences()))
    yield lenses.foldLeft(layer) { case (layer, lens) => modify(lens, layer) }
  }

  object on {
    type Id[T] = T
    def apply[A, AId](id: AId)(implicit resolver: Resolver[A, AId]): Optic[SortedSet, Id, A] =
      new Optic[SortedSet, Id, A]("focus") {
        def map[B](v: SortedSet[A])(fn: A => B): B = fn(v.find(resolver.matchOn(id, _)).get)
        def comap(f: SortedSet[A], g: A): SortedSet[A] = f.filterNot(resolver.matchOn(id, _)) + g
      }
  }

  object module extends Lens.Partial[Module]() {
    val dependencies = lens(_.after)
  }

  object project extends Lens.Partial[Project]() {
    def module(moduleId: ModuleId) = lens(_.modules(on(moduleId)))

    val modules = lens(_.modules)
    val mainModule = lens(_.main)
  }

  object schema extends Lens.Partial[Schema]() {
    val mainProject = lens(_.main)
    val projects = lens(_.projects)
  }

  object config extends Lens.Partial[Config]() {
    val theme = lens(_.theme)
  }

  object layer extends Lens.Partial[Layer]() {
    val schemas = lens(_.schemas)
    val mainSchema = lens(_.main)

    def schema(schemaId: SchemaId) = lens(_.schemas(on(schemaId)))

    def imports(schemaId: SchemaId) =
      lens(_.schemas(on(schemaId)).imports)

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

    def module(schemaId: SchemaId, projectId: ProjectId, moduleId: ModuleId) =
      lens(_.schemas(on(schemaId)).projects(on(projectId)).modules(on(moduleId)))
  
    def moduleKind(schemaId: SchemaId, projectId: ProjectId, moduleId: ModuleId) =
      lens(_.schemas(on(schemaId)).projects(on(projectId)).modules(on(moduleId)).kind)

    def moduleBloopSpec(schemaId: SchemaId, projectId: ProjectId, moduleId: ModuleId) =
      lens(_.schemas(on(schemaId)).projects(on(projectId)).modules(on(moduleId)).bloopSpec)

    def moduleMainClass(schemaId: SchemaId, projectId: ProjectId, moduleId: ModuleId) =
      lens(_.schemas(on(schemaId)).projects(on(projectId)).modules(on(moduleId)).main)

    def moduleCompiler(schemaId: SchemaId, projectId: ProjectId, moduleId: ModuleId) =
      lens(_.schemas(on(schemaId)).projects(on(projectId)).modules(on(moduleId)).compiler)

    def moduleId(schemaId: SchemaId, projectId: ProjectId, moduleId: ModuleId) =
      lens(_.schemas(on(schemaId)).projects(on(projectId)).modules(on(moduleId)).id)

    def after(schemaId: SchemaId, projectId: ProjectId, moduleId: ModuleId) =
      lens(_.schemas(on(schemaId)).projects(on(projectId)).modules(on(moduleId)).after)
  
    def sources(schemaId: SchemaId, projectId: ProjectId, moduleId: ModuleId) =
      lens(_.schemas(on(schemaId)).projects(on(projectId)).modules(on(moduleId)).sources)
  
    def binaries(schemaId: SchemaId, projectId: ProjectId, moduleId: ModuleId) =
      lens(_.schemas(on(schemaId)).projects(on(projectId)).modules(on(moduleId)).binaries)
    
    def params(schemaId: SchemaId, projectId: ProjectId, moduleId: ModuleId) =
      lens(_.schemas(on(schemaId)).projects(on(projectId)).modules(on(moduleId)).params)
  
    val aliases = lens(_.aliases)
  
  }
}

