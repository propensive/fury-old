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

import fury.error._

import optometry._
import scala.collection.immutable.SortedSet

import scala.util._

object Lenses {

  def updateSchemas[A](
      schemaId: Option[SchemaId],
      layer: Layer,
      force: Boolean
    )(lens: SchemaId => Lens[Layer, A, A]
    )(modify: (Lens[Layer, A, A], Layer) => Layer
    ): Outcome[Layer] = {
    val lenses = schemaId match {
      case Some(schemaId) => List(lens(schemaId))
      case None           => layer.schemas.map(_.id).to[List].map(lens(_))
    }

    for (lenses <- if (force || lenses.map(_(layer)).to[Set].size == 1) Success(lenses)
                  else Failure(SchemaDifferences()))
      yield lenses.foldLeft(layer) { case (layer, lens) => modify(lens, layer) }
  }

  def focus(schemaId: Option[SchemaId], force: Boolean) = new Focus(schemaId, force)

  class Focus(schemaId: Option[SchemaId], force: Boolean) {

    def update[A](
        layer: Layer,
        partialLens: Lens.Partial[Schema] => Lens[Schema, A, A],
        value: Option[A]
      ): Outcome[Layer] = value match {
      case None => Success(layer)
      case Some(value) =>
        val schemaLens = new Lens.Partial[Schema]()
        val lenses = schemaId match {
          case Some(schemaId) =>
            List(Optic.identity.compose(Lenses.layer.schema(schemaId), partialLens(schemaLens)))
          case None =>
            layer.schemas.map(_.id).to[List].map { s =>
              Optic.identity.compose(Lenses.layer.schema(s), partialLens(schemaLens))
            }
        }

        for (lenses <- if (force || lenses.map(_(layer)).to[Set].size == 1) Success(lenses)
                      else Failure(SchemaDifferences()))
          yield lenses.foldLeft(layer) { case (layer, lens) => lens(layer) = value }
    }
  }

  object on {
    type Id[T] = T

    def apply[A, AId](id: AId)(implicit resolver: Resolver[A, AId]): Optic[SortedSet, Id, A] =
      new Optic[SortedSet, Id, A]("focus") {
        def map[B](v: SortedSet[A])(fn: A => B): B     = fn(v.find(resolver.matchOn(id, _)).get)
        def comap(f: SortedSet[A], g: A): SortedSet[A] = f.filterNot(resolver.matchOn(id, _)) + g
      }
  }

  object layer extends Lens.Partial[Layer]() {
    def schema(schemaId: SchemaId) = lens(_.schemas(on(schemaId)))

  }
}
