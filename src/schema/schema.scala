/*
  Fury, version 0.2.2. Copyright 2019 Jon Pretty, Propensive Ltd.

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

import Args._

import guillotine._
import scala.util._

case class SchemaCtx(cli: Cli[CliParam[_]], layout: Layout, config: Config, layer: Layer)

object SchemaCli {

  def context(cli: Cli[CliParam[_]]) =
    for {
      layout <- cli.layout
      config <- fury.Config.read()(cli.env, layout)
      layer  <- Layer.read(layout.furyConfig, layout)
    } yield SchemaCtx(cli, layout, config, layer)

  def select(ctx: SchemaCtx) = {
    import ctx._
    for {
      cli      <- ctx.cli.hint(SchemaArg, ctx.layer.schemas.map(_.id))
      invoc    <- cli.read()
      io       <- invoc.io()
      schemaId <- invoc(SchemaArg)
      _        <- layer(schemaId)
      lens     <- ~Lenses.layer.mainSchema
      layer    <- ~(lens(layer) = schemaId)
      _        <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }

  def list(ctx: SchemaCtx) = {
    import ctx._
    for {
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      schemaArg <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaArg)
      cols      <- Success(Terminal.columns(cli.env).getOrElse(100))
      cli       <- cli.hint(RawArg)
      invoc     <- cli.read()
      io        <- invoc.io()
      raw       <- ~invoc(RawArg).isSuccess
      rows      <- ~layer.schemas.to[List]
      table     <- ~Tables(config).show(Tables(config).schemas(Some(schema.id)), cols, rows, raw)(_.id)
      _ <- ~(if (!raw)
               io.println(Tables(config).contextString(layout.pwd, layer.showSchema, schema)))
      _ <- ~io.println(UserMsg { theme =>
            table.mkString("\n")
          })
    } yield io.await()
  }

  def diff(ctx: SchemaCtx) = {
    import ctx._
    for {
      cli       <- ctx.cli.hint(SchemaArg, ctx.layer.schemas.map(_.id))
      cli       <- ctx.cli.hint(CompareArg, ctx.layer.schemas.map(_.id))
      cli       <- cli.hint(RawArg)
      invoc     <- cli.read()
      io        <- invoc.io()
      raw       <- ~invoc(RawArg).isSuccess
      schemaArg <- ~invoc(SchemaArg).toOption.getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaArg)
      otherArg  <- invoc(CompareArg)
      other     <- layer.schemas.findBy(otherArg)
      cols      <- Success(Terminal.columns(cli.env).getOrElse(100))
      rows      <- ~Diff.gen[Schema].diff(schema, other)
      table <- ~Tables(config).show(
                  Tables(config).differences(schema.id.key, other.id.key),
                  cols,
                  rows,
                  raw)(_.label)
      _ <- ~(if (!raw)
               io.println(Tables(config).contextString(layout.pwd, layer.showSchema, schema)))
      _ <- ~io.println(UserMsg { theme =>
            table.mkString("\n")
          })
    } yield io.await()
  }

  def update(ctx: SchemaCtx) = {
    import ctx._
    for {
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      cli       <- cli.hint(SchemaNameArg)
      invoc     <- cli.read()
      io        <- invoc.io()
      name      <- invoc(SchemaNameArg)
      schemaId  <- ~invoc(SchemaArg).toOption.getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaId)
      newSchema <- ~schema.copy(id = name)
      lens      <- ~Lenses.layer.schemas
      layer     <- ~lens.modify(layer)(_ - schema + newSchema)
      layer     <- ~(if (layer.main == schema.id) layer.copy(main = newSchema.id) else layer)
      _         <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }

  def add(ctx: SchemaCtx) = {
    import ctx._
    for {
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      cli       <- cli.hint(SchemaNameArg)
      invoc     <- cli.read()
      io        <- invoc.io()
      name      <- invoc(SchemaNameArg)
      schemaId  <- ~invoc(SchemaArg).toOption.getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaId)
      newSchema <- ~schema.copy(id = name)
      lens      <- ~Lenses.layer.schemas
      layer     <- ~lens.modify(layer)(_ + newSchema)
      layer     <- ~layer.copy(main = newSchema.id)
      _         <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }

  def remove(ctx: SchemaCtx) = {
    import ctx._
    for {
      cli      <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      invoc    <- cli.read()
      io       <- invoc.io()
      schemaId <- ~invoc(SchemaArg).toOption.getOrElse(layer.main)
      schema   <- layer.schemas.findBy(schemaId)
      lens     <- ~Lenses.layer.schemas
      layer    <- ~lens.modify(layer)(_.filterNot(_.id == schema.id))
      _        <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }
}
