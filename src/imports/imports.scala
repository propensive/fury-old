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

import fury.strings._, fury.io._, fury.core._

import Args._

import guillotine._
import scala.util._

object ImportCli {

  case class Context(cli: Cli[CliParam[_]], layout: Layout, config: Config, layer: Layer)

  def context(cli: Cli[CliParam[_]]) =
    for {
      layout <- cli.layout
      config <- Config.read()(cli.env, layout)
      layer  <- Layer.read(Io.silent(config), layout.layerFile, layout)
    } yield Context(cli, layout, config, layer)

  def add(ctx: Context): Try[ExitStatus] = {
    import ctx._
    for {
      cli           <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      schemaArg     <- ~cli.peek(SchemaArg)
      defaultSchema <- ~layer.schemas.findBy(schemaArg.getOrElse(layer.main)).toOption
      cli <- cli.hint(
                ImportArg,
                defaultSchema.map(_.importCandidates(Io.silent(config), layout)).getOrElse(Nil))
      invoc     <- cli.read()
      io        <- invoc.io()
      schemaRef <- invoc(ImportArg)
      layer <- Lenses.updateSchemas(schemaArg, layer, true)(Lenses.layer.imports(_))(
                  _.modify(_)(_ + schemaRef))
      _ <- ~Layer.save(io, layer, layout)
    } yield io.await()
  }

  def remove(ctx: Context): Try[ExitStatus] = {
    import ctx._
    for {
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      schemaArg <- ~cli.peek(SchemaArg)
      dSchema   <- ~layer.schemas.findBy(schemaArg.getOrElse(layer.main)).toOption
      cli       <- cli.hint(ImportArg, dSchema.map(_.imports).getOrElse(Nil))
      invoc     <- cli.read()
      io        <- invoc.io()
      schemaId  <- ~invoc(SchemaArg).toOption.getOrElse(layer.main)
      importArg <- invoc(ImportArg)
      schema    <- layer.schemas.findBy(schemaId)
      lens      <- ~Lenses.layer.imports(schema.id)
      layer     <- ~lens.modify(layer)(_.filterNot(_ == importArg))
      _         <- ~Layer.save(io, layer, layout)
    } yield io.await()
  }

  def list(ctx: Context): Try[ExitStatus] = {
    import ctx._
    for {
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      schemaArg <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaArg)
      cli       <- cli.hint(RawArg)
      invoc     <- cli.read()
      io        <- invoc.io()
      raw       <- ~invoc(RawArg).isSuccess
      rows <- ~schema.imports.to[List].map { i =>
               (i, i.resolve(io, schema, layout))
             }
      table <- ~Tables(config).show(Tables(config).imports(Some(layer.main)), cli.cols, rows, raw)(
                  _._1.schema.key)
      _ <- ~(if (!raw)
               io.println(Tables(config).contextString(layout.base, layer.showSchema, schema))
             else io)
      _ <- ~io.println(UserMsg { theme =>
            table.mkString("\n")
          })
    } yield io.await()
  }
}
