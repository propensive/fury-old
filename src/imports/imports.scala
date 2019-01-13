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

object ImportCli {

  case class Context(cli: Cli[CliParam[_]], layout: Layout, config: Config, layer: Layer)

  def context(cli: Cli[CliParam[_]]) =
    for {
      layout <- cli.layout
      config <- fury.Config.read()(cli.env, layout)
      layer <- Layer.read(layout.furyConfig)(layout)
    } yield Context(cli, layout, config, layer)

  def add(ctx: Context) = {
    import ctx._
    for {
      cli <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      schemaArg <- ~cli.peek(SchemaArg)
      defaultSchema <- ~layer.schemas.findBy(schemaArg.getOrElse(layer.main)).toOption
      cli <- cli.hint(ImportArg,
                      defaultSchema.map(_.importCandidates(layout, cli.shell)).getOrElse(Nil))
      io <- cli.io()
      schemaRef <- io(ImportArg)
      layer <- Lenses.updateSchemas(schemaArg, layer, true)(Lenses.layer.imports(_))(
                  _.modify(_)(_ :+ schemaRef))
      _ <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }

  def remove(ctx: Context) = {
    import ctx._
    for {
      cli <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      schemaArg <- ~cli.peek(SchemaArg)
      dSchema <- ~layer.schemas.findBy(schemaArg.getOrElse(layer.main)).toOption
      cli <- cli.hint(ImportArg, dSchema.map(_.imports).getOrElse(Nil))
      io <- cli.io()
      schemaId <- ~io(SchemaArg).toOption.getOrElse(layer.main)
      importArg <- io(ImportArg)
      schema <- layer.schemas.findBy(schemaId)
      lens <- ~Lenses.layer.imports(schema.id)
      layer <- ~lens.modify(layer)(_.filterNot(_ == importArg))
      _ <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }

  def list(ctx: Context) = {
    import ctx._
    for {
      cli <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      schemaArg <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema <- layer.schemas.findBy(schemaArg)
      cols <- Success(Terminal.columns(cli.env).getOrElse(100))
      cli <- cli.hint(RawArg)
      io <- cli.io()
      raw <- ~io(RawArg).isSuccess
      rows <- schema.importedSchemas(layout, cli.shell)
      table <- ~Tables(config).show(Tables(config).schemas(Some(layer.main)), cols, rows, raw)(_.id)
      _ <- ~(if (!raw)
               io.println(Tables(config).contextString(layout.pwd, layer.showSchema, schema))
             else io)
      _ <- ~io.println(UserMsg { theme =>
            table.mkString("\n")
          })
    } yield io.await()
  }
}
