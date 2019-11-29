/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.7.14. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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
package fury

import fury.strings._, fury.io._, fury.core._, fury.model._

import Args._

import guillotine._

import scala.util._

import language.higherKinds

case class SchemaCtx(cli: Cli[CliParam[_]], layout: Layout, layer: Layer)

object SchemaCli {

  def context(cli: Cli[CliParam[_]])(implicit log: Log) = for {
    layout <- cli.layout
    layer  <- Layer.read(layout)
  } yield SchemaCtx(cli, layout, layer)

  def select(ctx: SchemaCtx)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli      <- ctx.cli.hint(SchemaArg, ctx.layer.schemas.map(_.id))
      call     <- cli.call()
      schemaId <- call(SchemaArg)
      _        <- layer(schemaId)
      lens     <- ~Lenses.layer.mainSchema
      layer    <- ~(lens(layer) = schemaId)
      _        <- ~Layer.save(layer, layout)
    } yield log.await()
  }

  def list(ctx: SchemaCtx)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      schemaArg <- ~cli.peek(SchemaArg).getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaArg)
      cli       <- cli.hint(RawArg)
      call      <- cli.call()
      raw       <- ~call(RawArg).isSuccess
      rows      <- ~layer.schemas.to[List]
      table     <- ~Tables().show(Tables().schemas(Some(schema.id)), cli.cols, rows, raw)(_.id)
      _         <- ~(if(!raw) log.info(Tables().contextString(layout.baseDir, layer.showSchema, schema)))
      _         <- ~log.info(UserMsg { theme => table.mkString("\n") })
    } yield log.await()
  }

  private[this] def diffTable(left: Schema,
                              right: Schema,
                              rows: Seq[Difference],
                              cols: Int,
                              raw: Boolean) =
    Tables().show(Tables().differences(left.id.key, right.id.key), cols, rows, raw)(_.label)

  def diff(ctx: SchemaCtx)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli       <- ctx.cli.hint(SchemaArg, ctx.layer.schemas.map(_.id))
      cli       <- ctx.cli.hint(CompareArg, ctx.layer.schemas.map(_.id))
      cli       <- cli.hint(RawArg)
      call      <- cli.call()
      raw       <- ~call(RawArg).isSuccess
      schemaArg <- ~call(SchemaArg).toOption.getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaArg)
      otherArg  <- call(CompareArg)
      other     <- layer.schemas.findBy(otherArg)
      rows      <- ~Diff.gen[Schema].diff(schema, other)
      table     <- ~diffTable(schema, other, rows, cli.cols, raw)
      _         <- ~(if(!raw) log.info(Tables().contextString(layout.baseDir, layer.showSchema, schema)))
      _         <- ~log.info(UserMsg { theme => table.mkString("\n") })
    } yield log.await()
  }

  def update(ctx: SchemaCtx)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli      <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      cli      <- cli.hint(SchemaNameArg)
      call     <- cli.call()
      newName  <- call(SchemaNameArg)
      schemaId <- ~call(SchemaArg).toOption.getOrElse(layer.main)
      schema   <- layer.schemas.findBy(schemaId)
      force    <- ~call(ForceArg).isSuccess
      focus    <- ~Lenses.focus(Some(schemaId), force)
      layer    <- focus(layer, _.lens(_.id)) = Some(newName)
      layer    <- ~(if(layer.main == schema.id) layer.copy(main = newName) else layer)
      _        <- ~Layer.save(layer, layout)
    } yield log.await()
  }

  def add(ctx: SchemaCtx)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli       <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      cli       <- cli.hint(SchemaNameArg)
      call      <- cli.call()
      name      <- call(SchemaNameArg)
      schemaId  <- ~call(SchemaArg).toOption.getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaId)
      newSchema <- ~schema.copy(id = name)
      lens      <- ~Lenses.layer.schemas
      layer     <- ~lens.modify(layer)(_ + newSchema)
      layer     <- ~layer.copy(main = newSchema.id)
      _         <- ~Layer.save(layer, layout)
    } yield log.await()
  }

  def remove(ctx: SchemaCtx)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli      <- cli.hint(SchemaArg, layer.schemas.map(_.id))
      call     <- cli.call()
      schemaId <- ~call(SchemaArg).toOption.getOrElse(layer.main)
      schema   <- layer.schemas.findBy(schemaId)
      lens     <- ~Lenses.layer.schemas
      layer    <- ~lens.modify(layer)(_.filterNot(_.id == schema.id))
      _        <- ~Layer.save(layer, layout)
    } yield log.await()
  }
}
