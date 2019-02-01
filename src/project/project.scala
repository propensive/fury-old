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

import guillotine._
import optometry._
import scala.util._
import Lenses.on

import scala.collection.immutable.SortedSet

object ProjectCli {
  import Args._

  def context(cli: Cli[CliParam[_]]) =
    for {
      layout       <- cli.layout
      config       <- fury.Config.read()(cli.env, layout)
      layer        <- Layer.read(Io.silent(config), layout.furyConfig, layout)
      cli          <- cli.hint(SchemaArg, layer.schemas)
      optSchemaArg <- ~cli.peek(SchemaArg)
    } yield new MenuContext(cli, layout, config, layer, optSchemaArg)

  def select(ctx: MenuContext) = {
    import ctx._
    for {
      dSchema   <- layer.schemas.findBy(optSchemaId.getOrElse(layer.main))
      cli       <- cli.hint(ProjectArg, dSchema.projects)
      cli       <- cli.hint(ForceArg)
      invoc     <- cli.read()
      io        <- invoc.io()
      projectId <- ~cli.peek(ProjectArg)
      projectId <- projectId.ascribe(UnspecifiedProject())
      force     <- ~invoc(ForceArg).toOption.isDefined
      schemaId  <- ~optSchemaId.getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaId)
      _         <- schema(projectId)
      focus     <- ~Lenses.focus(optSchemaId, force)
      layer     <- focus(layer, _.lens(_.main)) = Some(Some(projectId))
      _         <- ~Layer.save(layer, layout)
    } yield io.await()
  }

  def list(ctx: MenuContext) = {
    import ctx._
    for {
      cols   <- Success(Terminal.columns.getOrElse(100))
      cli    <- cli.hint(RawArg)
      invoc  <- cli.read()
      io     <- invoc.io()
      raw    <- ~invoc(RawArg).isSuccess
      schema <- layer.schemas.findBy(optSchemaId.getOrElse(layer.main))
      rows   <- ~schema.projects.to[List]
      table  <- ~Tables(config).show(Tables(config).projects(schema.main), cols, rows, raw)(_.id)
      _ <- ~(if (!raw)
               io.println(Tables(config).contextString(layout.pwd, layer.showSchema, schema)))
      _ <- ~io.println(table.mkString("\n"))
    } yield io.await()
  }

  def add(ctx: MenuContext) = {
    import ctx._
    for {
      cli       <- cli.hint(ProjectNameArg)
      cli       <- cli.hint(LicenseArg, License.standardLicenses)
      invoc     <- cli.read()
      io        <- invoc.io()
      projectId <- invoc(ProjectNameArg)
      license   <- Success(invoc(LicenseArg).toOption.getOrElse(License.unknown))
      project   <- ~fury.Project(projectId, license = license)
      layer <- Lenses.updateSchemas(optSchemaId, layer, true)(Lenses.layer.projects(_))(
                  _.modify(_)((_: SortedSet[Project]) + project))
      layer <- Lenses.updateSchemas(optSchemaId, layer, true)(Lenses.layer.mainProject(_))(
                  _(_) = Some(project.id))
      _ <- ~Layer.save(layer, layout)
      _ <- ~io.println(msg"Set current project to ${project.id}")
    } yield io.await()
  }

  def remove(ctx: MenuContext) = {
    import ctx._
    for {
      dSchema   <- layer.schemas.findBy(optSchemaId.getOrElse(layer.main))
      cli       <- cli.hint(ProjectArg, dSchema.projects)
      cli       <- cli.hint(ForceArg)
      invoc     <- cli.read()
      io        <- invoc.io()
      projectId <- invoc(ProjectArg)
      project   <- dSchema.projects.findBy(projectId)
      force     <- ~invoc(ForceArg).toOption.isDefined
      layer <- Lenses.updateSchemas(optSchemaId, layer, force)(Lenses.layer.projects(_))(
                  _.modify(_)((_: SortedSet[Project]).filterNot(_.id == project.id)))
      layer <- Lenses.updateSchemas(optSchemaId, layer, force)(Lenses.layer.mainProject(_)) {
                (lens, ws) =>
                  if (lens(ws) == Some(projectId))(lens(ws) = None) else ws
              }
      _ <- ~Layer.save(layer, layout)
    } yield io.await()
  }

  def update(ctx: MenuContext) = {
    import ctx._
    for {
      dSchema        <- ~layer.schemas.findBy(optSchemaId.getOrElse(layer.main)).toOption
      cli            <- cli.hint(ProjectArg, dSchema.map(_.projects).getOrElse(Nil))
      cli            <- cli.hint(DescriptionArg)
      cli            <- cli.hint(ForceArg)
      projectId      <- ~cli.peek(ProjectArg).orElse(dSchema.flatMap(_.main))
      cli            <- cli.hint(LicenseArg, License.standardLicenses)
      cli            <- cli.hint(ProjectNameArg, projectId)
      invoc          <- cli.read()
      io             <- invoc.io()
      projectId      <- projectId.ascribe(UnspecifiedProject())
      schema         <- layer.schemas.findBy(optSchemaId.getOrElse(layer.main))
      project        <- schema.projects.findBy(projectId)
      force          <- ~invoc(ForceArg).toOption.isDefined
      focus          <- ~Lenses.focus(optSchemaId, force)
      licenseArg     <- ~invoc(LicenseArg).toOption
      layer          <- focus(layer, _.lens(_.projects(on(project.id)).license)) = licenseArg
      descriptionArg <- ~invoc(DescriptionArg).toOption
      layer          <- focus(layer, _.lens(_.projects(on(project.id)).description)) = descriptionArg
      nameArg        <- ~invoc(ProjectNameArg).toOption
      newId          <- ~nameArg.flatMap(schema.unused(_).toOption)
      layer          <- focus(layer, _.lens(_.projects(on(project.id)).id)) = newId
      _              <- ~Layer.save(layer, layout)
    } yield io.await()
  }
}
