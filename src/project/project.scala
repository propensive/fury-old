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

import guillotine._
import optometry._
import mercator._
import scala.util._
import Lenses.on

import scala.collection.immutable.SortedSet

object ProjectCli {
  import Args._

  def select(cli: Cli[CliParam[_]]): Try[ExitStatus] = {
    for {
      layout       <- cli.layout
      config       <- Config.read()(cli.env, layout)
      context      <- fury.core.Context.read(layout)
      focus        <- Layers(context, Io.silent(config), layout)
      cli          <- cli.hint(SchemaArg, focus.layer.schemas)
      optSchemaId <- ~cli.peek(SchemaArg)
      dSchema   <- focus.layer.schemas.findBy(optSchemaId.getOrElse(focus.schemaId))
      cli       <- cli.hint(ProjectArg, dSchema.projects)
      cli       <- cli.hint(ForceArg)
      invoc     <- cli.read()
      io        <- invoc.io()
      projectId <- ~cli.peek(ProjectArg)
      projectId <- projectId.ascribe(UnspecifiedProject())
      force     <- ~invoc(ForceArg).toOption.isDefined
      schemaId  <- ~optSchemaId.getOrElse(focus.schemaId)
      schema    <- focus.layer.schemas.findBy(schemaId)
      _         <- schema(projectId)
      lens     <- ~Lenses.focus(optSchemaId, force)
      layer     <- lens(focus.layer, _.lens(_.main)) = Some(Some(projectId))
      ctx         <- Layers.update(context, io, layout, layer)
      _ <- Context.write(context, layout)
    } yield io.await()
  }

  def list(cli: Cli[CliParam[_]]): Try[ExitStatus] = {
    for {
      layout       <- cli.layout
      config       <- Config.read()(cli.env, layout)
      context      <- Context.read(layout)
      focus        <- Layers(context, Io.silent(config), layout)
      cli          <- cli.hint(SchemaArg, focus.layer.schemas)
      optSchemaId <- ~cli.peek(SchemaArg)
      cli    <- cli.hint(RawArg)
      invoc  <- cli.read()
      io     <- invoc.io()
      raw    <- ~invoc(RawArg).isSuccess
      schema <- focus.layer.schemas.findBy(optSchemaId.getOrElse(focus.schemaId))
      rows   <- ~schema.projects.to[List]
      table  <- ~Tables(config).show(Tables(config).projects(schema.main), cli.cols, rows, raw)(_.id)
      _ <- ~(if (!raw)
               io.println(Tables(config).contextString(layout.base, focus.layer.showSchema, schema)))
      _ <- ~io.println(table.mkString("\n"))
    } yield io.await()
  }

  def add(cli: Cli[CliParam[_]]): Try[ExitStatus] = {
    for {
      layout       <- cli.layout
      config       <- Config.read()(cli.env, layout)
      context      <- Context.read(layout)
      focus        <- Layers(context, Io.silent(config), layout)
      cli          <- cli.hint(SchemaArg, focus.layer.schemas)
      optSchemaId <- ~cli.peek(SchemaArg)
      cli     <- cli.hint(ProjectNameArg)
      cli     <- cli.hint(LicenseArg, License.standardLicenses)
      dSchema <- focus.layer.schemas.findBy(optSchemaId.getOrElse(focus.schemaId))
      cli <- cli.hint(
                DefaultCompilerArg,
                ModuleRef.JavaRef :: dSchema.compilerRefs(Io.silent(config), layout))
      invoc      <- cli.read()
      io         <- invoc.io()
      compilerId <- ~invoc(DefaultCompilerArg).toOption
      optCompilerRef <- compilerId
                         .map(ModuleRef.parseFull(_, true))
                         .to[List]
                         .sequence
                         .map(_.headOption)
      projectId <- invoc(ProjectNameArg)
      license   <- Success(invoc(LicenseArg).toOption.getOrElse(License.unknown))
      project   <- ~Project(projectId, license = license, compiler = optCompilerRef)
      layer <- Lenses.updateSchemas(optSchemaId, focus.layer, true)(Lenses.layer.projects(_))(
                  _.modify(_)((_: SortedSet[Project]) + project))
      layer <- Lenses.updateSchemas(optSchemaId, layer, true)(Lenses.layer.mainProject(_))(
                  _(_) = Some(project.id))
      context       <- Layers.update(context, io, layout, layer)
      _ <- Context.write(context, layout)
      _ <- ~io.println(msg"Set current project to ${project.id}")
    } yield io.await()
  }

  def remove(cli: Cli[CliParam[_]]): Try[ExitStatus] = {
    for {
      layout       <- cli.layout
      config       <- Config.read()(cli.env, layout)
      context      <- Context.read(layout)
      focus        <- Layers(context, Io.silent(config), layout)
      cli          <- cli.hint(SchemaArg, focus.layer.schemas)
      optSchemaId <- ~cli.peek(SchemaArg)
      dSchema   <- focus.layer.schemas.findBy(optSchemaId.getOrElse(focus.schemaId))
      cli       <- cli.hint(ProjectArg, dSchema.projects)
      cli       <- cli.hint(ForceArg)
      invoc     <- cli.read()
      io        <- invoc.io()
      projectId <- invoc(ProjectArg)
      project   <- dSchema.projects.findBy(projectId)
      force     <- ~invoc(ForceArg).toOption.isDefined
      layer <- Lenses.updateSchemas(optSchemaId, focus.layer, force)(Lenses.layer.projects(_))(
                  _.modify(_)((_: SortedSet[Project]).filterNot(_.id == project.id)))
      layer <- Lenses.updateSchemas(optSchemaId, layer, force)(Lenses.layer.mainProject(_)) {
                (lens, ws) =>
                  if (lens(ws) == Some(projectId))(lens(ws) = None) else ws
              }
      context         <- Layers.update(context, io, layout, layer)
      _ <- Context.write(context, layout)
    } yield io.await()
  }

  def update(cli: Cli[CliParam[_]]): Try[ExitStatus] = {
    for {
      layout       <- cli.layout
      config       <- Config.read()(cli.env, layout)
      context      <- Context.read(layout)
      focus        <- Layers(context, Io.silent(config), layout)
      cli          <- cli.hint(SchemaArg, focus.layer.schemas)
      optSchemaId <- ~cli.peek(SchemaArg)
      dSchema <- ~focus.layer.schemas.findBy(optSchemaId.getOrElse(focus.schemaId)).toOption
      cli     <- cli.hint(ProjectArg, dSchema.map(_.projects).getOrElse(Nil))
      cli     <- cli.hint(DescriptionArg)
      cli <- cli.hint(
                DefaultCompilerArg,
                ModuleRef.JavaRef :: dSchema
                  .to[List]
                  .flatMap(_.compilerRefs(Io.silent(config), layout)))
      cli            <- cli.hint(ForceArg)
      projectId      <- ~cli.peek(ProjectArg).orElse(dSchema.flatMap(_.main))
      cli            <- cli.hint(LicenseArg, License.standardLicenses)
      cli            <- cli.hint(ProjectNameArg, projectId)
      invoc          <- cli.read()
      io             <- invoc.io()
      projectId      <- projectId.ascribe(UnspecifiedProject())
      schema         <- focus.layer.schemas.findBy(optSchemaId.getOrElse(focus.schemaId))
      project        <- schema.projects.findBy(projectId)
      force          <- ~invoc(ForceArg).toOption.isDefined
      lens          <- ~Lenses.focus(optSchemaId, force)
      licenseArg     <- ~invoc(LicenseArg).toOption
      layer          <- lens(focus.layer, _.lens(_.projects(on(project.id)).license)) = licenseArg
      descriptionArg <- ~invoc(DescriptionArg).toOption
      layer          <- lens(layer, _.lens(_.projects(on(project.id)).description)) = descriptionArg
      compilerArg <- ~invoc(DefaultCompilerArg).toOption
                      .flatMap(ModuleRef.parseFull(_, true).toOption)
      layer   <- lens(layer, _.lens(_.projects(on(project.id)).compiler)) = compilerArg.map(Some(_))
      nameArg <- ~invoc(ProjectNameArg).toOption
      newId   <- ~nameArg.flatMap(schema.unused(_).toOption)
      layer   <- lens(layer, _.lens(_.projects(on(project.id)).id)) = newId
      ctx         <- Layers.update(context, io, layout, layer)
      _ <- Context.write(context, layout)
    } yield io.await()
  }
}
