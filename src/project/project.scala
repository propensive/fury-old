/*
  Fury, version 0.1.0. Copyright 2018 Jon Pretty, Propensive Ltd.

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

import mitigation._
import totalitarian._
import guillotine._

import scala.collection.immutable.SortedSet

object ProjectCli {
  import Args._

  def context(cli: Cli[CliParam[_]]) = for {
    layout       <- cli.layout
    config       <- fury.Config.read()(cli.env, layout)
    workspace    <- fury.Workspace.read(layout.furyConfig)(layout)
    cli          <- cli.hint(SchemaArg, workspace.schemas)
    optSchemaArg <- ~cli.peek(SchemaArg)
  } yield new MenuContext(cli, layout, config, workspace, optSchemaArg)

  def select(ctx: MenuContext) = {
    import ctx._
    for { 
      dSchema       <- workspace.schemas.findBy(optSchemaId.getOrElse(workspace.main))
      cli           <- cli.hint(ProjectArg, dSchema.projects)
      cli           <- cli.hint(ForceArg)
      io            <- cli.io()
      projectId     <- ~cli.peek(ProjectArg)
      projectId     <- projectId.ascribe(UnspecifiedProject())
      force         <- ~io(ForceArg).opt.isDefined
      workspace     <- Lenses.updateSchemas(optSchemaId, workspace, force)(Lenses.workspace.mainProject(_))(_(_) = Some(projectId))
      io            <- ~io.save(workspace, layout.furyConfig)
    } yield io.await()
  }

  def list(ctx: MenuContext) = {
    import ctx._
    for {
      cols   <- Answer(Terminal.columns.getOrElse(100))
      cli    <- cli.hint(RawArg)
      io     <- cli.io()
      raw    <- ~io(RawArg).successful
      schema <- workspace.schemas.findBy(optSchemaId.getOrElse(workspace.main))
      rows   <- ~schema.projects.to[List]
      table  <- ~Tables(config).show(Tables(config).projects(schema.main), cols, rows, raw)(_.id)
      io     <- ~(if(!raw) io.println(Tables(config).contextString(layout.pwd, workspace.showSchema, schema)) else io)
      io     <- ~io.println(table.mkString("\n"))
    } yield io.await()
  }

  def add(ctx: MenuContext) = {
    import ctx._
    for {
      cli       <- cli.hint(ProjectNameArg)
      cli       <- cli.hint(LicenseArg, License.standardLicenses)
      io        <- cli.io()
      projectId <- io(ProjectNameArg)
      license   <- io(LicenseArg).remedy(License.unknown)
      project   <- ~fury.Project(projectId, license = license)
      workspace <- Lenses.updateSchemas(optSchemaId, workspace, true)(Lenses.workspace.projects(_))(_.modify(_)((_: SortedSet[Project]) + project))
      workspace <- Lenses.updateSchemas(optSchemaId, workspace, true)(Lenses.workspace.mainProject(_))(_(_) = Some(project.id))
      io        <- ~io.save(workspace, layout.furyConfig)
      io        <- ~io.println(msg"Set current project to ${project.id}")
    } yield io.await()
  }

  def delete(ctx: MenuContext) = {
    import ctx._
    for {
      dSchema   <- workspace.schemas.findBy(optSchemaId.getOrElse(workspace.main))
      cli       <- cli.hint(ProjectArg, dSchema.projects)
      cli       <- cli.hint(ForceArg)
      io        <- cli.io()
      projectId <- io(ProjectArg)
      project   <- dSchema.projects.findBy(projectId)
      force     <- ~io(ForceArg).opt.isDefined
      workspace <- Lenses.updateSchemas(optSchemaId, workspace, force)(Lenses.workspace.projects(_))(_.modify(_)((_: SortedSet[Project]).filterNot(_.id == project.id)))
      workspace <- Lenses.updateSchemas(optSchemaId, workspace, force)(Lenses.workspace.mainProject(_)) { (lens, ws) =>
                     if(lens(ws) == Some(projectId)) (lens(ws) = None) else ws
                   }
      io        <- ~io.save(workspace, layout.furyConfig)
    } yield io.await()
  }

  def update(ctx: MenuContext) = {
    import ctx._
    for {
      dSchema        <- ~workspace.schemas.findBy(optSchemaId.getOrElse(workspace.main)).opt
      cli            <- cli.hint(ProjectArg, dSchema.map(_.projects).getOrElse(Nil))
      cli            <- cli.hint(DescriptionArg)
      cli            <- cli.hint(ForceArg)
      projectId      <- ~cli.peek(ProjectArg).orElse(dSchema.flatMap(_.main))
      cli            <- cli.hint(LicenseArg, License.standardLicenses)
      cli            <- cli.hint(ProjectNameArg, projectId)
      io             <- cli.io()
      projectId      <- projectId.ascribe(UnspecifiedProject())
      schema         <- workspace.schemas.findBy(optSchemaId.getOrElse(workspace.main))
      oldProject     <- schema.projects.findBy(projectId)
      nameArg        <- ~io(ProjectNameArg).opt
      newId          <- ~nameArg.flatMap(schema.unused(_).opt).getOrElse {
                            oldProject.id }
      licenseArg     <- io(LicenseArg).remedy(oldProject.license)
      descriptionArg <- io(DescriptionArg).remedy(oldProject.description)
      project        <- ~oldProject.copy(id = newId, license = licenseArg, description = descriptionArg)
      force          <- ~io(ForceArg).opt.isDefined
      workspace      <- Lenses.updateSchemas(optSchemaId, workspace, force)(Lenses.workspace.project(_, oldProject.id))(_(_) = project)
      io             <- ~io.save(workspace, layout.furyConfig)
    } yield io.await()
  }
}
