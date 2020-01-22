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
package fury

import fury.strings._, fury.io._, fury.core._, fury.model._

import guillotine._
import optometry._
import mercator._
import scala.util._
import Lenses.on

import scala.collection.immutable.SortedSet

object ProjectCli {
  import Args._

  def context(cli: Cli[CliParam[_]])(implicit log: Log) = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.read(layout, conf)
    optSchemaArg <- ~Some(SchemaId.default)
  } yield new MenuContext(cli, layout, layer, conf, optSchemaArg)

  def select(ctx: MenuContext)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      dSchema   <- layer.schemas.findBy(optSchemaId.getOrElse(layer.main))
      cli       <- cli.hint(ProjectArg, dSchema.projects)
      cli       <- cli.hint(ForceArg)
      call      <- cli.call()
      projectId <- ~cli.peek(ProjectArg)
      projectId <- projectId.ascribe(UnspecifiedProject())
      force     <- ~call(ForceArg).isSuccess
      schemaId  <- ~optSchemaId.getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaId)
      _         <- schema(projectId)
      focus     <- ~Lenses.focus(optSchemaId, force)
      layer     <- focus(layer, _.lens(_.main)) = Some(Some(projectId))
      _         <- ~Layer.save(layer, layout)

    } yield log.await()
  }

  def list(ctx: MenuContext)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli    <- cli.hint(RawArg)
      call   <- cli.call()
      raw    <- ~call(RawArg).isSuccess
      schema <- layer.schemas.findBy(optSchemaId.getOrElse(layer.main))
      rows   <- ~schema.projects.to[List]
      table  <- ~Tables().show(Tables().projects(schema.main), cli.cols, rows, raw)(_.id)
      _      <- ~log.infoWhen(!raw)(conf.focus())
      _      <- ~log.rawln(table.mkString("\n"))
    } yield log.await()
  }

  def add(ctx: MenuContext)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli            <- cli.hint(ProjectNameArg, List(layout.baseDir.name))
      cli            <- cli.hint(LicenseArg, License.standardLicenses)
      dSchema        <- layer.schemas.findBy(optSchemaId.getOrElse(layer.main))

      cli            <- cli.hint(DefaultCompilerArg, ModuleRef.JavaRef :: dSchema.compilerRefs(
                            layout, false))

      call           <- cli.call()
      compilerId     <- ~cli.peek(DefaultCompilerArg)
      
      optCompilerRef <- compilerId.to[List].map { v =>
                          ModuleRef.parseFull(v, true).ascribe(InvalidValue(v))
                        }.sequence.map(_.headOption)

      projectId      <- call(ProjectNameArg)
      license        <- Success(call(LicenseArg).toOption.getOrElse(License.unknown))
      project        <- ~Project(projectId, license = license, compiler = optCompilerRef)

      layer          <- Lenses.updateSchemas(optSchemaId, layer, true)(Lenses.layer.projects(_))(
                            _.modify(_)((_: SortedSet[Project]) + project))

      layer          <- Lenses.updateSchemas(optSchemaId, layer, true)(Lenses.layer.mainProject(_))(_(_) =
                            Some(project.id))

      _              <- ~Layer.save(layer, layout)
      _              <- ~log.info(msg"Set current project to ${project.id}")
    } yield log.await()
  }

  def remove(ctx: MenuContext)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      dSchema   <- layer.schemas.findBy(optSchemaId.getOrElse(layer.main))
      cli       <- cli.hint(ProjectArg, dSchema.projects)
      cli       <- cli.hint(ForceArg)
      call      <- cli.call()
      projectId <- call(ProjectArg)
      project   <- dSchema.projects.findBy(projectId)
      force     <- ~call(ForceArg).isSuccess

      layer     <- Lenses.updateSchemas(optSchemaId, layer, force)(Lenses.layer.projects(_))(_.modify(_)((_:
                       SortedSet[Project]).filterNot(_.id == project.id)))

      layer     <- Lenses.updateSchemas(optSchemaId, layer, force)(Lenses.layer.mainProject(_)) { (lens, ws) =>
                       if(lens(ws) == Some(projectId))(lens(ws) = None) else ws }

      _         <- ~Layer.save(layer, layout)(log)
    } yield log.await()
  }

  def update(ctx: MenuContext)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      dSchema        <- ~layer.schemas.findBy(optSchemaId.getOrElse(layer.main)).toOption
      cli            <- cli.hint(ProjectArg, dSchema.map(_.projects).getOrElse(Nil))
      cli            <- cli.hint(DescriptionArg)

      cli            <- cli.hint(DefaultCompilerArg, ModuleRef.JavaRef :: dSchema.to[List].flatMap(
                            _.compilerRefs(layout, false)))
      
      cli            <- cli.hint(ForceArg)
      projectId      <- ~cli.peek(ProjectArg).orElse(dSchema.flatMap(_.main))
      cli            <- cli.hint(LicenseArg, License.standardLicenses)
      cli            <- cli.hint(ProjectNameArg, ProjectId(layout.baseDir.name) :: projectId.to[List])
      call           <- cli.call()
      projectId      <- projectId.ascribe(UnspecifiedProject())
      schema         <- layer.schemas.findBy(optSchemaId.getOrElse(layer.main))
      project        <- schema.projects.findBy(projectId)
      force          <- ~call(ForceArg).isSuccess
      focus          <- ~Lenses.focus(optSchemaId, force)
      licenseArg     <- ~call(LicenseArg).toOption
      layer          <- focus(layer, _.lens(_.projects(on(project.id)).license)) = licenseArg
      descriptionArg <- ~call(DescriptionArg).toOption
      layer          <- focus(layer, _.lens(_.projects(on(project.id)).description)) = descriptionArg
      compilerArg    <- ~call(DefaultCompilerArg).toOption.flatMap(ModuleRef.parseFull(_, true))
      layer          <- focus(layer, _.lens(_.projects(on(project.id)).compiler)) = compilerArg.map(Some(_))
      nameArg        <- ~call(ProjectNameArg).toOption
      newId          <- ~nameArg.flatMap(schema.projects.unique(_).toOption)
      layer          <- focus(layer, _.lens(_.projects(on(project.id)).id)) = newId
     
      layer          <- if(newId.isEmpty || schema.main != Some(project.id)) ~layer
                        else focus(layer, _.lens(_.main)) = Some(newId)

      newSchema      <- layer.schemas.findBy(optSchemaId.getOrElse(layer.main))
      lens           <- ~Lenses.layer.schemas
      layer          <- ~lens.modify(layer)(_.filterNot(_.id == schema.id))
      layer          <- ~lens.modify(layer)(_ + newSchema)
      _              <- ~Layer.save(layer, layout)
    } yield log.await()
  }
}
