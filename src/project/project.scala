/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.7.5. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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

  def context(cli: Cli[CliParam[_]]) = for {
    layout       <- cli.layout
    config       <- ~cli.config
    layer        <- Layer.read(Log.silent(config), layout, cli.installation)
    cli          <- cli.hint(SchemaArg, layer.schemas)
    optSchemaArg <- ~cli.peek(SchemaArg)
  } yield new MenuContext(cli, layout, config, layer, optSchemaArg)

  def select(ctx: MenuContext): Try[ExitStatus] = {
    import ctx._
    for {
      dSchema   <- layer.schemas.findBy(optSchemaId.getOrElse(layer.main))
      cli       <- cli.hint(ProjectArg, dSchema.projects)
      cli       <- cli.hint(ForceArg)
      invoc     <- cli.read()
      log       <- invoc.logger()
      projectId <- ~cli.peek(ProjectArg)
      projectId <- projectId.ascribe(UnspecifiedProject())
      force     <- ~invoc(ForceArg).isSuccess
      schemaId  <- ~optSchemaId.getOrElse(layer.main)
      schema    <- layer.schemas.findBy(schemaId)
      _         <- schema(projectId)
      focus     <- ~Lenses.focus(optSchemaId, force)
      layer     <- focus(layer, _.lens(_.main)) = Some(Some(projectId))
      _         <- ~Layer.save(log, layer, layout, cli.installation)

    } yield log.await()
  }

  def list(ctx: MenuContext): Try[ExitStatus] = {
    import ctx._
    for {
      cli    <- cli.hint(RawArg)
      invoc  <- cli.read()
      log    <- invoc.logger()
      raw    <- ~invoc(RawArg).isSuccess
      schema <- layer.schemas.findBy(optSchemaId.getOrElse(layer.main))
      rows   <- ~schema.projects.to[List]
      table  <- ~Tables(config).show(Tables(config).projects(schema.main), cli.cols, rows, raw)(_.id)
      _      <- ~(if(!raw) log.println(Tables(config).contextString(layout.base, layer.showSchema, schema)))
      _      <- ~log.println(table.mkString("\n"))
    } yield log.await()
  }

  def add(ctx: MenuContext): Try[ExitStatus] = {
    import ctx._
    for {
      cli            <- cli.hint(ProjectNameArg)
      cli            <- cli.hint(LicenseArg, License.standardLicenses)
      dSchema        <- layer.schemas.findBy(optSchemaId.getOrElse(layer.main))

      cli            <- cli.hint(DefaultCompilerArg, ModuleRef.JavaRef :: dSchema.compilerRefs(
                            Log.silent(config), layout, cli.installation, false))

      invoc          <- cli.read()
      log            <- invoc.logger()
      compilerId     <- ~invoc(DefaultCompilerArg).toOption
      optCompilerRef <- compilerId.map(ModuleRef.parseFull(_, true)).to[List].sequence.map(_.headOption)
      projectId      <- invoc(ProjectNameArg)
      license        <- Success(invoc(LicenseArg).toOption.getOrElse(License.unknown))
      project        <- ~Project(projectId, license = license, compiler = optCompilerRef)

      layer          <- Lenses.updateSchemas(optSchemaId, layer, true)(Lenses.layer.projects(_))(
                            _.modify(_)((_: SortedSet[Project]) + project))

      layer          <- Lenses.updateSchemas(optSchemaId, layer, true)(Lenses.layer.mainProject(_))(_(_) =
                            Some(project.id))

      _              <- ~Layer.save(log, layer, layout, cli.installation)
      _              <- ~log.info(msg"Set current project to ${project.id}")
    } yield log.await()
  }

  def remove(ctx: MenuContext): Try[ExitStatus] = {
    import ctx._
    for {
      dSchema   <- layer.schemas.findBy(optSchemaId.getOrElse(layer.main))
      cli       <- cli.hint(ProjectArg, dSchema.projects)
      cli       <- cli.hint(ForceArg)
      invoc     <- cli.read()
      log       <- invoc.logger()
      projectId <- invoc(ProjectArg)
      project   <- dSchema.projects.findBy(projectId)
      force     <- ~invoc(ForceArg).isSuccess

      layer     <- Lenses.updateSchemas(optSchemaId, layer, force)(Lenses.layer.projects(_))(_.modify(_)((_:
                       SortedSet[Project]).filterNot(_.id == project.id)))

      layer     <- Lenses.updateSchemas(optSchemaId, layer, force)(Lenses.layer.mainProject(_)) { (lens, ws) =>
                       if(lens(ws) == Some(projectId))(lens(ws) = None) else ws }

      _         <- ~Layer.save(log, layer, layout, cli.installation)
    } yield log.await()
  }

  def update(ctx: MenuContext): Try[ExitStatus] = {
    import ctx._
    for {
      dSchema        <- ~layer.schemas.findBy(optSchemaId.getOrElse(layer.main)).toOption
      cli            <- cli.hint(ProjectArg, dSchema.map(_.projects).getOrElse(Nil))
      cli            <- cli.hint(DescriptionArg)

      cli            <- cli.hint(DefaultCompilerArg, ModuleRef.JavaRef :: dSchema.to[List].flatMap(
                            _.compilerRefs(Log.silent(config), layout, cli.installation, false)))
      
      cli            <- cli.hint(ForceArg)
      projectId      <- ~cli.peek(ProjectArg).orElse(dSchema.flatMap(_.main))
      cli            <- cli.hint(LicenseArg, License.standardLicenses)
      cli            <- cli.hint(ProjectNameArg, projectId)
      invoc          <- cli.read()
      log            <- invoc.logger()
      projectId      <- projectId.ascribe(UnspecifiedProject())
      schema         <- layer.schemas.findBy(optSchemaId.getOrElse(layer.main))
      project        <- schema.projects.findBy(projectId)
      force          <- ~invoc(ForceArg).isSuccess
      focus          <- ~Lenses.focus(optSchemaId, force)
      licenseArg     <- ~invoc(LicenseArg).toOption
      layer          <- focus(layer, _.lens(_.projects(on(project.id)).license)) = licenseArg
      descriptionArg <- ~invoc(DescriptionArg).toOption
      layer          <- focus(layer, _.lens(_.projects(on(project.id)).description)) = descriptionArg
      compilerArg    <- ~invoc(DefaultCompilerArg).toOption.flatMap(ModuleRef.parseFull(_, true).toOption)
      layer          <- focus(layer, _.lens(_.projects(on(project.id)).compiler)) = compilerArg.map(Some(_))
      nameArg        <- ~invoc(ProjectNameArg).toOption
      newId          <- ~nameArg.flatMap(schema.unused(_).toOption)
      layer          <- focus(layer, _.lens(_.projects(on(project.id)).id)) = newId
      newSchema      <- layer.schemas.findBy(optSchemaId.getOrElse(layer.main))
      newSchema      <- ~(if(Some(project.id) == newSchema.main) newSchema.copy(main = newId) else newSchema)
      lens           <- ~Lenses.layer.schemas
      layer          <- ~lens.modify(layer)(_.filterNot(_.id == schema.id))
      layer          <- ~lens.modify(layer)(_ + newSchema)
      _              <- ~Layer.save(log, layer, layout, cli.installation)
    } yield log.await()
  }
}
