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

import scala.collection.immutable.SortedSet

case class ProjectCli(cli: Cli)(implicit log: Log) {
  import Args._

  def select: Try[ExitStatus] = for {
    layout      <- cli.layout
    conf        <- Layer.readFuryConf(layout)
    layer       <- Layer.read(layout, conf)
    optSchemaId <- ~Some(SchemaId.default)
    dSchema     <- layer.schemas.findBy(optSchemaId.getOrElse(layer.main))
    cli         <- cli.hint(ProjectArg, dSchema.projects)
    cli         <- cli.hint(ForceArg)
    call        <- cli.call()
    projectId   <- ~cli.peek(ProjectArg)
    projectId   <- projectId.asTry
    force       <- ~call(ForceArg).isSuccess
    schemaId    <- ~optSchemaId.getOrElse(layer.main)
    schema      <- layer.schemas.findBy(schemaId)
    _           <- schema(projectId)
    focus       <- ~Lenses.focus()
    layer       <- ~(focus(layer, _.lens(_.main)) = Some(Some(projectId)))
    _           <- Layer.save(layer, layout)
  } yield log.await()

  def list: Try[ExitStatus] = for {
    layout      <- cli.layout
    conf        <- Layer.readFuryConf(layout)
    layer       <- Layer.read(layout, conf)
    optSchemaId <- ~Some(SchemaId.default)
    dSchema     <- layer.schemas.findBy(optSchemaId.getOrElse(layer.main))
    cli         <- cli.hint(ProjectArg, dSchema.projects)
    cli         <- cli.hint(RawArg)
    table       <- ~Tables().projects(dSchema.main)
    cli         <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
    call        <- cli.call()
    projectId   <- ~cli.peek(ProjectArg)
    col         <- ~cli.peek(ColumnArg)
    raw         <- ~call(RawArg).isSuccess
    schema      <- layer.schemas.findBy(optSchemaId.getOrElse(layer.main))
    rows        <- ~schema.projects.to[List]
    table       <- ~Tables().show(table, cli.cols, rows, raw, col, projectId, "project")
    _           <- ~log.infoWhen(!raw)(conf.focus())
    _           <- ~log.rawln(table)
  } yield log.await()

  def add: Try[ExitStatus] = for {
    layout         <- cli.layout
    conf           <- Layer.readFuryConf(layout)
    layer          <- Layer.read(layout, conf)
    optSchemaId    <- ~Some(SchemaId.default)
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

    layer          <- Lenses.updateSchemas(layer)(Lenses.layer.projects(_))(
                          _.modify(_)((_: SortedSet[Project]) + project))

    layer          <- Lenses.updateSchemas(layer)(Lenses.layer.mainProject(_))(_(_) =
                          Some(project.id))

    _              <- Layer.save(layer, layout)
    _              <- ~log.info(msg"Set current project to ${project.id}")
  } yield log.await()

  def remove: Try[ExitStatus] = for {
    layout      <- cli.layout
    conf        <- Layer.readFuryConf(layout)
    layer       <- Layer.read(layout, conf)
    optSchemaId <- ~Some(SchemaId.default)
    dSchema     <- layer.schemas.findBy(optSchemaId.getOrElse(layer.main))
    cli         <- cli.hint(ProjectArg, dSchema.projects)
    cli         <- cli.hint(ForceArg)
    call        <- cli.call()
    projectId   <- call(ProjectArg)
    project     <- dSchema.projects.findBy(projectId)
    force       <- ~call(ForceArg).isSuccess
   
    layer       <- Lenses.updateSchemas(layer)(Lenses.layer.projects(_))(_.modify(_)((_:
                       SortedSet[Project]).filterNot(_.id == project.id)))
   
    layer       <- Lenses.updateSchemas(layer)(Lenses.layer.mainProject(_)) { (lens, ws) =>
                       if(lens(ws) == Some(projectId))(lens(ws) = None) else ws }
   
    _           <- Layer.save(layer, layout)(log)
  } yield log.await()

  def update: Try[ExitStatus] = for {
    layout         <- cli.layout
    conf           <- Layer.readFuryConf(layout)
    layer          <- Layer.read(layout, conf)
    optSchemaId    <- ~Some(SchemaId.default)
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
    projectId      <- projectId.asTry
    schema         <- layer.schemas.findBy(optSchemaId.getOrElse(layer.main))
    project        <- schema.projects.findBy(projectId)
    force          <- ~call(ForceArg).isSuccess
    focus          <- ~Lenses.focus()
    licenseArg     <- ~call(LicenseArg).toOption
    layer          <- ~(focus(layer, _.lens(_.projects(on(project.id)).license)) = licenseArg)
    descriptionArg <- ~call(DescriptionArg).toOption
    layer          <- ~(focus(layer, _.lens(_.projects(on(project.id)).description)) = descriptionArg)
    compilerArg    <- ~call(DefaultCompilerArg).toOption.flatMap(ModuleRef.parseFull(_, true))
    layer          <- ~(focus(layer, _.lens(_.projects(on(project.id)).compiler)) = compilerArg.map(Some(_)))
    nameArg        <- ~call(ProjectNameArg).toOption
    newId          <- ~nameArg.flatMap(schema.projects.unique(_).toOption)
    layer          <- ~(focus(layer, _.lens(_.projects(on(project.id)).id)) = newId)
    
    layer          <- if(newId.isEmpty || schema.main != Some(project.id)) ~layer
                      else ~(focus(layer, _.lens(_.main)) = Some(newId))

    newSchema      <- layer.schemas.findBy(optSchemaId.getOrElse(layer.main))
    lens           <- ~Lenses.layer.schemas
    layer          <- ~lens.modify(layer)(_.filterNot(_.id == schema.id))
    layer          <- ~lens.modify(layer)(_ + newSchema)
    _              <- Layer.save(layer, layout)
  } yield log.await()
}
