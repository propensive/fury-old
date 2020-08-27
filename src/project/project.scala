/*

    Fury, version 0.18.13. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury

import fury.text._, fury.io._, fury.core._, fury.model._

import guillotine._
import escritoire._
import optometry._
import mercator._
import scala.util._

import scala.collection.immutable.{SortedSet, TreeSet}

case class ProjectCli(cli: Cli)(implicit val log: Log) extends CliApi {
  import Args._

  lazy val getTable: Try[Tabulation[Project]] = getLayer >> (_.main) >> (Tables().projects(_))
  
  implicit lazy val columnHints: ColumnArg.Hinter =
    ColumnArg.hint(getTable.map(_.headings.map(_.name.toLowerCase)))

  def select: Try[ExitStatus] = (cli -< ProjectArg).action {
    (getLayer, cliProject >> (_.id) >> (Some(_))) >> (Layer(_.main)(_) = _) >> commit >> finish
  }

  def list: Try[ExitStatus] = (cli -< ProjectArg -< RawArg -< ColumnArg).action { for {
    col     <- ~cli.peek(ColumnArg)
    rows    <- getLayer >> (_.projects)
    project <- ~cliProject.toOption
    table   <- getTable >> (Tables().show(_, cli.cols, rows, has(RawArg), col, project >> (_.id), "project"))
    _       <- conf >> (_.focus()) >> (log.infoWhen(!has(RawArg))(_))
    _       <- ~log.rawln(table)
  } yield log.await() }

  def add: Try[ExitStatus] = (cli -< ProjectNameArg -< LicenseArg -< DefaultCompilerArg).action { for {
    optLicense  <- opt(LicenseArg).map(_.getOrElse(License.unknown))
    optCompiler <- opt(DefaultCompilerArg)
    project     <- get(ProjectNameArg) >> (Project(_, TreeSet(), None, optLicense, "", optCompiler))
    _           <- getLayer >>= (_.projects.unique(project.id))
    layer       <- getLayer >>
                       (Layer(_.projects).modify(_)(_ + project)) >>
                       (Layer(_.main)(_) = Some(project.id))

    _           <- commit(layer)
    _           <- ~log.info(msg"Set current project to ${project.id}")
  } yield log.await() }

  def remove: Try[ExitStatus] = (cli -< ProjectArg -< ForceArg).action { for {
    project <- cliProject
    layer   <- getLayer >> (Layer(_.projects).modify(_)(_.evict(project.id)))
    layer   <- ~Layer(_.main).modify(layer) { v => if(v == Some(project.id)) None else v }
    _       <- commit(layer)
  } yield log.await() }

  // FIXME: Todo
  def update: Try[ExitStatus] = for {
    layout         <- cli.layout
    conf           <- Layer.readFuryConf(layout)
    layer          <- Layer.retrieve(conf)
    cli            <- cli.hint(ProjectArg, layer.projects)
    cli            <- cli.hint(DescriptionArg)
    cli            <- cli.hint(DefaultCompilerArg, ModuleRef.JavaRef :: layer.compilerRefs(layout))
    cli            <- cli.hint(ForceArg)
    projectId      <- ~cli.peek(ProjectArg).orElse(layer.main)
    cli            <- cli.hint(LicenseArg, License.standardLicenses)
    cli            <- cli.hint(ProjectNameArg, ProjectId(layout.baseDir.name) :: projectId.to[List])
    call           <- cli.call()
    projectId      <- projectId.asTry
    project        <- layer.projects.findBy(projectId)
    force          <- ~call(ForceArg).isSuccess
    licenseArg     <- ~call(LicenseArg).toOption
    layer          <- ~licenseArg.fold(layer)(Layer(_.projects(project.id).license)(layer) = _)
    descriptionArg <- ~call(DescriptionArg).toOption
    layer          <- ~descriptionArg.fold(layer)(Layer(_.projects(project.id).description)(layer) = _)
    compilerArg    <- ~call(DefaultCompilerArg).toOption
    layer          <- ~compilerArg.map(Some(_)).fold(layer)(Layer(_.projects(project.id).compiler)(layer) = _)
    nameArg        <- ~call(ProjectNameArg).toOption
    newId          <- ~nameArg.flatMap(layer.projects.unique(_).toOption)
    layer          <- ~newId.fold(layer)(Layer(_.projects(project.id).id)(layer) = _)
    
    layer          <- if(newId.isEmpty || layer.main != Some(project.id)) ~layer
                      else ~(Layer(_.main)(layer) = newId)

    _              <- Layer.commit(layer, conf, layout)
  } yield log.await()
}