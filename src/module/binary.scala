/*

    Fury, version 0.35.0. Copyright 2018-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury

import fury.model._, fury.text._, fury.core._

import scala.util._

import Args._

case class BinaryCli(cli: Cli)(implicit log: Log) {
  def list: Try[ExitStatus] = for {
    layout     <- cli.layout
    conf       <- Layer.readFuryConf(layout)
    layer      <- Layer.retrieve(conf)
    cli        <- cli.hint(ProjectArg, layer.projects)
    projectId  <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject <- ~projectId.flatMap(layer.projects.findBy(_).toOption)
    cli        <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    moduleId   <- cli.preview(ModuleArg)(optProject.flatMap(_.main))
    optModule   = for(project <- optProject; module <- project.modules.findBy(moduleId).toOption) yield module
    cli        <- cli.hint(RawArg)
    table      <- ~Tables().binaries
    cli        <- cli.hint(BinaryArg, optModule.map(_.binaries).getOrElse(Nil).map(_.id))
    cli        <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
    call       <- cli.call()
    col        <- ~cli.peek(ColumnArg)
    binaryId   <- ~cli.peek(BinaryArg)
    raw        <- ~call(RawArg).isSuccess
    project    <- optProject.asTry
    module     <- optModule.asTry
    rows       <- ~module.allBinaries.to[List]
    table      <- ~Tables().show(table, cli.cols, rows, raw, col, binaryId, "binary")
    _          <- ~log.infoWhen(!raw)(conf.focus(project.id, module.id))
    _          <- ~log.rawln(table)
  } yield log.await()

  def update: Try[ExitStatus] = for {
    layout     <- cli.layout
    conf       <- Layer.readFuryConf(layout)
    layer      <- Layer.retrieve(conf)
    cli        <- cli.hint(ProjectArg, layer.projects)
    projectId  <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject <- ~projectId.flatMap(layer.projects.findBy(_).toOption)
    cli        <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    moduleId   <- cli.preview(ModuleArg)(optProject.flatMap(_.main))
    optModule   = for(project <- optProject; module <- project.modules.findBy(moduleId).toOption) yield module
    cli        <- cli.hint(BinaryArg, optModule.to[List].flatMap(_.binaries.map(_.id)))
    cli        <- cli.hint(VersionArg)
    call       <- cli.call()
    binaryArg  <- call(BinaryArg)
    versionArg <- call(VersionArg)
    project    <- optProject.asTry
    module     <- optModule.asTry
    binary     <- module.binaries.findBy(binaryArg)
    layer      <- ~Layer(_.projects(project.id).modules(module.id).binaries).modify(layer)(_ - binary)
    _          <- Layer.commit(layer, conf, layout)
    _          <- ~Build.asyncBuild(layer, module.ref(project), layout)
  } yield log.await()

  def remove: Try[ExitStatus] = for {
    layout     <- cli.layout
    conf       <- Layer.readFuryConf(layout)
    layer      <- Layer.retrieve(conf)
    cli        <- cli.hint(ProjectArg, layer.projects)
    projectId  <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject <- ~projectId.flatMap(layer.projects.findBy(_).toOption)
    cli        <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    moduleId   <- cli.preview(ModuleArg)(optProject.flatMap(_.main))
    optModule   = for(project <- optProject; module <- project.modules.findBy(moduleId).toOption) yield module
    cli        <- cli.hint(BinaryArg, optModule.to[List].flatMap(_.binaries.map(_.id)))
    call       <- cli.call()
    binaryArg  <- call(BinaryArg)
    project    <- optProject.asTry
    module     <- optModule.asTry
    deletion   <- module.binaries.findBy(binaryArg)
    layer      <- ~Layer(_.projects(project.id).modules(module.id).binaries).modify(layer)(_ - deletion)
    _          <- Layer.commit(layer, conf, layout)
    _          <- ~Build.asyncBuild(layer, module.ref(project), layout)
  } yield log.await()

  def add: Try[ExitStatus] = for {
    layout     <- cli.layout
    conf       <- Layer.readFuryConf(layout)
    layer      <- Layer.retrieve(conf)
    binSpecs   <- ~cli.peek(PartialBinSpecArg).to[Set].flatMap(MavenCentral.search(_).toOption.to[Set].flatten)
    cli        <- cli.hint(BinSpecArg, binSpecs)
    cli        <- cli.hint(ProjectArg, layer.projects)
    projectId  <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject <- ~projectId.flatMap(layer.projects.findBy(_).toOption)
    cli        <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    moduleId   <- cli.preview(ModuleArg)(optProject.flatMap(_.main))
    cli        <- cli.hint(BinaryNameArg)
    cli        <- cli.hint(BinaryRepoArg, List(RepoId("central")))
    call       <- cli.call()
    project    <- optProject.asTry
    module     <- project.modules.findBy(moduleId)
    binSpecArg <- call(BinSpecArg)
    binName    <- ~call(BinaryNameArg).toOption
    repoId     <- ~call(BinaryRepoArg).getOrElse(BinRepoId.Central)
    binary     <- Binary(binName, repoId, binSpecArg)
    _          <- module.binaries.unique(binary.id)
    layer      <- ~Layer(_.projects(project.id).modules(module.id).binaries).modify(layer)(_ + binary)
    _          <- Layer.commit(layer, conf, layout)
    _          <- ~Build.asyncBuild(layer, module.ref(project), layout)
  } yield log.await()
}