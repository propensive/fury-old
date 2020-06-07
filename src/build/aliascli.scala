/*

    Fury, version 0.16.1. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury

import fury.core._, fury.model._, fury.text._, Args._

import scala.util._

import java.{text => jt}

case class AliasCli(cli: Cli)(implicit log: Log) {
  def list: Try[ExitStatus] = for {
    layout <- cli.layout
    conf   <- Layer.readFuryConf(layout)
    layer  <- Layer.retrieve(conf)
    cli    <- cli.hint(RawArg)
    table  <- ~Tables().aliases
    cli    <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
    cli    <- cli.hint(AliasArg, layer.aliases)
    call   <- cli.call()
    col    <- ~cli.peek(ColumnArg)
    alias  <- ~cli.peek(AliasArg)
    raw    <- ~call(RawArg).isSuccess
    rows   <- ~layer.aliases.to[List]
    table  <- ~Tables().show(table, cli.cols, rows, raw, col, alias, "alias")
    _      <- ~log.infoWhen(!raw)(conf.focus())
    _      <- ~log.rawln(table)
  } yield log.await()

  def remove: Try[ExitStatus] = for {
    layout     <- cli.layout
    conf       <- Layer.readFuryConf(layout)
    layer      <- Layer.retrieve(conf)
    cli        <- cli.hint(AliasArg, layer.aliases.map(_.id))
    call       <- cli.call()
    aliasArg   <- call(AliasArg)
    aliasToDel <- ~layer.aliases.find(_.id == aliasArg)
    layer      <- ~Layer(_.aliases).modify(layer)(_ -- aliasToDel)
    _          <- Layer.commit(layer, conf, layout)
  } yield log.await()

  def add: Try[ExitStatus] = for {
    layout           <- cli.layout
    conf             <- Layer.readFuryConf(layout)
    layer            <- Layer.retrieve(conf)
    cli              <- cli.hint(AliasArg)
    cli              <- cli.hint(DescriptionArg)
    cli              <- cli.hint(ProjectArg, layer.projects)
    optProjectId     <- ~cli.peek(ProjectArg)
    
    optProject       <- ~optProjectId.orElse(layer.main).flatMap { id =>
                            layer.projects.findBy(id).toOption }.to[List].headOption
    
    cli              <- cli.hint(ModuleArg, optProject.map(_.modules).getOrElse(Nil))
    call             <- cli.call()
    moduleArg        <- call(ModuleArg)
    project          <- optProject.asTry
    module           <- project.modules.findBy(moduleArg)
    moduleRef        <- ~module.ref(project)
    aliasArg         <- call(AliasArg)
    description      <- call(DescriptionArg)
    alias            <- ~Alias(aliasArg, description, moduleRef, call.suffix)
    layer            <- ~Layer(_.aliases).modify(layer)(_ + alias)
    _                <- Layer.commit(layer, conf, layout)
  } yield log.await()
}
