/*

    Fury, version 0.33.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury

import fury.model._, fury.text._, fury.core._, fury.io._

import scala.util._

import Args._

case class OptionCli(cli: Cli) {
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
    table      <- ~Tables().opts
    cli        <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
    cli        <- cli.hint(OptArg, optModule.map(_.opts).getOrElse(Nil))
    call       <- cli.call()
    opt        <- ~cli.peek(OptArg)
    col        <- ~cli.peek(ColumnArg)
    raw        <- ~call(RawArg).isSuccess
    project    <- optProject.asTry
    module     <- optModule.asTry
    compiler   <- ~module.compiler
    build      <- Build.syncBuild(layer, module.ref(project), layout, true, cli.job)
    target     <- build(module.ref(project))
    rows       <- new build.TargetExtras(target).aggregatedOpts
    showRows   <- ~rows.to[List].filter(_.compiler == compiler)
    _          <- ~(if(!raw) log.info(conf.focus(project.id, module.id)))
    table      <- ~Tables().show(table, cli.cols, showRows, raw, col, opt, "param")
    _          <- ~log.raw(table+"\n")
  } yield cli.job.await()

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
    cli        <- cli.hint(OptArg, optModule.to[List].flatMap(_.opts))
    cli        <- cli.hint(PersistentArg)
    call       <- cli.call()
    paramArg   <- call(OptArg)
    persist    <- ~call(PersistentArg).isSuccess
    project    <- optProject.asTry
    module     <- optModule.asTry
    opt        <- ~module.opts.find(_.id == paramArg)

    base       <- ~Layer(_.projects(project.id).modules(module.id).opts).modify(layer)(_ + Opt(paramArg,
                      persist, true))
    
    layer      <- ~opt.fold(base) { deletion =>
                      Layer(_.projects(project.id).modules(module.id).opts).modify(layer)(_ - deletion) }

    _          <- Layer.commit(layer, conf, layout)
    _          <- ~Build.asyncBuild(layer, module.ref(project), layout, cli.job)
  } yield cli.job.await()

  def define: Try[ExitStatus] = for {
    layout     <- cli.layout
    conf       <- Layer.readFuryConf(layout)
    layer      <- Layer.retrieve(conf)
    cli        <- cli.hint(ProjectArg, layer.projects)
    projectId  <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject <- ~projectId.flatMap(layer.projects.findBy(_).toOption)
    cli        <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    moduleId   <- cli.preview(ModuleArg)(optProject.flatMap(_.main))
    optModule   = for(project <- optProject; module <- project.modules.findBy(moduleId).toOption) yield module
    cli        <- cli.hint(OptArg)
    cli        <- cli.hint(DescriptionArg)
    cli        <- cli.hint(TransformArg)
    cli        <- cli.hint(PersistentArg)
    call       <- cli.call()
    option     <- call(OptArg)
    module     <- optModule.asTry
    project    <- optProject.asTry
    desc       <- ~call(DescriptionArg).getOrElse("")
    persist    <- ~call(PersistentArg).isSuccess
    transform  <- ~call.suffix
    optDef     <- ~OptDef(option, desc, transform, persist)
    layer      <- ~Layer(_.projects(project.id).modules(module.id).optDefs).modify(layer)(_ + optDef)
    _          <- Layer.commit(layer, conf, layout)
  } yield cli.job.await()

  def undefine: Try[ExitStatus] = for {
    layout     <- cli.layout
    conf       <- Layer.readFuryConf(layout)
    layer      <- Layer.retrieve(conf)
    cli        <- cli.hint(ProjectArg, layer.projects)
    projectId  <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject <- ~projectId.flatMap(layer.projects.findBy(_).toOption)
    cli        <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    moduleId   <- cli.preview(ModuleArg)(optProject.flatMap(_.main))
    optModule   = for(project <- optProject; module <- project.modules.findBy(moduleId).toOption) yield module
    cli        <- cli.hint(OptArg)
    call       <- cli.call()
    option     <- call(OptArg)
    module     <- optModule.asTry
    project    <- optProject.asTry
    optDef     <- module.optDefs.findBy(option)
    layer      <- ~Layer(_.projects(project.id).modules(module.id).optDefs).modify(layer)(_ - optDef)
    _          <- Layer.commit(layer, conf, layout)
  } yield cli.job.await()

  def add: Try[ExitStatus] = for {
    layout     <- cli.layout
    conf       <- Layer.readFuryConf(layout)
    layer      <- Layer.retrieve(conf)
    cli        <- cli.hint(ProjectArg, layer.projects)
    projectId  <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject <- ~projectId.flatMap(layer.projects.findBy(_).toOption)
    cli        <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    moduleId   <- cli.preview(ModuleArg)(optProject.flatMap(_.main))
    optModule   = for(project <- optProject; module <- project.modules.findBy(moduleId).toOption) yield module

    optDefs    <- ~ { for {
                    project <- optProject
                    module  <- optModule
                    build   <- Build.syncBuild(layer, module.ref(project), layout, false, cli.job).toOption
                    target  <- build(module.ref(project)).toOption
                    optDefs <- new build.TargetExtras(target).aggregatedOptDefs.toOption
                  } yield optDefs.map(_.value.id) }.getOrElse(Set())
    
    cli        <- cli.hint(OptArg, optDefs)
    cli        <- cli.hint(PersistentArg)
    call       <- cli.call()
    project    <- optProject.asTry
    module     <- optModule.asTry
    paramArg   <- call(OptArg)
    persist    <- ~call(PersistentArg).isSuccess
    param      <- ~Opt(paramArg, persist, remove = false)
    layer      <- ~Layer(_.projects(project.id).modules(module.id).opts).modify(layer)(_ + param)
    _          <- Layer.commit(layer, conf, layout)
    _          <- ~Build.asyncBuild(layer, module.ref(project), layout, cli.job)
  } yield cli.job.await()
}