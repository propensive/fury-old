/*

    Fury, version 0.15.1. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury

import fury.strings._, fury.io._, fury.core._, fury.model._

import guillotine._
import mercator._
import Args._

import scala.collection.immutable.SortedSet
import scala.util._

case class ModuleCli(cli: Cli)(implicit log: Log) {
  def select: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    cli          <- cli.hint(ProjectArg, layer.projects)
    optProjectId <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject   <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
    cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    call         <- cli.call()
    project      <- optProject.asTry
    moduleId     <- ~call(ModuleArg).toOption
    moduleId     <- moduleId.asTry
    _            <- project(moduleId)
    layer        <- ~(Layer(_.projects(project.id).main)(layer) = Some(moduleId))
    _            <- Layer.commit(layer, conf, layout)
  } yield log.await()

  def list: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    cli          <- cli.hint(ProjectArg, layer.projects)
    optProjectId <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject   <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
    project      <- optProject.asTry
    cli          <- cli.hint(ModuleArg, project.modules.map(_.id))
    universe     <- layer.hierarchy().flatMap(_.universe)
    table        <- ~Tables().modules(project.id, project.main, universe)
    cli          <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
    cli          <- cli.hint(RawArg)
    call         <- cli.call()
    moduleId     <- ~cli.peek(ModuleArg)
    col          <- ~cli.peek(ColumnArg)
    raw          <- ~call(RawArg).isSuccess
    rows         <- ~project.modules.to[List]
    table        <- ~Tables().show(table, cli.cols, rows, raw, col, moduleId, "module")
    _            <- ~log.infoWhen(!raw)(conf.focus(project.id))
    _            <- ~log.rawln(table)
  } yield log.await()

  def add: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    cli          <- cli.hint(ProjectArg, layer.projects)
    optProjectId <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject   <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
    cli            <- cli.hint(ModuleNameArg)
    cli            <- cli.hint(HiddenArg, List("on", "off"))
    cli            <- cli.hint(CompilerArg, ModuleRef.JavaRef :: layer.compilerRefs(layout))
    cli            <- cli.hint(KindArg, Kind.all)
    optKind        <- ~cli.peek(KindArg)

    cli            <- optKind match {
                        case Some(Application) =>
                          for (cli <- cli.hint(MainArg)) yield cli
                        case Some(Plugin) =>
                          for(cli <- cli.hint(MainArg); cli <- cli.hint(PluginArg)) yield cli
                        case None | Some(Benchmarks | Library | Compiler) =>
                          ~cli
                      }

    call           <- cli.call()
    project        <- optProject.asTry
    moduleArg      <- call(ModuleNameArg)
    moduleId       <- project.modules.unique(moduleArg)
    compilerId     <- ~call(CompilerArg).toOption
    compilerRef    <- compilerId.map(resolveToCompiler(layer, optProject, layout, _))
                          .orElse(project.compiler.map(~_)).getOrElse(~ModuleRef.JavaRef)
    module         = Module(moduleId, compiler = compilerRef)

    module         <- ~call(KindArg).toOption.map { k => module.copy(kind = k) }.getOrElse(module)
    module         <- ~call(HiddenArg).toOption.map { h => module.copy(hidden = h) }.getOrElse(module)
    
    module         <- ~call(MainArg).toOption.fold(module) { m => module.copy(main = if(m.key.isEmpty) None else
                          Some(m)) }

    module         <- ~call(PluginArg).toOption.fold(module) { p => module.copy(plugin = if(p.key.isEmpty) None
                          else Some(p)) }

    layer          <- ~Layer(_.projects(project.id).modules).modify(layer)(_ + module)
    layer          <- ~(Layer(_.projects(project.id).main)(layer) = Some(module.id))

    layer          <- if(project.compiler.isEmpty && compilerRef != ModuleRef.JavaRef) {
                        ~Layer(_.projects(project.id).compiler).modify(layer) { v =>
                          log.info(msg"Setting default compiler for project ${project.id} to ${compilerRef}")
                          Some(compilerRef)
                        }
                      } else Try(layer)

    _              <- Layer.commit(layer, conf, layout)
    _              <- ~Compilation.asyncCompilation(layer, module.ref(project), layout)
    _              <- ~log.info(msg"Set current module to ${module.id}")
  } yield log.await()

  private def resolveToCompiler(layer: Layer, optProject: Option[Project], layout: Layout, reference: String)
                               (implicit log: Log)
                               : Try[ModuleRef] = for {
    project            <- optProject.asTry
    moduleRef          <- ModuleRef.parse(project.id, reference, true).ascribe(InvalidValue(reference))
    availableCompilers  = layer.compilerRefs(layout) :+ ModuleRef.JavaRef
    _                  <- if(availableCompilers.contains(moduleRef)) ~() else Failure(UnknownModule(moduleRef))
  } yield moduleRef

  def remove: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    cli          <- cli.hint(ProjectArg, layer.projects)
    optProjectId <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject   <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
    cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    call         <- cli.call()
    moduleId     <- call(ModuleArg)
    project      <- optProject.asTry
    module       <- project.modules.findBy(moduleId)
    layer        <- ~Layer(_.projects(project.id).modules).modify(layer)(_.evict(module.id))
    layer        <- ~Layer(_.projects(project.id).main).modify(layer)(_.filterNot(_ == moduleId))
    _            <- Layer.commit(layer, conf, layout)
    _            <- ~Compilation.asyncCompilation(layer, module.ref(project), layout)
  } yield log.await()

  def update: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    cli          <- cli.hint(ProjectArg, layer.projects)
    optProjectId <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject   <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
    cli         <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    cli         <- cli.hint(HiddenArg, List("on", "off"))
    cli         <- cli.hint(CompilerArg, ModuleRef.JavaRef :: layer.compilerRefs(layout))
    cli         <- cli.hint(KindArg, Kind.all)
    optModuleId =  cli.preview(ModuleArg)(optProject.flatMap(_.main)).toOption

    optModule   <- Success { for {
                      project  <- optProject
                      moduleId <- optModuleId
                      module   <- project.modules.findBy(moduleId).toOption
                    } yield module }

    cli         <- cli.hint(ModuleNameArg, optModuleId.to[List])
    optKind     <- ~cli.peek(KindArg).orElse(optModule.map(_.kind))
    
    cli         <- optKind match {
                      case Some(Application) =>
                        for (cli <- cli.hint(MainArg)) yield cli
                      case Some(Plugin) =>
                        for (cli <- cli.hint(MainArg); cli <- cli.hint(PluginArg)) yield cli
                      case Some(Compiler) =>
                        for (cli <- cli.hint(BloopSpecArg)) yield cli
                      case None | Some(Library | Benchmarks) =>
                        ~cli
                    }

    call        <- cli.call()
    compilerId  <- ~call(CompilerArg).toOption
    project     <- optProject.asTry
    module      <- optModule.asTry
    compilerRef <- compilerId.toSeq.traverse(resolveToCompiler(layer, optProject, layout, _)).map(_.headOption)
    hidden      <- ~call(HiddenArg).toOption
    mainClass   <- ~cli.peek(MainArg)
    pluginName  <- ~cli.peek(PluginArg)
    newId       <- ~call(ModuleNameArg).toOption
    name        <- newId.to[List].map(project.modules.unique(_)).sequence.map(_.headOption)
    bloopSpec   <- cli.peek(BloopSpecArg).to[List].map(_.as[BloopSpec]).sequence.map(_.headOption)
    
    layer       <- ~optKind.fold(layer)(Layer(_.projects(project.id).modules(module.id).kind)(layer) = _)

    layer       <- ~compilerRef.fold(layer)(Layer(_.projects(project.id).modules(module.id).compiler)(layer) =
                       _)

    layer       <- ~hidden.fold(layer)(Layer(_.projects(project.id).modules(module.id).hidden)(layer) = _)
    
    layer       <- ~bloopSpec.map(Some(_)).fold(layer)(Layer(_.projects(project.id).modules(
                       module.id).bloopSpec)(layer) = _)

    layer       <- ~mainClass.map(Some(_)).fold(layer)(Layer(_.projects(project.id).modules(module.id).main)(
                       layer) = _)

    layer       <- ~pluginName.map(Some(_)).fold(layer)(Layer(_.projects(project.id).modules(module.id).plugin)(
                       layer) = _)

    layer       <- if(newId.isEmpty || project.main != Some(module.id)) ~layer
                   else ~(Layer(_.projects(project.id).main)(layer) = newId)

    layer       <- ~name.fold(layer)(Layer(_.projects(project.id).modules(module.id).id)(layer) = _)
    _           <- Layer.commit(layer, conf, layout)
    _           <- ~Compilation.asyncCompilation(layer, module.ref(project), layout)
  } yield log.await()
}

case class BinaryCli(cli: Cli)(implicit log: Log) {
  def list: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    cli          <- cli.hint(ProjectArg, layer.projects)
    optProjectId <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject   <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
    cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    moduleId     <- cli.preview(ModuleArg)(optProject.flatMap(_.main))

    optModule    =  (for {
      project  <- optProject
      module   <- project.modules.findBy(moduleId).toOption
    } yield module)

    cli         <- cli.hint(RawArg)
    table       <- ~Tables().binaries
    cli         <- cli.hint(BinaryArg, optModule.map(_.binaries).getOrElse(Nil).map(_.id))
    cli         <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
    call        <- cli.call()
    col         <- ~cli.peek(ColumnArg)
    binaryId    <- ~cli.peek(BinaryArg)
    raw         <- ~call(RawArg).isSuccess
    project     <- optProject.asTry
    module      <- optModule.asTry
    rows        <- ~module.allBinaries.to[List]
    table       <- ~Tables().show(table, cli.cols, rows, raw, col, binaryId, "binary")
    _           <- ~log.infoWhen(!raw)(conf.focus(project.id, module.id))
    _           <- ~log.rawln(table)
  } yield log.await()

  def update: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    cli          <- cli.hint(ProjectArg, layer.projects)
    optProjectId <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject   <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
    cli         <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    moduleId     <- cli.preview(ModuleArg)(optProject.flatMap(_.main))

    optModule    =  (for {
      project  <- optProject
      module   <- project.modules.findBy(moduleId).toOption
    } yield module)

    cli         <- cli.hint(BinaryArg, optModule.to[List].flatMap(_.binaries.map(_.id)))
    cli         <- cli.hint(VersionArg)
    call        <- cli.call()
    binaryArg   <- call(BinaryArg)
    versionArg  <- call(VersionArg)
    project     <- optProject.asTry
    module      <- optModule.asTry
    binary      <- module.binaries.findBy(binaryArg)
    layer       <- ~Layer(_.projects(project.id).modules(module.id).binaries).modify(layer)(_ - binary)
    _           <- Layer.commit(layer, conf, layout)
    _           <- ~Compilation.asyncCompilation(layer, module.ref(project), layout)
  } yield log.await()

  def remove: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    cli          <- cli.hint(ProjectArg, layer.projects)
    optProjectId <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject   <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
    cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    moduleId     <- cli.preview(ModuleArg)(optProject.flatMap(_.main))

    optModule    = (for {
                      project  <- optProject
                      module   <- project.modules.findBy(moduleId).toOption
                    } yield module)

    cli         <- cli.hint(BinaryArg, optModule.to[List].flatMap(_.binaries.map(_.id)))
    call        <- cli.call()
    binaryArg   <- call(BinaryArg)
    project     <- optProject.asTry
    module      <- optModule.asTry
    binaryToDel <- module.binaries.findBy(binaryArg)
    layer       <- ~Layer(_.projects(project.id).modules(module.id).binaries).modify(layer)(_ - binaryToDel)
    _           <- Layer.commit(layer, conf, layout)
    _           <- ~Compilation.asyncCompilation(layer, module.ref(project), layout)
  } yield log.await()

  def add: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    
    binSpecs     <- ~cli.peek(PartialBinSpecArg).to[Set].flatMap(
                        MavenCentral.search(_).toOption.to[Set].flatten)

    cli          <- cli.hint(BinSpecArg, binSpecs)
    cli          <- cli.hint(ProjectArg, layer.projects)
    optProjectId <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject   <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
    cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    moduleId     <- cli.preview(ModuleArg)(optProject.flatMap(_.main))
    cli          <- cli.hint(BinaryNameArg)
    cli          <- cli.hint(BinaryRepoArg, List(RepoId("central")))
    call         <- cli.call()
    project      <- optProject.asTry
    module       <- project.modules.findBy(moduleId)
    binSpecArg   <- call(BinSpecArg)
    binName      <- ~call(BinaryNameArg).toOption
    repoId       <- ~call(BinaryRepoArg).getOrElse(BinRepoId.Central)
    binary       <- Binary(binName, repoId, binSpecArg)
    _            <- module.binaries.unique(binary.id)
    layer        <- ~Layer(_.projects(project.id).modules(module.id).binaries).modify(layer)(_ + binary)
    _            <- Layer.commit(layer, conf, layout)
    _            <- ~Compilation.asyncCompilation(layer, module.ref(project), layout)
  } yield log.await()
}

case class OptionCli(cli: Cli)(implicit log: Log) {
  def list: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    cli          <- cli.hint(ProjectArg, layer.projects)
    optProjectId <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject   <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
    cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    moduleId     <- cli.preview(ModuleArg)(optProject.flatMap(_.main))

    optModule     = (for {
                      project <- optProject
                      module  <- project.modules.findBy(moduleId).toOption
                    } yield module)

    cli          <- cli.hint(RawArg)
    table        <- ~Tables().opts
    cli          <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
    cli          <- cli.hint(OptArg, optModule.map(_.opts).getOrElse(Nil))
    call         <- cli.call()
    opt          <- ~cli.peek(OptArg)
    col          <- ~cli.peek(ColumnArg)
    raw          <- ~call(RawArg).isSuccess
    project      <- optProject.asTry
    module       <- optModule.asTry
    compiler     <- ~module.compiler
    compilation  <- Compilation.syncCompilation(layer, module.ref(project), layout, true)
    rows         <- compilation.aggregatedOpts(module.ref(project), layout)
    showRows     <- ~rows.to[List].filter(_.compiler == compiler)
    _            <- ~log.infoWhen(!raw)(conf.focus(project.id, module.id))
    table        <- ~Tables().show(table, cli.cols, showRows, raw, col, opt, "param")
    _            <- ~log.rawln(table)
  } yield log.await()

  def remove: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    cli          <- cli.hint(ProjectArg, layer.projects)
    optProjectId <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject   <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
    cli         <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    moduleId     <- cli.preview(ModuleArg)(optProject.flatMap(_.main))

    optModule    =  (for {
      project  <- optProject
      module   <- project.modules.findBy(moduleId).toOption
    } yield module)

    cli      <- cli.hint(OptArg, optModule.to[List].flatMap(_.opts))
    cli      <- cli.hint(PersistentArg)
    call     <- cli.call()
    paramArg <- call(OptArg)
    persist  <- ~call(PersistentArg).isSuccess
    project  <- optProject.asTry
    module   <- optModule.asTry
    opt      <- ~module.opts.find(_.id == paramArg)

    base     <- ~Layer(_.projects(project.id).modules(module.id).opts).modify(layer)(_ + Opt(paramArg, persist,
                    true))
    
    layer    <- ~opt.fold(base) { optToDel =>
                    Layer(_.projects(project.id).modules(module.id).opts).modify(layer)(_ - optToDel) }

    _        <- Layer.commit(layer, conf, layout)
    _        <- ~Compilation.asyncCompilation(layer, module.ref(project), layout)
  } yield log.await()

  def define: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    cli          <- cli.hint(ProjectArg, layer.projects)
    optProjectId <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject   <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
    cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    moduleId     <- cli.preview(ModuleArg)(optProject.flatMap(_.main))

    optModule    =  (for {
                      project  <- optProject
                      module   <- project.modules.findBy(moduleId).toOption
                    } yield module)

    cli          <- cli.hint(OptArg)
    cli          <- cli.hint(DescriptionArg)
    cli          <- cli.hint(TransformArg)
    cli          <- cli.hint(PersistentArg)
    call         <- cli.call()
    option       <- call(OptArg)
    module       <- optModule.asTry
    project      <- optProject.asTry
    description  <- ~call(DescriptionArg).getOrElse("")
    persist      <- ~call(PersistentArg).isSuccess
    transform    <- ~call.suffix
    optDef       <- ~OptDef(option, description, transform, persist)
    layer        <- ~Layer(_.projects(project.id).modules(module.id).optDefs).modify(layer)(_ + optDef)
    _            <- Layer.commit(layer, conf, layout)
  } yield log.await()

  def undefine: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    cli          <- cli.hint(ProjectArg, layer.projects)
    optProjectId <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject   <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
    cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    moduleId     <- cli.preview(ModuleArg)(optProject.flatMap(_.main))

    optModule     = (for {
                      project  <- optProject
                      module   <- project.modules.findBy(moduleId).toOption
                    } yield module)

    cli         <- cli.hint(OptArg)
    call        <- cli.call()
    option      <- call(OptArg)
    module      <- optModule.asTry
    project     <- optProject.asTry
    optDef      <- module.optDefs.findBy(option)
    layer       <- ~Layer(_.projects(project.id).modules(module.id).optDefs).modify(layer)(_ - optDef)
    _           <- Layer.commit(layer, conf, layout)
  } yield log.await()

  def add: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    cli          <- cli.hint(ProjectArg, layer.projects)
    optProjectId <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject   <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
    cli         <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    moduleId     <- cli.preview(ModuleArg)(optProject.flatMap(_.main))

    optModule    =  (for {
      project  <- optProject
      module   <- project.modules.findBy(moduleId).toOption
    } yield module)

    optDefs  <- ~(for {
                  project     <- optProject
                  module      <- optModule
                  compilation <- Compilation.syncCompilation(layer, module.ref(project), layout, false).toOption
                  optDefs     <- compilation.aggregatedOptDefs(module.ref(project)).toOption
                } yield optDefs.map(_.value.id)).getOrElse(Set())
    
    cli      <- cli.hint(OptArg, optDefs)
    cli      <- cli.hint(PersistentArg)
    call     <- cli.call()
    project  <- optProject.asTry
    module   <- optModule.asTry
    paramArg <- call(OptArg)
    persist  <- ~call(PersistentArg).isSuccess
    param    <- ~Opt(paramArg, persist, remove = false)
    layer    <- ~Layer(_.projects(project.id).modules(module.id).opts).modify(layer)(_ + param)
    _        <- Layer.commit(layer, conf, layout)
    _        <- ~Compilation.asyncCompilation(layer, module.ref(project), layout)
  } yield log.await()
}
