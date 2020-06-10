/*

    Fury, version 0.17.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

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

import mercator._

import scala.collection.immutable.SortedSet
import scala.util._

import Args._

case class ModuleCli(cli: Cli)(implicit log: Log) {
  
  private def parseKind(kindName: Kind.Id,
                        main: Option[ClassRef],
                        repl: Option[ClassRef],
                        timeout: Option[Int],
                        spec: Option[BloopSpec],
                        plugin: Option[PluginId])
                       : Try[Kind] = kindName match {
    case Lib      => Success(Lib())
    case App      => main.map(App(_, timeout.getOrElse(0))).ascribe(MissingParam(MainArg))
    case Bench    => main.map(Bench(_)).ascribe(MissingParam(MainArg))

    case Compiler => spec.map(Compiler(_, repl.getOrElse(ClassRef("scala.tools.nsc.MainGenericRunner"))))
                         .ascribe(MissingParam(SpecArg))

    case Plugin   => for(m <- main.ascribe(MissingParam(MainArg)); p <- plugin.ascribe(MissingParam(PluginArg)))
                     yield Plugin(p, m)
  }
  
  private def resolveToCompiler(layer: Layer, optProject: Option[Project], layout: Layout, ref: CompilerRef)
                               (implicit log: Log)
                               : Try[CompilerRef] =
    ref match {
      case Javac(n) =>
        Success(ref)
      case ref@BspCompiler(_) => for {
        project   <- optProject.asTry
        available  = layer.compilerRefs(layout)
        _         <- if(available.contains(ref.ref)) Success(ref) else Failure(UnknownModule(ref.ref))
      } yield ref
    }
  
  private def hintKindParams(cli: Cli, mains: Set[ClassRef], kindName: Option[Kind.Id]) = kindName.map {
    case Lib      => ~cli
    case App      => cli.hint(MainArg, mains).flatMap(_.hint(TimeoutArg, List("0")))
    case Compiler => cli.hint(SpecArg).flatMap(_.hint(ReplArg, mains))
    case Bench    => cli.hint(MainArg, mains)
    case Plugin   => cli.hint(MainArg, mains).flatMap(_.hint(PluginArg))
  }.getOrElse(~cli)

  def select: Try[ExitStatus] = for {
    layout     <- cli.layout
    conf       <- Layer.readFuryConf(layout)
    layer      <- Layer.retrieve(conf)
    cli        <- cli.hint(ProjectArg, layer.projects)
    projectId  <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject <- ~projectId.flatMap(layer.projects.findBy(_).toOption)
    cli        <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    call       <- cli.call()
    project    <- optProject.asTry
    moduleId   <- ~call(ModuleArg).toOption
    moduleId   <- moduleId.asTry
    _          <- project(moduleId)
    layer      <- ~(Layer(_.projects(project.id).main)(layer) = Some(moduleId))
    _          <- Layer.commit(layer, conf, layout)
  } yield log.await()

  def list: Try[ExitStatus] = for {
    layout     <- cli.layout
    conf       <- Layer.readFuryConf(layout)
    layer      <- Layer.retrieve(conf)
    cli        <- cli.hint(ProjectArg, layer.projects)
    projectId  <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject <- ~projectId.flatMap(layer.projects.findBy(_).toOption)
    project    <- optProject.asTry
    cli        <- cli.hint(ModuleArg, project.modules.map(_.id))
    universe   <- layer.hierarchy(ImportPath.Root).flatMap(_.universe)
    table      <- ~Tables().modules(project.id, project.main, universe)
    cli        <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
    cli        <- cli.hint(RawArg)
    call       <- cli.call()
    moduleId   <- ~cli.peek(ModuleArg)
    col        <- ~cli.peek(ColumnArg)
    raw        <- ~call(RawArg).isSuccess
    rows       <- ~project.modules.to[List]
    table      <- ~Tables().show(table, cli.cols, rows, raw, col, moduleId, "module")
    _          <- ~log.infoWhen(!raw)(conf.focus(project.id))
    _          <- ~log.rawln(table)
  } yield log.await()

  def add: Try[ExitStatus] = for {
    layout      <- cli.layout
    conf        <- Layer.readFuryConf(layout)
    layer       <- Layer.retrieve(conf)
    cli         <- cli.hint(ProjectArg, layer.projects)
    projectId   <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject  <- ~projectId.flatMap(layer.projects.findBy(_).toOption)
    cli         <- cli.hint(KindArg, Kind.ids)
    kindName    <- ~cli.peek(KindArg)
    cli         <- hintKindParams(cli, Set(), kindName)
    cli         <- cli.hint(ModuleNameArg)
    cli         <- cli.hint(HiddenArg, List("on", "off"))
    cli         <- cli.hint(CompilerArg, Javac.Versions ++ layer.compilerRefs(layout).map(BspCompiler(_)))
    call        <- cli.call()

    kind        <- parseKind(kindName.getOrElse(Lib), cli.peek(MainArg), cli.peek(ReplArg),
                       cli.peek(TimeoutArg), cli.peek(SpecArg), cli.peek(PluginArg))
    
    project     <- optProject.asTry
    moduleArg   <- call(ModuleNameArg)
    moduleId    <- project.modules.unique(moduleArg)
    compilerRef <- ~call(CompilerArg).toOption
    _           <- ~compilerRef.map(resolveToCompiler(layer, optProject, layout, _))
    defaultComp <- ~layer.compilerRefs(layout).map(BspCompiler(_)).headOption.getOrElse(Javac(8))
    compiler    <- compilerRef.toSeq.traverse(resolveToCompiler(layer, optProject, layout, _)).map(_.headOption)
    module      = Module(moduleId, compiler = compiler.getOrElse(defaultComp), kind = kind)
    module      <- ~call(HiddenArg).toOption.map { h => module.copy(hidden = h) }.getOrElse(module)
    layer       <- ~Layer(_.projects(project.id).modules).modify(layer)(_ + module)
    layer       <- ~(Layer(_.projects(project.id).main)(layer) = Some(module.id))

    layer       <- if(project.compiler.isEmpty && compilerRef.isDefined) {
                     ~Layer(_.projects(project.id).compiler).modify(layer) { v =>
                       log.info(msg"Setting default compiler for ${project.id} to ${module.compiler}")
                       Some(module.compiler)
                     }
                   } else Try(layer)

    _           <- Layer.commit(layer, conf, layout)
    _           <- ~Build.asyncBuild(layer, module.ref(project), layout)
    _           <- ~log.info(msg"Set current module to ${module.id}")
  } yield log.await()

  def remove: Try[ExitStatus] = for {
    layout     <- cli.layout
    conf       <- Layer.readFuryConf(layout)
    layer      <- Layer.retrieve(conf)
    cli        <- cli.hint(ProjectArg, layer.projects)
    projectId  <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject <- ~projectId.flatMap(layer.projects.findBy(_).toOption)
    cli        <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    call       <- cli.call()
    moduleId   <- call(ModuleArg)
    project    <- optProject.asTry
    module     <- project.modules.findBy(moduleId)
    layer      <- ~Layer(_.projects(project.id).modules).modify(layer)(_.evict(module.id))
    layer      <- ~Layer(_.projects(project.id).main).modify(layer)(_.filterNot(_ == moduleId))
    _          <- Layer.commit(layer, conf, layout)
    _          <- ~Build.asyncBuild(layer, module.ref(project), layout)
  } yield log.await()

  def update: Try[ExitStatus] = for {
    layout      <- cli.layout
    conf        <- Layer.readFuryConf(layout)
    layer       <- Layer.retrieve(conf)
    cli         <- cli.hint(ProjectArg, layer.projects)
    projectId   <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject  <- ~projectId.flatMap(layer.projects.findBy(_).toOption)
    cli         <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    cli         <- cli.hint(KindArg, Kind.ids)
    optModuleId  = cli.preview(ModuleArg)(optProject.flatMap(_.main)).toOption

    optModule   <- Success { for {
                     project  <- optProject
                     moduleId <- optModuleId
                     module   <- project.modules.findBy(moduleId).toOption
                   } yield module }
    kindName    <- ~cli.peek(KindArg).orElse(optModule.map(_.kind.name))
    targetId    <- ~projectId.flatMap { p => optModuleId.map(m => ModuleRef(p, m, false, false)) }
    mainClasses <- ~targetId.map { t => Asm.executableClasses(layout.classesDir(t)) }.to[Set].flatten
    cli         <- hintKindParams(cli, mainClasses, kindName)
    cli         <- cli.hint(HiddenArg, List("on", "off"))
    cli         <- cli.hint(ModuleNameArg, optModuleId.to[List])
    cli         <- cli.hint(CompilerArg, Javac.Versions ++ layer.compilerRefs(layout).map(BspCompiler(_)))
    call        <- cli.call()
    compilerId  <- ~call(CompilerArg).toOption
    project     <- optProject.asTry
    module      <- optModule.asTry
    
    mainArg     <- ~cli.peek(MainArg).orElse(module.kind.as[Plugin].map(_.main)).orElse(
                       module.kind.as[App].map(_.main)).orElse(module.kind.as[Bench].map(_.main))

    specArg     <- ~cli.peek(SpecArg).orElse(module.kind.as[Compiler].map(_.spec))
    replArg     <- ~cli.peek(ReplArg).orElse(module.kind.as[Compiler].map(_.repl))
    timeoutArg  <- ~cli.peek(TimeoutArg).orElse(module.kind.as[App].map(_.timeout))
    pluginArg   <- ~cli.peek(PluginArg).orElse(module.kind.as[Plugin].map(_.id))
    
    kind        <- kindName.to[List].traverse(parseKind(_, mainArg, replArg, timeoutArg, specArg,
                       pluginArg)).map(_.headOption)

    compiler    <- compilerId.toSeq.traverse(resolveToCompiler(layer, optProject, layout, _)).map(_.headOption)
    hidden      <- ~call(HiddenArg).toOption
    newId       <- ~call(ModuleNameArg).toOption
    name        <- newId.to[List].map(project.modules.unique(_)).sequence.map(_.headOption)
    layer       <- ~kind.fold(layer)(Layer(_.projects(project.id).modules(module.id).kind)(layer) = _)
    layer       <- ~hidden.fold(layer)(Layer(_.projects(project.id).modules(module.id).hidden)(layer) = _)
    layer       <- ~compiler.fold(layer)(Layer(_.projects(project.id).modules(module.id).compiler)(layer) = _)

    layer       <- if(newId.isEmpty || project.main != Some(module.id)) ~layer
                   else ~(Layer(_.projects(project.id).main)(layer) = newId)

    layer       <- ~name.fold(layer)(Layer(_.projects(project.id).modules(module.id).id)(layer) = _)
    _           <- Layer.commit(layer, conf, layout)
    _           <- ~Build.asyncBuild(layer, module.ref(project), layout)
  } yield log.await()
}