/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.7.14. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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
import mercator._
import Args._

import scala.collection.immutable.SortedSet
import scala.util._

import Lenses.on

object ModuleCli {

  case class Context(override val cli: Cli[CliParam[_]],
                     override val layout: Layout,
                     override val layer: Layer,
                     optSchema: Option[Schema],
                     optProject: Option[Project])
             extends MenuContext(cli, layout, layer, optSchema.map(_.id)) {

    def defaultSchemaId: SchemaId  = optSchemaId.getOrElse(layer.main)
    def defaultSchema: Try[Schema] = layer.schemas.findBy(defaultSchemaId)
  }

  def context(cli: Cli[CliParam[_]])(implicit log: Log) = for {
    layout       <- cli.layout
    layer        <- Layer.read(layout)
    cli          <- cli.hint(SchemaArg, layer.schemas)
    schemaArg    <- ~cli.peek(SchemaArg)
    schema       <- ~layer.schemas.findBy(schemaArg.getOrElse(layer.main)).toOption
    cli          <- cli.hint(ProjectArg, schema.map(_.projects).getOrElse(Nil))
    optProjectId <- ~schema.flatMap { s => cli.peek(ProjectArg).orElse(s.main) }
    optProject   <- ~schema.flatMap { s => optProjectId.flatMap(s.projects.findBy(_).toOption) }
  } yield Context(cli, layout, layer, schema, optProject)

  def select(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli      <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
      cli      <- cli.hint(ForceArg)
      call     <- cli.call()
      project  <- optProject.ascribe(UnspecifiedProject())
      moduleId <- ~call(ModuleArg).toOption
      moduleId <- moduleId.ascribe(UnspecifiedModule())
      _        <- project(moduleId)
      force    <- ~call(ForceArg).isSuccess
      focus    <- ~Lenses.focus(optSchemaId, force)
      layer    <- focus(layer, _.lens(_.projects(on(project.id)).main)) = Some(Some(moduleId))
      _        <- ~Layer.save(layer, layout)
    } yield log.await()
  }

  def list(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      project <- optProject.ascribe(UnspecifiedProject())
      cli     <- cli.hint(RawArg)
      call    <- cli.call()
      raw     <- ~call(RawArg).isSuccess
      rows    <- ~project.modules.to[List]

      table   <- ~Tables().show(Tables().modules(project.id, project.main), cli.cols, rows,
                     raw)(_.id)

      schema  <- defaultSchema

      _       <- ~(if(!raw) log.info(Tables().contextString(layer, layer.showSchema, schema,
                     project)))

      _       <- ~log.rawln(table.mkString("\n"))
    } yield log.await()
  }

  def add(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    val defaultCompiler = ModuleRef.JavaRef
    for {
      cli            <- cli.hint(ModuleNameArg)
      cli            <- cli.hint(HiddenArg, List("on", "off"))

      cli            <- cli.hint(CompilerArg, ModuleRef.JavaRef :: defaultSchema.toOption.to[List].flatMap(
                            _.compilerRefs(layout, true)))

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
      project        <- optProject.ascribe(UnspecifiedProject())
      moduleArg      <- call(ModuleNameArg)
      moduleId       <- project.unused(moduleArg)
      compilerId     <- ~call(CompilerArg).toOption
      compilerRef    <- compilerId.map(resolveToCompiler(ctx, _))
                            .orElse(project.compiler.map(~_)).getOrElse(~defaultCompiler)
      module         = Module(moduleId, compiler = compilerRef)

      module         <- ~call(KindArg).toOption.map { k => module.copy(kind = k) }.getOrElse(module)
      module         <- ~call(HiddenArg).toOption.map { h => module.copy(hidden = h) }.getOrElse(module)
      
      module         <- ~call(MainArg).toOption.map { m => module.copy(main = if(m == "") None else Some(m))
                            }.getOrElse(module)

      module         <- ~call(PluginArg).toOption.map { p => module.copy(plugin = if(p == "") None else
                            Some(p)) }.getOrElse(module)

      layer          <- Lenses.updateSchemas(optSchemaId, layer, true)(Lenses.layer.modules(_, project.id)) {
                            (lens, ws) => lens.modify(layer)((_: SortedSet[Module]) + module) }

      layer          <- Lenses.updateSchemas(optSchemaId, layer, true)(Lenses.layer.mainModule(_, project.id)) {
                            (lens, ws) => lens(ws) = Some(module.id) }

      layer          <- if(project.compiler.isEmpty && compilerRef != defaultCompiler) Lenses.updateSchemas(optSchemaId, layer, true)(
                            Lenses.layer.compiler(_, project.id)) { (lens, ws) =>
                            log.info(msg"Setting default compiler for project ${project.id} to ${compilerRef}")
                            lens(ws) = Some(compilerRef)
                        } else Try(layer)

      _              <- ~Layer.save(layer, layout)
      schema         <- defaultSchema

      _              <- ~Compilation.asyncCompilation(schema, module.ref(project), layout,
                            false)

      _              <- ~log.info(msg"Set current module to ${module.id}")
    } yield log.await()
  }

  private def resolveToCompiler(ctx: Context, reference: String)(implicit log: Log): Try[ModuleRef] = for {
    project  <- ctx.optProject.ascribe(UnspecifiedProject())
    moduleRef      <- ModuleRef.parse(project.id, reference, true)
    availableCompilers = ctx.layer.schemas.flatMap(_.compilerRefs(ctx.layout, https = true))
    _      <-   if(availableCompilers.contains(moduleRef)) ~() else Failure(UnknownModule(moduleRef))
  } yield moduleRef

  def remove(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli      <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))

      cli      <- cli.hint(CompilerArg, defaultSchema.toOption.to[List].flatMap(_.compilerRefs(
                      layout, true)))

      cli      <- cli.hint(ForceArg)
      call     <- cli.call()
      force    <- ~call(ForceArg).isSuccess
      moduleId <- call(ModuleArg)
      project  <- optProject.ascribe(UnspecifiedProject())
      module   <- project.modules.findBy(moduleId)

      layer    <- Lenses.updateSchemas(optSchemaId, layer, force)(Lenses.layer.modules(_, project.id)) {
                      (lens, ws) => lens.modify(ws)((_: SortedSet[Module]).filterNot(_.id == module.id)) }

      layer    <- Lenses.updateSchemas(optSchemaId, layer, force)(Lenses.layer.mainModule(_, project.id)) {
                      (lens, ws) => if(lens(ws) == Some(moduleId)) lens(ws) = None else ws }

      _        <- ~Layer.save(layer, layout)
      schema   <- defaultSchema
      
      _        <- ~Compilation.asyncCompilation(schema, module.ref(project), layout,
                      false)

    } yield log.await()
  }

  def update(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli         <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
      cli         <- cli.hint(HiddenArg, List("on", "off"))
      
      cli         <- cli.hint(CompilerArg, ModuleRef.JavaRef :: defaultSchema.toOption.to[List].flatMap(
                         _.compilerRefs(layout, true)))
      
      cli         <- cli.hint(KindArg, Kind.all)
      optModuleId <- ~cli.peek(ModuleArg).orElse(optProject.flatMap(_.main))

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

      cli         <- cli.hint(ForceArg)
      call        <- cli.call()
      compilerId  <- ~call(CompilerArg).toOption
      project     <- optProject.ascribe(UnspecifiedProject())
      module      <- optModule.ascribe(UnspecifiedModule())
      compilerRef <- compilerId.toSeq.traverse(resolveToCompiler(ctx, _)).map(_.headOption)
      hidden      <- ~call(HiddenArg).toOption
      mainClass   <- ~call(MainArg).toOption
      pluginName  <- ~call(PluginArg).toOption
      nameArg     <- ~call(ModuleNameArg).toOption
      name        <- nameArg.to[List].map(project.unused(_)).sequence.map(_.headOption)
      bloopSpec   <- call(BloopSpecArg).toOption.to[List].map(BloopSpec.parse(_)).sequence.map(_.headOption)
      force       <- ~call(ForceArg).isSuccess
      focus       <- ~Lenses.focus(optSchemaId, force)
      layer       <- focus(layer, _.lens(_.projects(on(project.id)).modules(on(module.id)).kind)) = optKind

      layer       <- focus(layer, _.lens(_.projects(on(project.id)).modules(on(module.id)).compiler)) =
                         compilerRef

      layer       <- focus(layer, _.lens(_.projects(on(project.id)).modules(on(module.id)).hidden)) =
                         hidden

      layer       <- focus(layer, _.lens(_.projects(on(project.id)).modules(on(module.id)).bloopSpec)) =
                         bloopSpec.map(Some(_))

      layer       <- focus(layer, _.lens(_.projects(on(project.id)).modules(on(module.id)).main)) =
                         mainClass.map(Some(_))

      layer       <- focus(layer, _.lens(_.projects(on(project.id)).modules(on(module.id)).plugin)) =
                         pluginName.map(Some(_))

      layer       <- focus(layer, _.lens(_.projects(on(project.id)).modules(on(module.id)).id)) = name
      _           <- ~Layer.save(layer, layout)
      schema      <- defaultSchema

      _           <- ~Compilation.asyncCompilation(schema, module.ref(project), layout,
                         false)

    } yield log.await()
  }
}

object BinaryCli {

  case class BinariesCtx(moduleCtx: ModuleCli.Context, optModule: Option[Module])

  def context(cli: Cli[CliParam[_]])(implicit log: Log) = for {
    ctx         <- ModuleCli.context(cli)
    cli         <- cli.hint(ModuleArg, ctx.optProject.to[List].flatMap(_.modules))
    optModuleId <- ~cli.peek(ModuleArg).orElse(ctx.optProject.flatMap(_.main))

    optModule   <- Success { for {
                      project  <- ctx.optProject
                      moduleId <- optModuleId
                      module   <- project.modules.findBy(moduleId).toOption
                    } yield module }

  } yield BinariesCtx(ctx.copy(cli = cli), optModule)

  def list(ctx: BinariesCtx)(implicit log: Log): Try[ExitStatus] = {
    import ctx._, moduleCtx._
    for {
      cli     <- cli.hint(RawArg)
      call    <- cli.call()
      raw     <- ~call(RawArg).isSuccess
      project <- optProject.ascribe(UnspecifiedProject())
      module  <- optModule.ascribe(UnspecifiedModule())
      rows    <- ~module.allBinaries.to[List]
      schema  <- defaultSchema
      table   <- ~Tables().show(Tables().binaries, cli.cols, rows, raw)(identity)

      _       <- ~(if(!raw) log.info(Tables().contextString(layer, layer.showSchema, schema,
                     project, module)))

      _       <- ~log.rawln(table.mkString("\n"))
    } yield log.await()
  }

  def remove(ctx: BinariesCtx)(implicit log: Log): Try[ExitStatus] = {
    import ctx._, moduleCtx._
    for {
      cli         <- cli.hint(BinaryArg, optModule.to[List].flatMap(_.binaries))
      cli         <- cli.hint(ForceArg)
      call        <- cli.call()
      binaryArg   <- call(BinaryArg)
      project     <- optProject.ascribe(UnspecifiedProject())
      module      <- optModule.ascribe(UnspecifiedModule())
      binaryToDel <- Binary.filterByPartialId(module.binaries, binaryArg) match {
                       case bin :: Nil => Success(bin)
                       case Nil        => Failure(UnspecifiedBinary(module.binaries.map(_.spec).toList))
                       case bins       => Failure(UnspecifiedBinary(bins.map(_.spec)))
                     }
      force       <- ~call(ForceArg).isSuccess
      
      layer       <- Lenses.updateSchemas(optSchemaId, layer, force)(Lenses.layer.binaries(_, project.id,
                         module.id))(_(_) -= binaryToDel)

      _           <- ~Layer.save(layer, layout)
      schema      <- defaultSchema

      _           <- ~Compilation.asyncCompilation(schema, module.ref(project), layout,
                         false)

    } yield log.await()
  }

  def add(ctx: BinariesCtx)(implicit log: Log): Try[ExitStatus] = {
    import ctx._, moduleCtx._
    for {
      cli       <- cli.hint(BinaryArg)
      cli       <- cli.hint(BinaryRepoArg, List(RepoId("central")))
      call      <- cli.call()
      project   <- optProject.ascribe(UnspecifiedProject())
      module    <- optModule.ascribe(UnspecifiedModule())
      binaryArg <- call(BinaryArg)
      repoId    <- ~call(BinaryRepoArg).toOption.getOrElse(BinRepoId.Central)
      binary    <- Binary.unapply(repoId, binaryArg)

      layer     <- Lenses.updateSchemas(optSchemaId, layer, true)(Lenses.layer.binaries(_, project.id,
                       module.id))(_(_) += binary)
      
      _         <- ~Layer.save(layer, layout)
      schema    <- defaultSchema

      _         <- ~Compilation.asyncCompilation(schema, module.ref(project), layout,
                       false)

    } yield log.await()
  }
}

object ParamCli {

  case class ParamCtx(moduleCtx: ModuleCli.Context, optModule: Option[Module])

  def context(cli: Cli[CliParam[_]])(implicit log: Log) =
    for {
      ctx         <- ModuleCli.context(cli)
      cli         <- cli.hint(ModuleArg, ctx.optProject.to[List].flatMap(_.modules))
      optModuleId <- ~cli.peek(ModuleArg).orElse(ctx.optProject.flatMap(_.main))

      optModule   <- Success { for {
                       project  <- ctx.optProject
                       moduleId <- optModuleId
                       module   <- project.modules.findBy(moduleId).toOption
                     } yield module }

    } yield ParamCtx(ctx.copy(cli = cli), optModule)

  def list(ctx: ParamCtx)(implicit log: Log): Try[ExitStatus] = {
    import ctx._, moduleCtx._
    for {
      cli     <- cli.hint(RawArg)
      call    <- cli.call()
      raw     <- ~call(RawArg).isSuccess
      project <- optProject.ascribe(UnspecifiedProject())
      module  <- optModule.ascribe(UnspecifiedModule())
      rows    <- ~module.params.to[List]
      table   <- ~Tables().show(Tables().params, cli.cols, rows, raw)(_.name)
      schema  <- defaultSchema

      _       <- ~(if(!raw) log.info(Tables().contextString(layer, layer.showSchema, schema,
                     project, module)))

      _       <- ~log.rawln(table.mkString("\n"))
    } yield log.await()
  }

  def remove(ctx: ParamCtx)(implicit log: Log): Try[ExitStatus] = {
    import ctx._, moduleCtx._
    for {
      cli      <- cli.hint(ParamArg, optModule.to[List].flatMap(_.params))
      cli      <- cli.hint(ForceArg)
      call     <- cli.call()
      paramArg <- call(ParamArg)
      project  <- optProject.ascribe(UnspecifiedProject())
      module   <- optModule.ascribe(UnspecifiedModule())
      force    <- ~call(ForceArg).isSuccess

      layer    <- Lenses.updateSchemas(optSchemaId, layer, force)(Lenses.layer.params(_, project.id,
                      module.id))(_(_) -= paramArg)

      _        <- ~Layer.save(layer, layout)
      schema   <- defaultSchema

      _        <- ~Compilation.asyncCompilation(schema, module.ref(project), layout,
                      false)

    } yield log.await()
  }

  def add(ctx: ParamCtx)(implicit log: Log): Try[ExitStatus] = {
    import ctx._, moduleCtx._
    for {
      cli     <- cli.hint(ParamArg)
      call    <- cli.call()
      project <- optProject.ascribe(UnspecifiedProject())
      module  <- optModule.ascribe(UnspecifiedModule())
      param   <- call(ParamArg)

      layer   <- Lenses.updateSchemas(optSchemaId, layer, true)(Lenses.layer.params(_, project.id, module.id))(
                     _(_) += param)

      _       <- ~Layer.save(layer, layout)
      schema  <- defaultSchema

      _       <- ~Compilation.asyncCompilation(schema, module.ref(project), layout,
                     false)

    } yield log.await()
  }
}
