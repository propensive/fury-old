/*
  Fury, version 0.2.2. Copyright 2019 Jon Pretty, Propensive Ltd.

  The primary distribution site is: https://propensive.com/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required  by applicable  law or  agreed to  in writing,  software  distributed  under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
  express  or  implied.  See  the  License for  the specific  language  governing  permissions and
  limitations under the License.
 */
package fury

import fury.error._

import guillotine._
import mercator._
import Args._

import scala.collection.immutable.SortedSet
import scala.util._

object ModuleCli {

  case class Context(
      override val cli: Cli[CliParam[_]],
      override val layout: Layout,
      override val config: Config,
      override val layer: Layer,
      optSchema: Option[Schema],
      optProject: Option[Project])
      extends MenuContext(cli, layout, config, layer, optSchema.map(_.id)) {
    def defaultSchemaId: SchemaId      = optSchemaId.getOrElse(layer.main)
    def defaultSchema: Outcome[Schema] = layer.schemas.findBy(defaultSchemaId)
  }

  def context(cli: Cli[CliParam[_]]) =
    for {
      layout    <- cli.layout
      config    <- Config.read()(cli.env, layout)
      layer     <- Layer.read(layout.furyConfig, layout)
      cli       <- cli.hint(SchemaArg, layer.schemas)
      schemaArg <- ~cli.peek(SchemaArg)
      schema    <- ~layer.schemas.findBy(schemaArg.getOrElse(layer.main)).toOption
      cli       <- cli.hint(ProjectArg, schema.map(_.projects).getOrElse(Nil))
      optProjectId <- ~schema.flatMap { s =>
                       cli.peek(ProjectArg).orElse(s.main)
                     }
      optProject <- ~schema.flatMap { s =>
                     optProjectId.flatMap(s.projects.findBy(_).toOption)
                   }
    } yield new Context(cli, layout, config, layer, schema, optProject)

  def select(ctx: Context) = {
    import ctx._
    for {
      cli      <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
      invoc    <- cli.read()
      io       <- invoc.io()
      schema   <- defaultSchema
      project  <- optProject.ascribe(UnspecifiedProject())
      moduleId <- ~invoc(ModuleArg).toOption
      moduleId <- moduleId.ascribe(UnspecifiedModule())
      _        <- project(moduleId)
      lens     <- ~Lenses.layer.mainModule(schema.id, project.id)
      layer    <- ~(lens(layer) = Some(moduleId))
      _        <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }

  def list(ctx: Context) = {
    import ctx._
    for {
      cols    <- Success(Terminal.columns.getOrElse(100))
      project <- optProject.ascribe(UnspecifiedProject())
      cli     <- cli.hint(RawArg)
      invoc   <- cli.read()
      io      <- invoc.io()
      raw     <- ~invoc(RawArg).isSuccess
      rows    <- ~project.modules.to[List]
      table <- ~Tables(config)
                .show(Tables(config).modules(project.id, project.main), cols, rows, raw)(_.id)
      schema <- defaultSchema
      _ <- ~(if (!raw)
               io.println(
                   Tables(config).contextString(layout.pwd, layer.showSchema, schema, project)))
      _ <- ~io.println(table.mkString("\n"))
    } yield io.await()
  }

  def add(ctx: Context) = {
    import ctx._
    for {
      cli <- cli.hint(ModuleNameArg)
      cli <- cli.hint(
                CompilerArg,
                ModuleRef.JavaRef :: defaultSchema.toOption
                  .to[List]
                  .flatMap(_.compilerRefs(layout)))
      cli     <- cli.hint(KindArg, Kind.all)
      optKind <- ~cli.peek(KindArg)
      cli <- optKind match {
              case Some(Application) =>
                for {
                  cli <- cli.hint(MainArg)
                } yield cli
              case Some(Plugin) =>
                for {
                  cli <- cli.hint(MainArg)
                  cli <- cli.hint(PluginArg)
                } yield cli
              case None | Some(Library | Compiler) => ~cli
            }
      invoc      <- cli.read()
      io         <- invoc.io()
      project    <- optProject.ascribe(UnspecifiedProject())
      moduleArg  <- invoc(ModuleNameArg)
      moduleId   <- fury.Module.available(moduleArg, project)
      compilerId <- ~invoc(CompilerArg).toOption
      optCompilerRef <- compilerId
                         .map(ModuleRef.parse(project, _, true))
                         .to[List]
                         .sequence
                         .map(_.headOption)
      module <- ~fury.Module(moduleId, compiler = optCompilerRef.getOrElse(ModuleRef.JavaRef))
      module <- ~invoc(KindArg).toOption.map { k =>
                 module.copy(kind = k)
               }.getOrElse(module)
      module <- ~invoc(MainArg).toOption.map { m =>
                 module.copy(main = if (m == "") None else Some(m))
               }.getOrElse(module)
      module <- ~invoc(PluginArg).toOption.map { p =>
                 module.copy(plugin = if (p == "") None else Some(p))
               }.getOrElse(module)
      layer <- Lenses.updateSchemas(optSchemaId, layer, true)(Lenses.layer.modules(_, project.id)) {
                (lens, ws) =>
                  lens.modify(layer)((_: SortedSet[Module]) + module)
              }
      layer <- Lenses.updateSchemas(optSchemaId, layer, true)(
                  Lenses.layer.mainModule(_, project.id)) { (lens, ws) =>
                lens(ws) = Some(module.id)
              }
      _ <- ~io.save(layer, layout.furyConfig)
      _ <- ~io.println(msg"Set current module to ${module.id}")
    } yield io.await()
  }

  def remove(ctx: Context) = {
    import ctx._
    for {
      cli      <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
      schema   <- defaultSchema
      cli      <- cli.hint(CompilerArg, defaultSchema.toOption.to[List].flatMap(_.compilerRefs(layout)))
      cli      <- cli.hint(ForceArg)
      invoc    <- cli.read()
      io       <- invoc.io()
      force    <- ~invoc(ForceArg).isSuccess
      moduleId <- invoc(ModuleArg)
      project  <- optProject.ascribe(UnspecifiedProject())
      module   <- project.modules.findBy(moduleId)
      layer <- Lenses
                .updateSchemas(optSchemaId, layer, force)(Lenses.layer.modules(_, project.id)) {
                  (lens, ws) =>
                    lens.modify(ws)((_: SortedSet[Module]).filterNot(_.id == module.id))
                }
      layer <- Lenses.updateSchemas(optSchemaId, layer, force)(
                  Lenses.layer.mainModule(_, project.id)) { (lens, ws) =>
                if (lens(ws) == Some(moduleId)) lens(ws) = None else ws
              }
      _ <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }

  def update(ctx: Context) = {
    import ctx._
    for {
      cli <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
      cli <- cli.hint(
                CompilerArg,
                ModuleRef.JavaRef :: defaultSchema.toOption
                  .to[List]
                  .flatMap(_.compilerRefs(layout)))
      cli         <- cli.hint(KindArg, Kind.all)
      optModuleId <- ~cli.peek(ModuleArg).orElse(optProject.flatMap(_.main))
      optModule <- Success {
                    for {
                      project  <- optProject
                      moduleId <- optModuleId
                      module   <- project.modules.findBy(moduleId).toOption
                    } yield module
                  }
      cli     <- cli.hint(ModuleNameArg, optModuleId.to[List])
      optKind <- ~cli.peek(KindArg).orElse(optModule.map(_.kind))
      cli <- optKind match {
              case Some(Application) =>
                for {
                  cli <- cli.hint(MainArg)
                } yield cli
              case Some(Plugin) =>
                for {
                  cli <- cli.hint(MainArg)
                  cli <- cli.hint(PluginArg)
                } yield cli
              case Some(Compiler) =>
                for (cli <- cli.hint(BloopSpecArg)) yield cli
              case None | Some(Library) =>
                ~cli
            }
      cli        <- cli.hint(ForceArg)
      invoc      <- cli.read()
      io         <- invoc.io()
      compilerId <- ~invoc(CompilerArg).toOption
      project    <- optProject.ascribe(UnspecifiedProject())
      module     <- optModule.ascribe(UnspecifiedModule())
      compilerRef <- compilerId
                      .map(ModuleRef.parse(project, _, true))
                      .to[List]
                      .sequence
                      .map(_.headOption.getOrElse(module.compiler))
      kind       <- ~optKind.getOrElse(module.kind)
      mainClass  <- ~invoc(MainArg).toOption
      pluginName <- ~invoc(PluginArg).toOption
      nameArg    <- ~invoc(ModuleNameArg).toOption
      newId      <- ~nameArg.flatMap(project.unused(_).toOption).getOrElse(module.id)
      bloopSpec <- invoc(BloopSpecArg).toOption
                    .to[List]
                    .map(BloopSpec.parse(_))
                    .sequence
                    .map(_.headOption)
      force <- ~invoc(ForceArg).isSuccess
      layer <- Lenses.updateSchemas(optSchemaId, layer, force)(
                  Lenses.layer.moduleKind(_, project.id, module.id))(_(_) = kind)
      layer <- Lenses.updateSchemas(optSchemaId, layer, force)(
                  Lenses.layer.moduleCompiler(_, project.id, module.id))(_(_) = compilerRef)
      layer <- Lenses.updateSchemas(optSchemaId, layer, force)(
                  Lenses.layer.moduleBloopSpec(_, project.id, module.id))(_(_) = bloopSpec)
      layer <- Lenses.updateSchemas(optSchemaId, layer, force)(
                  Lenses.layer.moduleMainClass(_, project.id, module.id))(_(_) = mainClass)
      layer <- Lenses.updateSchemas(optSchemaId, layer, force)(
                  Lenses.layer.modulePluginName(_, project.id, module.id))(_(_) = pluginName)
      layer <- Lenses.updateSchemas(optSchemaId, layer, force)(
                  Lenses.layer.moduleId(_, project.id, module.id))(_(_) = newId)
      _ <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }
}

object BinaryCli {

  case class BinariesCtx(moduleCtx: ModuleCli.Context, optModule: Option[Module])

  def context(cli: Cli[CliParam[_]]) =
    for {
      ctx         <- ModuleCli.context(cli)
      cli         <- cli.hint(ModuleArg, ctx.optProject.to[List].flatMap(_.modules))
      optModuleId <- ~cli.peek(ModuleArg).orElse(ctx.optProject.flatMap(_.main))
      optModule <- Success {
                    for {
                      project  <- ctx.optProject
                      moduleId <- optModuleId
                      module   <- project.modules.findBy(moduleId).toOption
                    } yield module
                  }
    } yield BinariesCtx(ctx, optModule)

  def list(ctx: BinariesCtx) = {
    import ctx._, moduleCtx._
    for {
      cli     <- cli.hint(RawArg)
      invoc   <- cli.read()
      io      <- invoc.io()
      raw     <- ~invoc(RawArg).isSuccess
      project <- optProject.ascribe(UnspecifiedProject())
      module  <- optModule.ascribe(UnspecifiedModule())
      cols    <- Success(Terminal.columns.getOrElse(100))
      rows    <- ~module.binaries.to[List]
      schema  <- defaultSchema
      table   <- ~Tables(config).show(Tables(config).binaries, cols, rows, raw)(identity)
      _ <- ~(if (!raw)
               io.println(
                   Tables(config)
                     .contextString(layout.pwd, layer.showSchema, schema, project, module)))
      _ <- ~io.println(table.mkString("\n"))
    } yield io.await()
  }

  def remove(ctx: BinariesCtx) = {
    import ctx._, moduleCtx._
    for {
      cli         <- cli.hint(BinaryArg, optModule.to[List].flatMap(_.binaries))
      cli         <- cli.hint(ForceArg)
      invoc       <- cli.read()
      io          <- invoc.io()
      binaryArg   <- invoc(BinaryArg)
      module      <- optModule.ascribe(UnspecifiedModule())
      project     <- optProject.ascribe(UnspecifiedProject())
      binaryToDel <- ~module.binaries.find(_.spec == binaryArg)
      force       <- ~invoc(ForceArg).isSuccess
      layer <- Lenses.updateSchemas(optSchemaId, layer, force)(
                  Lenses.layer.binaries(_, project.id, module.id))(_(_) --= binaryToDel)
      _ <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }

  def add(ctx: BinariesCtx) = {
    import ctx._, moduleCtx._
    for {
      cli       <- cli.hint(BinaryArg)
      cli       <- cli.hint(BinaryRepoArg, List(RepoId("central")))
      invoc     <- cli.read()
      io        <- invoc.io()
      module    <- optModule.ascribe(UnspecifiedModule())
      project   <- optProject.ascribe(UnspecifiedProject())
      binaryArg <- invoc(BinaryArg)
      repoId    <- ~invoc(BinaryRepoArg).toOption.getOrElse(BinRepoId.Central)
      binary    <- Binary.unapply(repoId, binaryArg)
      layer <- Lenses.updateSchemas(optSchemaId, layer, true)(
                  Lenses.layer.binaries(_, project.id, module.id))(_(_) += binary)
      _ <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }
}

object ParamCli {

  case class ParamCtx(moduleCtx: ModuleCli.Context, optModule: Option[Module])

  def context(cli: Cli[CliParam[_]]) =
    for {
      ctx         <- ModuleCli.context(cli)
      cli         <- cli.hint(ModuleArg, ctx.optProject.to[List].flatMap(_.modules))
      optModuleId <- ~cli.peek(ModuleArg).orElse(ctx.optProject.flatMap(_.main))
      optModule <- Success {
                    for {
                      project  <- ctx.optProject
                      moduleId <- optModuleId
                      module   <- project.modules.findBy(moduleId).toOption
                    } yield module
                  }
    } yield ParamCtx(ctx, optModule)

  def list(ctx: ParamCtx) = {
    import ctx._, moduleCtx._
    for {
      cli     <- cli.hint(RawArg)
      invoc   <- cli.read()
      io      <- invoc.io()
      raw     <- ~invoc(RawArg).isSuccess
      project <- optProject.ascribe(UnspecifiedProject())
      module  <- optModule.ascribe(UnspecifiedModule())
      cols    <- Success(Terminal.columns.getOrElse(100))
      rows    <- ~module.params.to[List]
      table   <- ~Tables(config).show(Tables(config).params, cols, rows, raw)(_.name)
      schema  <- defaultSchema
      _ <- ~(if (!raw)
               io.println(
                   Tables(config)
                     .contextString(layout.pwd, layer.showSchema, schema, project, module)))
      _ <- ~io.println(table.mkString("\n"))
    } yield io.await()
  }

  def remove(ctx: ParamCtx) = {
    import ctx._, moduleCtx._
    for {
      cli      <- cli.hint(ParamArg, optModule.to[List].flatMap(_.params))
      cli      <- cli.hint(ForceArg)
      invoc    <- cli.read()
      io       <- invoc.io()
      paramArg <- invoc(ParamArg)
      module   <- optModule.ascribe(UnspecifiedModule())
      project  <- optProject.ascribe(UnspecifiedProject())
      force    <- ~invoc(ForceArg).isSuccess
      layer <- Lenses.updateSchemas(optSchemaId, layer, force)(
                  Lenses.layer.params(_, project.id, module.id))(_(_) -= paramArg)
      _ <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }

  def add(ctx: ParamCtx) = {
    import ctx._, moduleCtx._
    for {
      cli     <- cli.hint(ParamArg)
      invoc   <- cli.read()
      io      <- invoc.io()
      module  <- optModule.ascribe(UnspecifiedModule())
      project <- optProject.ascribe(UnspecifiedProject())
      param   <- invoc(ParamArg)
      layer <- Lenses.updateSchemas(optSchemaId, layer, true)(
                  Lenses.layer.params(_, project.id, module.id))(_(_) += param)
      _ <- ~io.save(layer, layout.furyConfig)
    } yield io.await()
  }
}
