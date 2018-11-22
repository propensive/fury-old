/*
  Fury, version 0.1.0. Copyright 2018 Jon Pretty, Propensive Ltd.

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

import mitigation._
import guillotine._
import Args._

import scala.collection.immutable.SortedSet

object ModuleCli {
 
  case class Context(override val cli: Cli[CliParam[_]], override val layout: Layout, override val config: Config, override val workspace: Workspace, override val optSchemaId: Option[SchemaId],
      optProject: Option[Project])
      extends MenuContext(cli, layout, config, workspace, optSchemaId) {
    def defaultSchemaId: SchemaId = optSchemaId.getOrElse(workspace.main)
    def defaultSchema: Result[Schema, ~ | ItemNotFound] = workspace.schemas.findBy(defaultSchemaId)
  }

  def context(cli: Cli[CliParam[_]]) = for {
    layout       <- cli.layout
    config       <- Config.read()(cli.env, layout)
    workspace    <- Workspace.read(layout.furyConfig)(layout)
    cli          <- cli.hint(SchemaArg, workspace.schemas)
    schemaArg    <- ~cli.peek(SchemaArg)
    schema       <- ~workspace.schemas.findBy(schemaArg.getOrElse(workspace.main)).opt
    cli          <- cli.hint(ProjectArg, schema.map(_.projects).getOrElse(Nil))
    optProjectId <- ~schema.flatMap { s => cli.peek(ProjectArg).orElse(s.main) }
    optProject   <- ~schema.flatMap { s => optProjectId.flatMap(s.projects.findBy(_).opt) }
  } yield new Context(cli, layout, config, workspace, schemaArg, optProject)
     
  def select(ctx: Context) = {
    import ctx._
    for { 
      cli       <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
      io        <- cli.io()
      schema    <- defaultSchema
      project   <- optProject.ascribe(UnspecifiedProject())
      moduleId  <- ~io(ModuleArg).opt
      moduleId  <- moduleId.ascribe(UnspecifiedModule())
      lens      <- ~Lenses.workspace.mainModule(schema.id, project.id)
      workspace <- ~(lens(workspace) = Some(moduleId))
      io        <- ~io.save(workspace, layout.furyConfig)
    } yield io.await()
  }

  def list(ctx: Context) = {
    import ctx._
    for {
      cols    <- Answer(Terminal.columns.getOrElse(100))
      project <- optProject.ascribe(UnspecifiedProject())
      cli     <- cli.hint(RawArg)
      io      <- cli.io()
      raw     <- ~io(RawArg).successful
      rows    <- ~project.modules.to[List]
      table   <- ~Tables(config).show(Tables(config).modules(project.id, project.main), cols, rows, raw)(_.id)
      schema  <- defaultSchema
      io      <- ~(if(!raw) io.println(Tables(config).contextString(layout.pwd, workspace.showSchema, schema, project)) else io)
      io      <- ~io.println(table.mkString("\n"))
    } yield io.await()
  }
     
  def add(ctx: Context) = {
    import ctx._
    for {
      cli         <- cli.hint(ModuleNameArg)
      cli         <- cli.hint(CompilerArg, ModuleRef.JavaRef :: defaultSchema.opt.to[List].flatMap(_.compilerRefs))
      cli         <- cli.hint(KindArg, Kind.all)
      optKind     <- ~cli.peek(KindArg)
      cli         <- optKind match {
                       case Some(Application | Plugin) =>
                         for(cli <- cli.hint(MainArg)) yield cli
                       case None | Some(Library | Compiler) => ~cli
                     }
      io          <- cli.io()
      project     <- optProject.ascribe(UnspecifiedProject())
      moduleArg   <- io(ModuleNameArg)
      moduleId    <- fury.Module.available(moduleArg, project)
      compilerId  <- ~io(CompilerArg).opt
      optCompilerRef <- compilerId.map(ModuleRef.parse(project, _, true)).to[List].sequence.map(_.headOption)
      module      <- ~fury.Module(moduleId, compiler = optCompilerRef.getOrElse(ModuleRef.JavaRef))
      module      <- ~io(KindArg).opt.map { k => module.copy(kind = k) }.getOrElse(module)
      module      <- ~io(MainArg).opt.map { m =>
                       module.copy(main = if(m == "") None else Some(m))
                     }.getOrElse(module)
      workspace   <- Lenses.updateSchemas(optSchemaId, workspace, true)(Lenses.workspace.modules(_, project.id)) { (lens, ws) =>
                       lens.modify(workspace)((_: SortedSet[Module]) + module)
                     }
      workspace   <- Lenses.updateSchemas(optSchemaId, workspace, true)(Lenses.workspace.mainModule(_, project.id)) { (lens, ws) =>
                       lens(ws) = Some(module.id)
                     }
      io          <- ~io.save(workspace, layout.furyConfig)
      io          <- ~io.println(msg"Set current module to ${module.id}")
    } yield io.await()
  }

  def delete(ctx: Context) = {
    import ctx._
    for {
      cli       <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
      schema    <- defaultSchema
      cli       <- cli.hint(CompilerArg, defaultSchema.opt.to[List].flatMap(_.compilerRefs))
      cli       <- cli.hint(ForceArg)
      io        <- cli.io()
      force     <- ~io(ForceArg).successful
      moduleId  <- io(ModuleArg)
      project   <- optProject.ascribe(UnspecifiedProject())
      module    <- project.modules.findBy(moduleId)
      workspace <- Lenses.updateSchemas(optSchemaId, workspace, force)(Lenses.workspace.modules(_, project.id)) { (lens, ws) =>
                     lens.modify(ws)((_: SortedSet[Module]).filterNot(_.id == module.id))
                   }
      workspace <- Lenses.updateSchemas(optSchemaId, workspace, force)(Lenses.workspace.mainModule(_, project.id)) { (lens, ws) =>
                     if(lens(ws) == Some(moduleId)) lens(ws) = None else ws
                   }
      io        <- ~io.save(workspace, layout.furyConfig)
    } yield io.await()
  }


  def update(ctx: Context) = {
    import ctx._
    for {
      cli            <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
      cli            <- cli.hint(CompilerArg, ModuleRef.JavaRef :: defaultSchema.opt.to[List].flatMap(_.compilerRefs))
      cli            <- cli.hint(KindArg, Kind.all)
      optModuleId    <- ~cli.peek(ModuleArg).orElse(optProject.flatMap(_.main))
      optModule      <- Answer { for {
                          project  <- optProject
                          moduleId <- optModuleId
                          module   <- project.modules.findBy(moduleId).opt
                        } yield module }
      cli            <- cli.hint(ModuleNameArg, optModuleId.to[List])
      optKind        <- ~cli.peek(KindArg)
      cli            <- optKind match {
                          case Some(Application | Plugin) =>
                            for(cli <- cli.hint(MainArg)) yield cli
                          case Some(Compiler) =>
                            for(cli <- cli.hint(BloopSpecArg)) yield cli
                          case None | Some(Library) =>
                            ~cli
                        }
      cli            <- cli.hint(ForceArg)
      io             <- cli.io()
      compilerId     <- ~io(CompilerArg).opt
      project        <- optProject.ascribe(UnspecifiedProject())
      module         <- optModule.ascribe(UnspecifiedModule())
      compilerRef    <- compilerId.map(ModuleRef.parse(project, _, true)).to[List].sequence.map(_.headOption.getOrElse(module.compiler))
      kind           <- ~optKind.getOrElse(module.kind)
      mainClass      <- ~io(MainArg).opt
      nameArg        <- ~io(ModuleNameArg).opt
      newId          <- ~nameArg.flatMap(project.unused(_).opt).getOrElse(module.id)
      bloopSpec      <- io(BloopSpecArg).opt.to[List].map(BloopSpec.parse(_)).sequence.map(_.headOption)
      force          <- ~io(ForceArg).successful
      workspace      <- Lenses.updateSchemas(optSchemaId, workspace, force)(Lenses.workspace.moduleKind(_, project.id, module.id))(_(_) = kind)
      workspace      <- Lenses.updateSchemas(optSchemaId, workspace, force)(Lenses.workspace.moduleCompiler(_, project.id, module.id))(_(_) = compilerRef)
      workspace      <- Lenses.updateSchemas(optSchemaId, workspace, force)(Lenses.workspace.moduleBloopSpec(_, project.id, module.id))(_(_) = bloopSpec)
      workspace      <- Lenses.updateSchemas(optSchemaId, workspace, force)(Lenses.workspace.moduleMainClass(_, project.id, module.id))(_(_) = mainClass)
      workspace      <- Lenses.updateSchemas(optSchemaId, workspace, force)(Lenses.workspace.moduleId(_, project.id, module.id))(_(_) = newId)
      io             <- ~io.save(workspace, layout.furyConfig)
    } yield io.await()
  }
}

object DependencyCli {
  
  class Context(cli: Cli[CliParam[_]], layout: Layout, config: Config, workspace: Workspace, optSchemaId: Option[SchemaId],
      optProject: Option[Project], val optModule: Option[Module]) extends ModuleCli.Context(cli, layout, config, workspace, optSchemaId, optProject)
  
  def context(cli: Cli[CliParam[_]]) = {
    for {
      ctx         <- ModuleCli.context(cli)
      cli         <- cli.hint(ModuleArg, ctx.optProject.to[List].flatMap(_.modules))
      optModuleId <- ~cli.peek(ModuleArg).orElse(ctx.optProject.flatMap(_.main))
      optModule   <- Answer { for {
                       project  <- ctx.optProject
                       moduleId <- optModuleId
                       module   <- project.modules.findBy(moduleId).opt
                     } yield module }
    } yield new Context(cli, ctx.layout, ctx.config, ctx.workspace, ctx.optSchemaId, ctx.optProject, optModule)
  }

  def list(ctx: Context) = {
    import ctx._
    for {
      cli     <- cli.hint(RawArg)
      io      <- cli.io()
      raw     <- ~io(RawArg).successful
      project <- optProject.ascribe(UnspecifiedProject())
      module  <- optModule.ascribe(UnspecifiedModule())
      cols    <- Answer(Terminal.columns.getOrElse(100))
      rows    <- ~module.after.to[List].sorted
      table   <- ~Tables(config).show(Tables(config).dependencies, cols, rows, raw)(identity)
      schema  <- defaultSchema
      io      <- ~(if(!raw) io.println(Tables(config).contextString(layout.pwd, workspace.showSchema, schema, project, module)) else io)
      io      <- ~io.println(table.mkString("\n"))
    } yield io.await()
  }

  def delete(ctx: Context) = {
    import ctx._
    for {
      cli           <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
      cli           <- cli.hint(DependencyArg, optModule.to[List].flatMap(_.after.to[List]))
      cli           <- cli.hint(ForceArg)
      io            <- cli.io()
      dependencyArg <- io(DependencyArg)
      project       <- optProject.ascribe(UnspecifiedProject())
      module        <- optModule.ascribe(UnspecifiedModule())
      moduleRef     <- ModuleRef.parse(project, dependencyArg, false)
      force         <- ~io(ForceArg).successful
      workspace     <- Lenses.updateSchemas(optSchemaId, workspace, force)(Lenses.workspace.after(_, project.id, module.id))(_(_) -= moduleRef)
      io            <- ~io.save(workspace, layout.furyConfig)
    } yield io.await()
  }

  def add(ctx: Context) = {
    import ctx._
    for {
      cli           <- cli.hint(DependencyArg,
                           optProject.to[List].flatMap(workspace.moduleRefStrings(_)))
      cli           <- cli.hint(IntransitiveArg)
      io            <- cli.io()
      project       <- optProject.ascribe(UnspecifiedProject())
      module        <- optModule.ascribe(UnspecifiedModule())
      intransitive  <- ~io(IntransitiveArg).successful
      dependencyArg <- io(DependencyArg)
      moduleRef     <- ModuleRef.parse(project, dependencyArg, intransitive)
      workspace     <- Lenses.updateSchemas(optSchemaId, workspace, true)(Lenses.workspace.after(_, project.id, module.id))(_(_) += moduleRef)
      io            <- ~io.save(workspace, layout.furyConfig)
    } yield io.await()
  }
}
  
object BinaryCli {

  case class BinariesCtx(moduleCtx: ModuleCli.Context, optModule: Option[Module])

  def context(cli: Cli[CliParam[_]]) = {
    for {
      ctx         <- ModuleCli.context(cli)
      cli         <- cli.hint(ModuleArg, ctx.optProject.to[List].flatMap(_.modules))
      optModuleId <- ~cli.peek(ModuleArg).orElse(ctx.optProject.flatMap(_.main))
      optModule   <- Answer { for {
                       project  <- ctx.optProject
                       moduleId <- optModuleId
                       module   <- project.modules.findBy(moduleId).opt
                     } yield module }
    } yield BinariesCtx(ctx, optModule)
  }

  def list(ctx: BinariesCtx) = {
    import ctx._, moduleCtx._
    for {
      cli     <- cli.hint(RawArg)
      io      <- cli.io()
      raw     <- ~io(RawArg).successful
      project <- optProject.ascribe(UnspecifiedProject())
      module  <- optModule.ascribe(UnspecifiedModule())
      cols    <- Answer(Terminal.columns.getOrElse(100))
      rows    <- ~module.binaries.to[List]
      schema  <- defaultSchema
      table   <- ~Tables(config).show(Tables(config).binaries, cols, rows, raw)(identity)
      io      <- ~(if(!raw) io.println(Tables(config).contextString(layout.pwd, workspace.showSchema, schema, project, module)) else io)
      io      <- ~io.println(table.mkString("\n"))
    } yield io.await()
  }

  def delete(ctx: BinariesCtx) = {
    import ctx._, moduleCtx._
    for {
      cli         <- cli.hint(BinaryArg, optModule.to[List].flatMap(_.binaries))
      cli         <- cli.hint(ForceArg)
      io          <- cli.io()
      binaryArg   <- io(BinaryArg)
      module      <- optModule.ascribe(UnspecifiedModule())
      project     <- optProject.ascribe(UnspecifiedProject())
      binaryToDel <- ~module.binaries.find(_.spec == binaryArg)
      force       <- ~io(ForceArg).successful
      workspace   <- Lenses.updateSchemas(optSchemaId, workspace, force)(Lenses.workspace.binaries(_, project.id, module.id))(_(_) --= binaryToDel)
      io          <- ~io.save(workspace, layout.furyConfig)
    } yield io.await()
  }

  def add(ctx: BinariesCtx) = {
    import ctx._, moduleCtx._
    for {
      cli       <- cli.hint(BinaryArg)
      cli       <- cli.hint(BinaryRepoArg, List(RepoId("central")))
      io        <- cli.io()
      module    <- optModule.ascribe(UnspecifiedModule())
      project   <- optProject.ascribe(UnspecifiedProject())
      binaryArg <- io(BinaryArg)
      repoId    <- ~io(BinaryRepoArg).opt.getOrElse(BinRepoId.Central)
      binary    <- Binary.unapply(repoId, binaryArg)
      workspace <- Lenses.updateSchemas(optSchemaId, workspace, true)(Lenses.workspace.binaries(_, project.id, module.id))(_(_) += binary)
      io        <- ~io.save(workspace, layout.furyConfig)
    } yield io.await()
  }
}
  
object ParamCli {

  case class ParamCtx(moduleCtx: ModuleCli.Context, optModule: Option[Module])

  def context(cli: Cli[CliParam[_]]) = {
    for {
      ctx         <- ModuleCli.context(cli)
      cli         <- cli.hint(ModuleArg, ctx.optProject.to[List].flatMap(_.modules))
      optModuleId <- ~cli.peek(ModuleArg).orElse(ctx.optProject.flatMap(_.main))
      optModule   <- Answer { for {
                       project  <- ctx.optProject
                       moduleId <- optModuleId
                       module   <- project.modules.findBy(moduleId).opt
                     } yield module }
    } yield ParamCtx(ctx, optModule)
  }

  def list(ctx: ParamCtx) = {
    import ctx._, moduleCtx._
    for {
      cli     <- cli.hint(RawArg)
      io      <- cli.io()
      raw     <- ~io(RawArg).successful
      project <- optProject.ascribe(UnspecifiedProject())
      module  <- optModule.ascribe(UnspecifiedModule())
      cols    <- Answer(Terminal.columns.getOrElse(100))
      rows    <- ~module.params.to[List]
      table   <- ~Tables(config).show(Tables(config).params, cols, rows, raw)(_.name)
      schema  <- defaultSchema
      io      <- ~(if(!raw) io.println(Tables(config).contextString(layout.pwd, workspace.showSchema, schema, project, module)) else io)
      io      <- ~io.println(table.mkString("\n"))
    } yield io.await()
  }

  def delete(ctx: ParamCtx) = {
    import ctx._, moduleCtx._
    for {
      cli         <- cli.hint(ParamArg, optModule.to[List].flatMap(_.params))
      cli         <- cli.hint(ForceArg)
      io          <- cli.io()
      paramArg    <- io(ParamArg)
      module      <- optModule.ascribe(UnspecifiedModule())
      project     <- optProject.ascribe(UnspecifiedProject())
      force       <- ~io(ForceArg).successful
      workspace   <- Lenses.updateSchemas(optSchemaId, workspace, force)(Lenses.workspace.params(_, project.id, module.id))(_(_) -= paramArg)
      io          <- ~io.save(workspace, layout.furyConfig)
    } yield io.await()
  }

  def add(ctx: ParamCtx) = {
    import ctx._, moduleCtx._
    for {
      cli       <- cli.hint(ParamArg)
      io        <- cli.io()
      module    <- optModule.ascribe(UnspecifiedModule())
      project   <- optProject.ascribe(UnspecifiedProject())
      param     <- io(ParamArg)
      workspace <- Lenses.updateSchemas(optSchemaId, workspace, true)(Lenses.workspace.params(_, project.id, module.id))(_(_) += param)
      io        <- ~io.save(workspace, layout.furyConfig)
    } yield io.await()
  }
}


object SourceCli {

  case class SourcesCtx(moduleCtx: ModuleCli.Context, optModule: Option[Module])

  def context(cli: Cli[CliParam[_]]) = {
    for {
      ctx         <- ModuleCli.context(cli)
      cli         <- cli.hint(ModuleArg, ctx.optProject.to[List].flatMap(_.modules))
      optModuleId <- ~cli.peek(ModuleArg).orElse(ctx.optProject.flatMap(_.main))
      optModule   <- Answer { for {
                       project  <- ctx.optProject
                       moduleId <- optModuleId
                       module   <- project.modules.findBy(moduleId).opt
                     } yield module }
    } yield SourcesCtx(ctx.copy(cli = cli), optModule)
  }

  def list(ctx: SourcesCtx) = {
    import ctx._, moduleCtx._
    for {
      cli     <- cli.hint(RawArg)
      io      <- cli.io()
      raw     <- ~io(RawArg).successful
      module  <- optModule.ascribe(UnspecifiedModule())
      project <- optProject.ascribe(UnspecifiedProject())
      cols    <- Answer(Terminal.columns.getOrElse(100))
      rows    <- ~module.sources.to[List]
      table   <- ~Tables(config).show(Tables(config).sources, cols, rows, raw)(_.repoId)
      schema  <- defaultSchema
      io      <- ~(if(!raw) io.println(Tables(config).contextString(layout.pwd, workspace.showSchema, schema, project, module)) else io)
      io      <- ~io.println(table.mkString("\n"))
    } yield io.await()
  }

  def delete(ctx: SourcesCtx) = {
    import ctx._, moduleCtx._
    for {
      cli         <- cli.hint(SourceArg, optModule.to[List].flatMap(_.sources))
      cli         <- cli.hint(ForceArg)
      io          <- cli.io()
      sourceArg   <- io(SourceArg)
      source      <- ~Source.unapply(sourceArg)
      module      <- optModule.ascribe(UnspecifiedModule())
      project     <- optProject.ascribe(UnspecifiedProject())
      sourceToDel <- ~module.sources.find(Some(_) == source)
      force       <- ~io(ForceArg).successful
      workspace   <- Lenses.updateSchemas(optSchemaId, workspace, force)(Lenses.workspace.sources(_, project.id, module.id))(_(_) --= sourceToDel)
      io          <- ~io.save(workspace, layout.furyConfig)
    } yield io.await()
  }

    
  def add(ctx: SourcesCtx) = {
    import ctx._, moduleCtx._
    for {
      dSchema   <- defaultSchema
      repos     <- ~dSchema.allRepos.opt.to[List].flatten
      sources   <- optProject.to[List].flatMap { project =>
                     repos.map(_.sources({ n => n.endsWith(".scala") || n.endsWith(".java") },
                         project.id).map(_.to[List])).to[List]
                   }.sequence.map(_.flatten)
      cli       <- cli.hint(SourceArg, sources)
      io        <- cli.io()
      module    <- optModule.ascribe(UnspecifiedModule())
      project   <- optProject.ascribe(UnspecifiedProject())
      sourceArg <- io(SourceArg)
      source    <- ~Source.unapply(sourceArg)
      workspace <- Lenses.updateSchemas(optSchemaId, workspace, true)(Lenses.workspace.sources(_, project.id, module.id))(_(_) ++= source)
      io        <- ~io.save(workspace, layout.furyConfig)
    } yield io.await()
  }
}

