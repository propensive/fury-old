/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.5.0. Copyright 2018-19 Jon Pretty, Propensive Ltd.                                        ║
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

import fury.strings._, fury.io._, fury.core._

import guillotine._
import Args._

import scala.collection.immutable.SortedSet
import scala.util._

object DependencyCli {

  case class Context(
      override val cli: Cli[CliParam[_]],
      override val layout: Layout,
      override val config: Config,
      override val layer: Layer,
      optSchema: Option[Schema],
      optProject: Option[Project],
      optModule: Option[Module])
      extends MenuContext(cli, layout, config, layer, optSchema.map(_.id)) {

    def defaultSchemaId: SchemaId  = optSchemaId.getOrElse(layer.main)
    def defaultSchema: Try[Schema] = layer.schemas.findBy(defaultSchemaId)
  }

  def context(cli: Cli[CliParam[_]]) =
    for {
      layout       <- cli.layout
      config       <- Config.read()(cli.env, cli.globalLayout)
      layer        <- Layer.read(Io.silent(config), layout.furyConfig, layout)
      cli          <- cli.hint(SchemaArg, layer.schemas)
      schemaArg    <- ~cli.peek(SchemaArg)
      schema       <- ~layer.schemas.findBy(schemaArg.getOrElse(layer.main)).toOption
      cli          <- cli.hint(ProjectArg, schema.map(_.projects).getOrElse(Nil))
      optProjectId <- ~schema.flatMap { s => cli.peek(ProjectArg).orElse(s.main) }
      optProject   <- ~schema.flatMap { s => optProjectId.flatMap(s.projects.findBy(_).toOption) }
      cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
      optModuleId  <- ~cli.peek(ModuleArg).orElse(optProject.flatMap(_.main))

      optModule    <- Success { for {
                        project  <- optProject
                        moduleId <- optModuleId
                        module   <- project.modules.findBy(moduleId).toOption
                      } yield module }

    } yield new Context(cli, layout, config, layer, schema, optProject, optModule)

  def list(ctx: Context): Try[ExitStatus] = {
    import ctx._
    for {
      cli     <- cli.hint(RawArg)
      invoc   <- cli.read()
      io      <- invoc.io()
      raw     <- ~invoc(RawArg).isSuccess
      project <- optProject.ascribe(UnspecifiedProject())
      module  <- optModule.ascribe(UnspecifiedModule())
      rows    <- ~module.after.to[List].sorted
      table   <- ~Tables(config).show(Tables(config).dependencies, cli.cols, rows, raw)(identity)
      schema  <- defaultSchema

      _       <- ~(if(!raw) io.println(Tables(config).contextString(layout.base, layer.showSchema, schema,
                     project, module)))

      _       <- ~io.println(table.mkString("\n"))
    } yield io.await()
  }

  def remove(ctx: Context): Try[ExitStatus] = {
    import ctx._
    for {
      cli       <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
      cli       <- cli.hint(LinkArg, optModule.to[List].flatMap(_.after.to[List]))
      cli       <- cli.hint(ForceArg)
      invoc     <- cli.read()
      io        <- invoc.io()
      linkArg   <- invoc(LinkArg)
      project   <- optProject.ascribe(UnspecifiedProject())
      module    <- optModule.ascribe(UnspecifiedModule())
      moduleRef <- ModuleRef.parse(project, linkArg, false)
      force     <- ~invoc(ForceArg).isSuccess

      layer     <- Lenses.updateSchemas(optSchemaId, layer, force)(Lenses.layer.after(_, project.id,
                       module.id))(_(_) -= moduleRef)

      _         <- ~Layer.save(io, layer, layout)
      optSchema <- ~layer.mainSchema.toOption
      _         <- ~optSchema.foreach(Compilation.asyncCompilation(io, _, moduleRef, layout, cli.globalLayout))
    } yield io.await()
  }

  def add(ctx: Context): Try[ExitStatus] = {
    import ctx._
    for {
      optSchema       <- ~layer.mainSchema.toOption
      importedSchemas  = optSchema.flatMap(_.importedSchemas(Io.silent(ctx.config), ctx.layout).toOption)
      allSchemas       = optSchema.toList ::: importedSchemas.toList.flatten
      allModules       = allSchemas.map(_.moduleRefs).flatten
      cli              <- cli.hint(LinkArg, allModules)
      cli              <- cli.hint(IntransitiveArg)
      invoc            <- cli.read()
      io               <- invoc.io()
      project          <- optProject.ascribe(UnspecifiedProject())
      module           <- optModule.ascribe(UnspecifiedModule())
      intransitive     <- ~invoc(IntransitiveArg).isSuccess
      linkArg          <- invoc(LinkArg)
      moduleRef        <- ModuleRef.parse(project, linkArg, intransitive)

      layer            <- Lenses.updateSchemas(optSchemaId, layer, true)(Lenses.layer.after(_, project.id,
                              module.id))(_(_) += moduleRef)

      _                <- ~Layer.save(io, layer, layout)

      _                <- ~optSchema.foreach(Compilation.asyncCompilation(io, _, moduleRef, layout,
                              cli.globalLayout))

    } yield io.await()
  }
}

object EnvCli {

  case class Context(override val cli: Cli[CliParam[_]],
                     override val layout: Layout,
                     override val config: Config,
                     override val layer: Layer,
                     optSchema: Option[Schema],
                     optProject: Option[Project],
                     optModule: Option[Module])
             extends MenuContext(cli, layout, config, layer, optSchema.map(_.id)) {

    def defaultSchemaId: SchemaId  = optSchemaId.getOrElse(layer.main)
    def defaultSchema: Try[Schema] = layer.schemas.findBy(defaultSchemaId)
  }

  def context(cli: Cli[CliParam[_]]) = for {
    layout       <- cli.layout
    config       <- Config.read()(cli.env, cli.globalLayout)
    layer        <- Layer.read(Io.silent(config), layout.furyConfig, layout)
    cli          <- cli.hint(SchemaArg, layer.schemas)
    schemaArg    <- ~cli.peek(SchemaArg)
    schema       <- ~layer.schemas.findBy(schemaArg.getOrElse(layer.main)).toOption
    cli          <- cli.hint(ProjectArg, schema.map(_.projects).getOrElse(Nil))
    optProjectId <- ~schema.flatMap { s => cli.peek(ProjectArg).orElse(s.main) }
    optProject   <- ~schema.flatMap { s => optProjectId.flatMap(s.projects.findBy(_).toOption) }
    cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    optModuleId  <- ~cli.peek(ModuleArg).orElse(optProject.flatMap(_.main))
    optModule    <- Success { for {
                      project  <- optProject
                      moduleId <- optModuleId
                      module   <- project.modules.findBy(moduleId).toOption
                    } yield module }
  } yield new Context(cli, layout, config, layer, schema, optProject, optModule)

  def list(ctx: Context): Try[ExitStatus] = {
    import ctx._
    for {
      cli     <- cli.hint(RawArg)
      invoc   <- cli.read()
      io      <- invoc.io()
      raw     <- ~invoc(RawArg).isSuccess
      project <- optProject.ascribe(UnspecifiedProject())
      module  <- optModule.ascribe(UnspecifiedModule())
      rows    <- ~module.environment.to[List].sorted
      table   <- ~Tables(config).show(Tables(config).envs, cli.cols, rows, raw)(identity)
      schema  <- defaultSchema

      _       <- ~(if(!raw) io.println(Tables(config).contextString(layout.base, layer.showSchema, schema,
                     project, module)))

      _       <- ~io.println(table.mkString("\n"))
    } yield io.await()
  }

  def remove(ctx: Context): Try[ExitStatus] = {
    import ctx._
    for {
      cli       <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
      cli       <- cli.hint(EnvArg, optModule.to[List].flatMap(_.environment.to[List]))
      cli       <- cli.hint(ForceArg)
      invoc     <- cli.read()
      io        <- invoc.io()
      envArg    <- invoc(EnvArg)
      project   <- optProject.ascribe(UnspecifiedProject())
      module    <- optModule.ascribe(UnspecifiedModule())
      force     <- ~invoc(ForceArg).isSuccess
      
      layer     <- Lenses.updateSchemas(optSchemaId, layer, force)(Lenses.layer.environment(_, project.id,
                       module.id))(_(_) -= envArg)

      _         <- ~Layer.save(io, layer, layout)
      optSchema <- ~layer.mainSchema.toOption
    } yield io.await()
  }

  def add(ctx: Context): Try[ExitStatus] = {
    import ctx._
    for {
      optSchema       <- ~layer.mainSchema.toOption
      importedSchemas  = optSchema.flatMap(_.importedSchemas(Io.silent(ctx.config), ctx.layout).toOption)
      allSchemas       = optSchema.toList ::: importedSchemas.toList.flatten
      allModules       = allSchemas.map(_.moduleRefs).flatten
      cli             <- cli.hint(EnvArg)
      invoc           <- cli.read()
      io              <- invoc.io()
      project         <- optProject.ascribe(UnspecifiedProject())
      module          <- optModule.ascribe(UnspecifiedModule())
      envArg          <- invoc(EnvArg)

      layer           <- Lenses.updateSchemas(optSchemaId, layer, true)(Lenses.layer.environment(_, project.id,
                             module.id))(_(_) += envArg)

      _               <- ~Layer.save(io, layer, layout)
    } yield io.await()
  }
}

object PermissionCli {
  
  case class Context(override val cli: Cli[CliParam[_]],
                     override val layout: Layout,
                     override val config: Config,
                     override val layer: Layer,
                     optSchema: Option[Schema],
                     optProject: Option[Project],
                     optModule: Option[Module])
             extends MenuContext(cli, layout, config, layer, optSchema.map(_.id)) {

    def defaultSchemaId: SchemaId  = optSchemaId.getOrElse(layer.main)
    def defaultSchema: Try[Schema] = layer.schemas.findBy(defaultSchemaId)
  }

  def context(cli: Cli[CliParam[_]]) = for {
    layout       <- cli.layout
    config       <- Config.read()(cli.env, cli.globalLayout)
    layer        <- Layer.read(Io.silent(config), layout.furyConfig, layout)
    cli          <- cli.hint(SchemaArg, layer.schemas)
    schemaArg    <- ~cli.peek(SchemaArg)
    schema       <- ~layer.schemas.findBy(schemaArg.getOrElse(layer.main)).toOption
    cli          <- cli.hint(ProjectArg, schema.map(_.projects).getOrElse(Nil))
    optProjectId <- ~schema.flatMap { s => cli.peek(ProjectArg).orElse(s.main) }
    optProject   <- ~schema.flatMap { s => optProjectId.flatMap(s.projects.findBy(_).toOption) }
    cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    optModuleId  <- ~cli.peek(ModuleArg).orElse(optProject.flatMap(_.main))

    optModule    <- Success { for {
                      project  <- optProject
                      moduleId <- optModuleId
                      module   <- project.modules.findBy(moduleId).toOption
                    } yield module }

  } yield new Context(cli, layout, config, layer, schema, optProject, optModule)

  def require(ctx: Context): Try[ExitStatus] = {
    import ctx._
    for {
      optSchema       <- ~layer.mainSchema.toOption
      importedSchemas  = optSchema.flatMap(_.importedSchemas(Io.silent(ctx.config), ctx.layout).toOption)
      allSchemas       = optSchema.toList ::: importedSchemas.toList.flatten
      allModules       = allSchemas.map(_.moduleRefs).flatten
      cli             <- cli.hint(ClassArg, Permission.Classes)
      cli             <- cli.hint(PermissionTargetArg)
      cli             <- cli.hint(ActionArg, List("read", "write", "read,write"))
      invoc           <- cli.read()
      io              <- invoc.io()
      project         <- optProject.ascribe(UnspecifiedProject())
      module          <- optModule.ascribe(UnspecifiedModule())
      classArg        <- invoc(ClassArg)
      targetArg       <- invoc(PermissionTargetArg)
      actionArg       <- ~invoc(ActionArg).toOption
      permission      <- ~Permission(classArg, targetArg, actionArg)
      
      layer           <- Lenses.updateSchemas(optSchemaId, layer, true)(Lenses.layer.policy(_, project.id,
                             module.id))(_(_) += permission)

      _               <- ~Layer.save(io, layer, layout)
    } yield io.await()
  }

  def obviate(ctx: Context): Try[ExitStatus] = {
    import ctx._
    for {
      cli      <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
      cli      <- cli.hint(PermissionArg, optModule.to[List].flatMap(_.policyEntries))
      cli      <- cli.hint(ForceArg)
      invoc    <- cli.read()
      io       <- invoc.io()
      permHash <- invoc(PermissionArg).map(PermissionHash(_))
      project  <- optProject.ascribe(UnspecifiedProject())
      module   <- optModule.ascribe(UnspecifiedModule())
      entry    <- module.policyEntries.find(_.hash == permHash).ascribe(ItemNotFound(permHash))
      force    <- ~invoc(ForceArg).isSuccess
      
      layer    <- Lenses.updateSchemas(optSchemaId, layer, force)(Lenses.layer.policy(_, project.id,
                      module.id))(_(_) -= entry.permission)

      _        <- ~Layer.save(io, layer, layout)
    } yield io.await()
  }
  
  def list(ctx: Context): Try[ExitStatus] = {
    import ctx._
    for {
      cli     <- cli.hint(RawArg)
      invoc   <- cli.read()
      io      <- invoc.io()
      raw     <- ~invoc(RawArg).isSuccess
      project <- optProject.ascribe(UnspecifiedProject())
      module  <- optModule.ascribe(UnspecifiedModule())
      rows    <- ~module.policyEntries.to[List].sortBy(_.hash.key)
      table   <- ~Tables(config).show(Tables(config).permissions, cli.cols, rows, raw)(identity)
      schema  <- defaultSchema

      _       <- ~(if(!raw) io.println(Tables(config).contextString(layout.base, layer.showSchema, schema,
                     project, module)))

      _       <- ~io.println(table.mkString("\n"))
    } yield io.await()
  }

  def grant(ctx: Context): Try[ExitStatus] = {
    import ctx._
    for {
      cli      <- cli.hint(ScopeArg, ScopeId.All)
      cli      <- cli.hint(PermissionArg, optModule.to[List].flatMap(_.policyEntries))
      invoc    <- cli.read()
      io       <- invoc.io()
      scopeId  <- ~invoc(ScopeArg).getOrElse(ScopeId.Project)
      project  <- optProject.ascribe(UnspecifiedProject())
      module   <- optModule.ascribe(UnspecifiedModule())
      permHash <- invoc(PermissionArg).map(PermissionHash(_))
      entry    <- module.policyEntries.find(_.hash == permHash).ascribe(ItemNotFound(permHash))
      policy   <- Policy.read(io, cli.globalLayout)
      policy   <- ~policy.grant(Scope(scopeId, layout, layer, project), entry.permission)
      _        <- ~Policy.save(io, cli.globalLayout, policy)
    } yield io.await()
  }

}

object PropertyCli {

  case class Context(
      override val cli: Cli[CliParam[_]],
      override val layout: Layout,
      override val config: Config,
      override val layer: Layer,
      optSchema: Option[Schema],
      optProject: Option[Project],
      optModule: Option[Module])
      extends MenuContext(cli, layout, config, layer, optSchema.map(_.id)) {

    def defaultSchemaId: SchemaId  = optSchemaId.getOrElse(layer.main)
    def defaultSchema: Try[Schema] = layer.schemas.findBy(defaultSchemaId)
  }

  def context(cli: Cli[CliParam[_]]) = for {
    layout       <- cli.layout
    config       <- Config.read()(cli.env, cli.globalLayout)
    layer        <- Layer.read(Io.silent(config), layout.furyConfig, layout)
    cli          <- cli.hint(SchemaArg, layer.schemas)
    schemaArg    <- ~cli.peek(SchemaArg)
    schema       <- ~layer.schemas.findBy(schemaArg.getOrElse(layer.main)).toOption
    cli          <- cli.hint(ProjectArg, schema.map(_.projects).getOrElse(Nil))
    optProjectId <- ~schema.flatMap { s => cli.peek(ProjectArg).orElse(s.main) }
    optProject   <- ~schema.flatMap { s => optProjectId.flatMap(s.projects.findBy(_).toOption) }
    cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    optModuleId  <- ~cli.peek(ModuleArg).orElse(optProject.flatMap(_.main))

    optModule    <- Success { for {
                      project  <- optProject
                      moduleId <- optModuleId
                      module   <- project.modules.findBy(moduleId).toOption
                    } yield module }

  } yield new Context(cli, layout, config, layer, schema, optProject, optModule)

  def list(ctx: Context): Try[ExitStatus] = {
    import ctx._
    for {
      cli     <- cli.hint(RawArg)
      invoc   <- cli.read()
      io      <- invoc.io()
      raw     <- ~invoc(RawArg).isSuccess
      project <- optProject.ascribe(UnspecifiedProject())
      module  <- optModule.ascribe(UnspecifiedModule())
      rows    <- ~module.properties.to[List].sorted
      table   <- ~Tables(config).show(Tables(config).props, cli.cols, rows, raw)(identity)
      schema  <- defaultSchema

      _       <- ~(if(!raw) io.println(Tables(config).contextString(layout.base, layer.showSchema, schema,
                     project, module)))

      _       <- ~io.println(table.mkString("\n"))
    } yield io.await()
  }

  def remove(ctx: Context): Try[ExitStatus] = {
    import ctx._
    for {
      cli       <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
      cli       <- cli.hint(PropArg, optModule.to[List].flatMap(_.properties.to[List]))
      cli       <- cli.hint(ForceArg)
      invoc     <- cli.read()
      io        <- invoc.io()
      propArg   <- invoc(PropArg)
      project   <- optProject.ascribe(UnspecifiedProject())
      module    <- optModule.ascribe(UnspecifiedModule())
      force     <- ~invoc(ForceArg).isSuccess

      layer     <- Lenses.updateSchemas(optSchemaId, layer, force)(Lenses.layer.properties(_, project.id,
                       module.id))(_(_) -= propArg)

      _         <- ~Layer.save(io, layer, layout)
    } yield io.await()
  }

  def add(ctx: Context): Try[ExitStatus] = {
    import ctx._
    for {
      optSchema       <- ~layer.mainSchema.toOption
      importedSchemas  = optSchema.flatMap(_.importedSchemas(Io.silent(ctx.config), ctx.layout).toOption)
      allSchemas       = optSchema.toList ::: importedSchemas.toList.flatten
      allModules       = allSchemas.map(_.moduleRefs).flatten
      cli             <- cli.hint(PropArg)
      invoc           <- cli.read()
      io              <- invoc.io()
      project         <- optProject.ascribe(UnspecifiedProject())
      module          <- optModule.ascribe(UnspecifiedModule())
      propArg         <- invoc(PropArg)

      layer           <- Lenses.updateSchemas(optSchemaId, layer, true)(Lenses.layer.properties(_, project.id,
                             module.id))(_(_) += propArg)

      _               <- ~Layer.save(io, layer, layout)
    } yield io.await()
  }
}
