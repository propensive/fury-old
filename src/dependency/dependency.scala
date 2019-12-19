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

object DependencyCli {

  case class Context(
      override val cli: Cli[CliParam[_]],
      override val layout: Layout,
      override val layer: Layer,
      optSchema: Option[Schema],
      optProject: Option[Project],
      optModule: Option[Module])
      extends MenuContext(cli, layout, layer, optSchema.map(_.id)) {

    def defaultSchemaId: SchemaId  = optSchemaId.getOrElse(layer.main)
    def defaultSchema: Try[Schema] = layer.schemas.findBy(defaultSchemaId)
  }

  def context(cli: Cli[CliParam[_]])(implicit log: Log) =
    for {
      layout       <- cli.layout
      layer        <- Layer.read(layout)
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

    } yield new Context(cli, layout, layer, schema, optProject, optModule)

  def list(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli     <- cli.hint(RawArg)
      call    <- cli.call()
      raw     <- ~call(RawArg).isSuccess
      project <- optProject.ascribe(UnspecifiedProject())
      module  <- optModule.ascribe(UnspecifiedModule())
      rows    <- ~module.after.to[List].sorted
      table   <- ~Tables().show(Tables().dependencies, cli.cols, rows, raw)(identity)
      schema  <- defaultSchema

      _       <- ~(if(!raw) log.info(Tables().contextString(layer, layer.showSchema, schema,
                     project, module)))

      _       <- ~log.info(table.mkString("\n"))
    } yield log.await()
  }

  def remove(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli       <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
      cli       <- cli.hint(LinkArg, optModule.to[List].flatMap(_.after.to[List]))
      cli       <- cli.hint(ForceArg)
      cli       <- cli.hint(HttpsArg)
      call      <- cli.call()
      https     <- ~call(HttpsArg).isSuccess
      linkArg   <- call(LinkArg)
      project   <- optProject.ascribe(UnspecifiedProject())
      module    <- optModule.ascribe(UnspecifiedModule())
      moduleRef <- ModuleRef.parse(project.id, linkArg, false)
      force     <- ~call(ForceArg).isSuccess

      layer     <- Lenses.updateSchemas(optSchemaId, layer, force)(Lenses.layer.after(_, project.id,
                       module.id))(_(_) -= moduleRef)

      _         <- ~Layer.save(layer, layout)
      optSchema <- ~layer.mainSchema.toOption

      _         <- ~optSchema.foreach(Compilation.asyncCompilation(_, moduleRef, layout,
                       https))

    } yield log.await()
  }

  def add(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      optSchema       <- ~layer.mainSchema.toOption
      importedSchemas  = optSchema.flatMap(_.importedSchemas(ctx.layout, false).toOption)
      allSchemas       = optSchema.toList ::: importedSchemas.toList.flatten
      allModules       = allSchemas.map(_.moduleRefs).flatten
      cli              <- cli.hint(LinkArg, allModules.filter(!_.hidden))
      cli              <- cli.hint(IntransitiveArg)
      call             <- cli.call()
      project          <- optProject.ascribe(UnspecifiedProject())
      module           <- optModule.ascribe(UnspecifiedModule())
      intransitive     <- ~call(IntransitiveArg).isSuccess
      linkArg          <- call(LinkArg)
      moduleRef        <- ModuleRef.parse(project.id, linkArg, intransitive)

      layer            <- Lenses.updateSchemas(optSchemaId, layer, true)(Lenses.layer.after(_, project.id,
                              module.id))(_(_) += moduleRef)

      _                <- ~Layer.save(layer, layout)

      _                <- ~optSchema.foreach(Compilation.asyncCompilation(_, moduleRef, layout,
                              false))

    } yield log.await()
  }
}

object EnvCli {

  case class Context(override val cli: Cli[CliParam[_]],
                     override val layout: Layout,
                     override val layer: Layer,
                     optSchema: Option[Schema],
                     optProject: Option[Project],
                     optModule: Option[Module])
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
    cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    optModuleId  <- ~cli.peek(ModuleArg).orElse(optProject.flatMap(_.main))
    optModule    <- Success { for {
                      project  <- optProject
                      moduleId <- optModuleId
                      module   <- project.modules.findBy(moduleId).toOption
                    } yield module }
  } yield new Context(cli, layout, layer, schema, optProject, optModule)

  def list(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli     <- cli.hint(RawArg)
      call    <- cli.call()
      raw     <- ~call(RawArg).isSuccess
      project <- optProject.ascribe(UnspecifiedProject())
      module  <- optModule.ascribe(UnspecifiedModule())
      rows    <- ~module.environment.to[List].sorted
      table   <- ~Tables().show(Tables().envs, cli.cols, rows, raw)(identity)
      schema  <- defaultSchema

      _       <- ~(if(!raw) log.info(Tables().contextString(layer, layer.showSchema, schema,
                     project, module)))

      _       <- ~log.info(table.mkString("\n"))
    } yield log.await()
  }

  def remove(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli       <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
      cli       <- cli.hint(EnvArg, optModule.to[List].flatMap(_.environment.to[List]))
      cli       <- cli.hint(ForceArg)
      call      <- cli.call()
      envArg    <- call(EnvArg)
      project   <- optProject.ascribe(UnspecifiedProject())
      module    <- optModule.ascribe(UnspecifiedModule())
      force     <- ~call(ForceArg).isSuccess
      
      layer     <- Lenses.updateSchemas(optSchemaId, layer, force)(Lenses.layer.environment(_, project.id,
                       module.id))(_(_) -= envArg)

      _         <- ~Layer.save(layer, layout)
      optSchema <- ~layer.mainSchema.toOption
    } yield log.await()
  }

  def add(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      optSchema       <- ~layer.mainSchema.toOption
      importedSchemas  = optSchema.flatMap(_.importedSchemas(ctx.layout, false).toOption)
      allSchemas       = optSchema.toList ::: importedSchemas.toList.flatten
      allModules       = allSchemas.map(_.moduleRefs).flatten
      cli             <- cli.hint(EnvArg)
      call            <- cli.call()
      project         <- optProject.ascribe(UnspecifiedProject())
      module          <- optModule.ascribe(UnspecifiedModule())
      envArg          <- call(EnvArg)

      layer           <- Lenses.updateSchemas(optSchemaId, layer, true)(Lenses.layer.environment(_, project.id,
                             module.id))(_(_) += envArg)

      _               <- ~Layer.save(layer, layout)
    } yield log.await()
  }
}

object PermissionCli {
  
  case class Context(override val cli: Cli[CliParam[_]],
                     override val layout: Layout,
                     override val layer: Layer,
                     optSchema: Option[Schema],
                     optProject: Option[Project],
                     optModule: Option[Module])
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
    cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    optModuleId  <- ~cli.peek(ModuleArg).orElse(optProject.flatMap(_.main))

    optModule    <- Success { for {
                      project  <- optProject
                      moduleId <- optModuleId
                      module   <- project.modules.findBy(moduleId).toOption
                    } yield module }

  } yield Context(cli, layout, layer, schema, optProject, optModule)

  def require(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli             <- cli.hint(ScopeArg, ScopeId.All)
      cli             <- cli.hint(NoGrantArg)
      cli             <- cli.hint(ClassArg, Permission.Classes)
      cli             <- cli.hint(PermissionTargetArg)
      cli             <- cli.hint(ActionArg, List("read", "write", "read,write"))
      call            <- cli.call()
      scopeId         =  call(ScopeArg).getOrElse(ScopeId.Project)
      project         <- optProject.ascribe(UnspecifiedProject())
      module          <- optModule.ascribe(UnspecifiedModule())
      classArg        <- call(ClassArg)
      targetArg       <- call(PermissionTargetArg)
      actionArg       =  call(ActionArg).toOption
      grant           =  call(NoGrantArg).isFailure
      permission      =  Permission(classArg, targetArg, actionArg)
      layer           <- Lenses.updateSchemas(optSchemaId, layer, true)(Lenses.layer.policy(_, project.id,
                             module.id))(_(_) += permission)
      _               <- Layer.save(layer, layout)
      policy          <- ~Policy.read(log)
      newPolicy       =  if(grant) policy.grant(Scope(scopeId, layout, project.id), List(permission)) else policy
      _               <- Policy.save(newPolicy)
    } yield {
      log.info(msg"${PermissionHash(permission.hash)}")
      log.await()
    }
  }

  def obviate(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli           <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
      cli           <- cli.hint(PermissionArg, optModule.to[List].flatMap(_.policyEntries))
      cli           <- cli.hint(ForceArg)
      call          <- cli.call()
      permHashes    <- call(PermissionArg).map(_.map(PermissionHash(_)))
      project       <- optProject.ascribe(UnspecifiedProject())
      module        <- optModule.ascribe(UnspecifiedModule())
      schema        <- layer.schemas.findBy(layer.main)
      hierarchy     <- schema.hierarchy(layout)
      universe      <- hierarchy.universe
      compilation   <- Compilation.fromUniverse(universe, module.ref(project), layout)
      permissions   <- permHashes.traverse(_.resolve(compilation.requiredPermissions))
      force         =  call(ForceArg).isSuccess
      layer         <- Lenses.updateSchemas(optSchemaId, layer, force)(Lenses.layer.policy(_, project.id,
                           module.id))((x, y) => x(y) = x(y) diff permissions.to[Set])
      _             <- Layer.save(layer, layout)
    } yield log.await()
  }
  
  def list(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli     <- cli.hint(RawArg)
      call    <- cli.call()
      raw     <- ~call(RawArg).isSuccess
      project <- optProject.ascribe(UnspecifiedProject())
      module  <- optModule.ascribe(UnspecifiedModule())
      rows    <- ~module.policyEntries.to[List].sortBy(_.hash.key)
      table   <- ~Tables().show(Tables().permissions, cli.cols, rows, raw)(identity)
      schema  <- defaultSchema

      _       <- ~(if(!raw) log.info(Tables().contextString(layer, layer.showSchema, schema,
                     project, module)))

      _       <- ~log.info(table.mkString("\n"))
    } yield log.await()
  }

  def grant(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._ 
    
    for {
      cli           <- cli.hint(ScopeArg, ScopeId.All)
      //TODO check if hints still work
      cli           <- cli.hint(PermissionArg, optModule.to[List].flatMap(_.policyEntries))
      call          <- cli.call()
      scopeId       =  call(ScopeArg).getOrElse(ScopeId.Project)
      project       <- optProject.ascribe(UnspecifiedProject())
      module        <- optModule.ascribe(UnspecifiedModule())
      permHashes    <- call(PermissionArg).map(_.map(PermissionHash(_)))
      schema        <- layer.schemas.findBy(layer.main)
      hierarchy     <- schema.hierarchy(layout)
      universe      <- hierarchy.universe
      compilation   <- Compilation.fromUniverse(universe, module.ref(project), layout)
      permissions   <- permHashes.traverse(_.resolve(compilation.requiredPermissions))
      policy        =  Policy.read(log)
      newPolicy     =  policy.grant(Scope(scopeId, layout, project.id), permissions)
      _             <- Policy.save(newPolicy)
    } yield log.await()
  }

}

object PropertyCli {

  case class Context(
      override val cli: Cli[CliParam[_]],
      override val layout: Layout,
      override val layer: Layer,
      optSchema: Option[Schema],
      optProject: Option[Project],
      optModule: Option[Module])
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
    cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    optModuleId  <- ~cli.peek(ModuleArg).orElse(optProject.flatMap(_.main))

    optModule    =  { for {
                      project  <- optProject
                      moduleId <- optModuleId
                      module   <- project.modules.findBy(moduleId).toOption
                    } yield module }

  } yield Context(cli, layout, layer, schema, optProject, optModule)

  def list(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli     <- cli.hint(RawArg)
      call    <- cli.call()
      raw     <- ~call(RawArg).isSuccess
      project <- optProject.ascribe(UnspecifiedProject())
      module  <- optModule.ascribe(UnspecifiedModule())
      rows    <- ~module.properties.to[List].sorted
      table   <- ~Tables().show(Tables().props, cli.cols, rows, raw)(identity)
      schema  <- defaultSchema

      _       <- ~(if(!raw) log.info(Tables().contextString(layer, layer.showSchema, schema,
                     project, module)))

      _       <- ~log.info(table.mkString("\n"))
    } yield log.await()
  }

  def remove(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli       <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
      cli       <- cli.hint(PropArg, optModule.to[List].flatMap(_.properties.to[List]))
      cli       <- cli.hint(ForceArg)
      call      <- cli.call()
      propArg   <- call(PropArg)
      project   <- optProject.ascribe(UnspecifiedProject())
      module    <- optModule.ascribe(UnspecifiedModule())
      force     <- ~call(ForceArg).isSuccess

      layer     <- Lenses.updateSchemas(optSchemaId, layer, force)(Lenses.layer.properties(_, project.id,
                       module.id))(_(_) -= propArg)

      _         <- Layer.save(layer, layout)
    } yield log.await()
  }

  def add(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      optSchema       <- ~layer.mainSchema.toOption
      importedSchemas  = optSchema.flatMap(_.importedSchemas(ctx.layout, false).toOption)
      allSchemas       = optSchema.toList ::: importedSchemas.toList.flatten
      cli             <- cli.hint(PropArg)
      call            <- cli.call()
      project         <- optProject.ascribe(UnspecifiedProject())
      module          <- optModule.ascribe(UnspecifiedModule())
      propArg         <- call(PropArg)

      layer           <- Lenses.updateSchemas(optSchemaId, layer, true)(Lenses.layer.properties(_, project.id,
                             module.id))(_(_) += propArg)

      _               <- Layer.save(layer, layout)
    } yield log.await()
  }
}
