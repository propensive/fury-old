/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.8.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.                                         ║
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

case class DependencyCli(cli: Cli)(implicit log: Log) {

  def list: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.read(layout, conf)
    schemaArg    <- ~Some(SchemaId.default)
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

    cli     <- cli.hint(RawArg)
    table   <- ~Tables().dependencies
    cli     <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
    cli     <- cli.hint(LinkArg, optModule.map(_.dependencies).getOrElse(Nil))
    call    <- cli.call()
    col     <- ~cli.peek(ColumnArg)
    dep     <- ~cli.peek(LinkArg)
    raw     <- ~call(RawArg).isSuccess
    project <- optProject.asTry
    module  <- optModule.asTry
    rows    <- ~module.dependencies.to[List].sorted
    table   <- ~Tables().show(table, cli.cols, rows, raw, col, dep, "dependency")
    schema  <- layer.schemas.findBy(SchemaId.default)
    _       <- ~log.infoWhen(!raw)(conf.focus(project.id, module.id))
    _       <- ~log.rawln(table)
  } yield log.await()

  def remove: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.read(layout, conf)
    schemaArg    <- ~Some(SchemaId.default)
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

    cli       <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    cli       <- cli.hint(LinkArg, optModule.to[List].flatMap(_.dependencies.to[List]))
    cli       <- cli.hint(ForceArg)
    cli       <- cli.hint(HttpsArg)
    call      <- cli.call()
    https     <- ~call(HttpsArg).isSuccess
    linkArg   <- call(LinkArg)
    project   <- optProject.asTry
    module    <- optModule.asTry
    moduleRef <- ModuleRef.parse(project.id, linkArg, false).ascribe(InvalidValue(linkArg))
    force     <- ~call(ForceArg).isSuccess

    layer     <- Lenses.updateSchemas(layer)(Lenses.layer.dependencies(_, project.id,
                      module.id))(_(_) -= moduleRef)

    _         <- ~Layer.save(layer, layout)
    optSchema <- ~layer.mainSchema.toOption

    _         <- ~optSchema.foreach(Compilation.asyncCompilation(_, moduleRef, layout,
                      https))

  } yield log.await()

  def add: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.read(layout, conf)
    schemaArg    <- ~Some(SchemaId.default)
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

    optSchema       <- ~layer.mainSchema.toOption
    importedSchemas  = optSchema.flatMap(_.importedSchemas(layout, false).toOption)
    allSchemas       = optSchema.toList ::: importedSchemas.toList.flatten
    allModules       = allSchemas.map(_.moduleRefs).flatten
    cli              <- cli.hint(LinkArg, allModules.filter(!_.hidden))
    cli              <- cli.hint(IntransitiveArg)
    call             <- cli.call()
    project          <- optProject.asTry
    module           <- optModule.asTry
    intransitive     <- ~call(IntransitiveArg).isSuccess
    linkArg          <- call(LinkArg)
    moduleRef        <- ModuleRef.parse(project.id, linkArg, intransitive).ascribe(InvalidValue(linkArg))

    layer            <- Lenses.updateSchemas(layer)(Lenses.layer.dependencies(_,
                            project.id, module.id))(_(_) += moduleRef)

    _                <- ~Layer.save(layer, layout)

    _                <- ~optSchema.foreach(Compilation.asyncCompilation(_, moduleRef, layout,
                            false))

  } yield log.await()
}

case class EnvCli(cli: Cli)(implicit log: Log) {
  def list: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.read(layout, conf)
    schemaArg    <- ~Some(SchemaId.default)
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
    cli          <- cli.hint(RawArg)
    table        <- ~Tables().envs
    cli          <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
    cli          <- cli.hint(EnvArg, optModule.map(_.environment).getOrElse(Nil))
    call         <- cli.call()
    col          <- ~cli.peek(ColumnArg)
    env          <- ~cli.peek(EnvArg)
    raw          <- ~call(RawArg).isSuccess
    project      <- optProject.asTry
    module       <- optModule.asTry
    rows         <- ~module.environment.to[List].sorted
    table        <- ~Tables().show(table, cli.cols, rows, raw, col, env, "id")
    _            <- ~log.infoWhen(!raw)(conf.focus(project.id, module.id))
    _            <- ~log.rawln(table)
  } yield log.await()

  def remove: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.read(layout, conf)
    schemaArg    <- ~Some(SchemaId.default)
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

    cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    cli          <- cli.hint(EnvArg, optModule.to[List].flatMap(_.environment.to[List]))
    cli          <- cli.hint(ForceArg)
    call         <- cli.call()
    envArg       <- call(EnvArg)
    project      <- optProject.asTry
    module       <- optModule.asTry
    force        <- ~call(ForceArg).isSuccess
    
    layer        <- Lenses.updateSchemas(layer)(Lenses.layer.environment(_, project.id,
                        module.id))(_(_) -= envArg)

    _            <- ~Layer.save(layer, layout)
    optSchema    <- ~layer.mainSchema.toOption
  } yield log.await()

  def add: Try[ExitStatus] = for {
    layout          <- cli.layout
    conf            <- Layer.readFuryConf(layout)
    layer           <- Layer.read(layout, conf)
    schemaArg       <- ~Some(SchemaId.default)
    schema          <- ~layer.schemas.findBy(schemaArg.getOrElse(layer.main)).toOption
    cli             <- cli.hint(ProjectArg, schema.map(_.projects).getOrElse(Nil))
    optProjectId    <- ~schema.flatMap { s => cli.peek(ProjectArg).orElse(s.main) }
    optProject      <- ~schema.flatMap { s => optProjectId.flatMap(s.projects.findBy(_).toOption) }
    cli             <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    optModuleId     <- ~cli.peek(ModuleArg).orElse(optProject.flatMap(_.main))

    optModule       <- Success { for {
                         project  <- optProject
                         moduleId <- optModuleId
                         module   <- project.modules.findBy(moduleId).toOption
                       } yield module }
    optSchema       <- ~layer.mainSchema.toOption

    importedSchemas  = optSchema.flatMap(_.importedSchemas(layout, false).toOption)
    allSchemas       = optSchema.toList ::: importedSchemas.toList.flatten
    allModules       = allSchemas.map(_.moduleRefs).flatten
    cli             <- cli.hint(EnvArg)
    call            <- cli.call()
    project         <- optProject.asTry
    module          <- optModule.asTry
    envArg          <- call(EnvArg)

    layer           <- Lenses.updateSchemas(layer)(Lenses.layer.environment(_, project.id,
                            module.id))(_(_) += envArg)

    _               <- ~Layer.save(layer, layout)
  } yield log.await()
}

case class PermissionCli(cli: Cli)(implicit log: Log) {
  
  def require: Try[ExitStatus] = for {
    layout          <- cli.layout
    conf            <- Layer.readFuryConf(layout)
    layer           <- Layer.read(layout, conf)
    schemaArg       <- ~Some(SchemaId.default)
    schema          <- ~layer.schemas.findBy(schemaArg.getOrElse(layer.main)).toOption
    cli             <- cli.hint(ProjectArg, schema.map(_.projects).getOrElse(Nil))
    optProjectId    <- ~schema.flatMap { s => cli.peek(ProjectArg).orElse(s.main) }
    optProject      <- ~schema.flatMap { s => optProjectId.flatMap(s.projects.findBy(_).toOption) }
    cli             <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    optModuleId     <- ~cli.peek(ModuleArg).orElse(optProject.flatMap(_.main))

    optModule       <- Success { for {
                         project  <- optProject
                         moduleId <- optModuleId
                         module   <- project.modules.findBy(moduleId).toOption
                       } yield module }

    cli             <- cli.hint(ScopeArg, ScopeId.All)
    cli             <- cli.hint(NoGrantArg)
    cli             <- cli.hint(ClassArg, Permission.Classes)
    cli             <- cli.hint(PermissionTargetArg)
    cli             <- cli.hint(ActionArg, List("read", "write", "read,write"))
    call            <- cli.call()
    scopeId         =  call(ScopeArg).getOrElse(ScopeId.Project)
    project         <- optProject.asTry
    module          <- optModule.asTry
    classArg        <- call(ClassArg)
    targetArg       <- call(PermissionTargetArg)
    actionArg       =  call(ActionArg).toOption
    grant           =  call(NoGrantArg).isFailure
    permission      =  Permission(classArg, targetArg, actionArg)
    layer           <- Lenses.updateSchemas(layer)(Lenses.layer.policy(_, project.id,
                            module.id))(_(_) += permission)
    _               <- Layer.save(layer, layout)
    policy          <- ~Policy.read(log)
    newPolicy       =  if(grant) policy.grant(Scope(scopeId, layout, project.id), List(permission)) else policy
    _               <- Policy.save(newPolicy)
  } yield {
    log.info(msg"${PermissionHash(permission.hash)}")
    log.await()
  }

  def obviate: Try[ExitStatus] = for {
    layout        <- cli.layout
    conf          <- Layer.readFuryConf(layout)
    layer         <- Layer.read(layout, conf)
    schemaArg     <- ~Some(SchemaId.default)
    schema        <- ~layer.schemas.findBy(schemaArg.getOrElse(layer.main)).toOption
    cli           <- cli.hint(ProjectArg, schema.map(_.projects).getOrElse(Nil))
    optProjectId  <- ~schema.flatMap { s => cli.peek(ProjectArg).orElse(s.main) }
    optProject    <- ~schema.flatMap { s => optProjectId.flatMap(s.projects.findBy(_).toOption) }
    cli           <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    optModuleId   <- ~cli.peek(ModuleArg).orElse(optProject.flatMap(_.main))

    optModule     <- Success { for {
                       project  <- optProject
                       moduleId <- optModuleId
                       module   <- project.modules.findBy(moduleId).toOption
                     } yield module }

    cli           <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    cli           <- cli.hint(PermissionArg, optModule.to[List].flatMap(_.policyEntries))
    cli           <- cli.hint(ForceArg)
    call          <- cli.call()
    permHashes    <- call(PermissionArg).map(_.map(PermissionHash(_)))
    project       <- optProject.asTry
    module        <- optModule.asTry
    schema        <- layer.schemas.findBy(layer.main)
    hierarchy     <- schema.hierarchy(layout)
    universe      <- hierarchy.universe
    compilation   <- Compilation.fromUniverse(universe, module.ref(project), layout)
    permissions   <- permHashes.traverse(_.resolve(compilation.requiredPermissions))
    force         =  call(ForceArg).isSuccess
    layer         <- Lenses.updateSchemas(layer)(Lenses.layer.policy(_, project.id,
                          module.id))((x, y) => x(y) = x(y) diff permissions.to[Set])
    _             <- Layer.save(layer, layout)
  } yield log.await()
  
  def list: Try[ExitStatus] = for {
    layout        <- cli.layout
    conf          <- Layer.readFuryConf(layout)
    layer         <- Layer.read(layout, conf)
    schemaArg     <- ~Some(SchemaId.default)
    schema        <- ~layer.schemas.findBy(schemaArg.getOrElse(layer.main)).toOption
    cli           <- cli.hint(ProjectArg, schema.map(_.projects).getOrElse(Nil))
    optProjectId  <- ~schema.flatMap { s => cli.peek(ProjectArg).orElse(s.main) }
    optProject    <- ~schema.flatMap { s => optProjectId.flatMap(s.projects.findBy(_).toOption) }
    cli           <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    optModuleId   <- ~cli.peek(ModuleArg).orElse(optProject.flatMap(_.main))

    optModule     <- Success { for {
                       project  <- optProject
                       moduleId <- optModuleId
                       module   <- project.modules.findBy(moduleId).toOption
                     } yield module }
    
    cli           <- cli.hint(RawArg)
    table         <- ~Tables().permissions
    cli           <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
    call          <- cli.call()
    col           <- ~cli.peek(ColumnArg)
    raw           <- ~call(RawArg).isSuccess
    project       <- optProject.asTry
    module        <- optModule.asTry
    rows          <- ~module.policyEntries.to[List].sortBy(_.hash.key)
    table         <- ~Tables().show[PermissionEntry, PermissionEntry](table, cli.cols, rows, raw, col, None, "hash")
    _             <- ~log.infoWhen(!raw)(conf.focus(project.id, module.id))
    _             <- ~log.rawln(table)
  } yield log.await()

  def grant: Try[ExitStatus] = for {
    layout        <- cli.layout
    conf          <- Layer.readFuryConf(layout)
    layer         <- Layer.read(layout, conf)
    schemaArg     <- ~Some(SchemaId.default)
    schema        <- ~layer.schemas.findBy(schemaArg.getOrElse(layer.main)).toOption
    cli           <- cli.hint(ProjectArg, schema.map(_.projects).getOrElse(Nil))
    optProjectId  <- ~schema.flatMap { s => cli.peek(ProjectArg).orElse(s.main) }
    optProject    <- ~schema.flatMap { s => optProjectId.flatMap(s.projects.findBy(_).toOption) }
    cli           <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    optModuleId   <- ~cli.peek(ModuleArg).orElse(optProject.flatMap(_.main))

    optModule     <- Success { for {
                       project  <- optProject
                       moduleId <- optModuleId
                       module   <- project.modules.findBy(moduleId).toOption
                     } yield module }
    cli           <- cli.hint(ScopeArg, ScopeId.All)
    
    //TODO check if hints still work
    cli           <- cli.hint(PermissionArg, optModule.to[List].flatMap(_.policyEntries))
    call          <- cli.call()
    scopeId       =  call(ScopeArg).getOrElse(ScopeId.Project)
    project       <- optProject.asTry
    module        <- optModule.asTry
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

case class PropertyCli(cli: Cli)(implicit log: Log) {
  def list: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.read(layout, conf)
    schemaArg    <- ~Some(SchemaId.default)
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

    cli     <- cli.hint(RawArg)
    table   <- ~Tables().props
    cli     <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
    cli     <- cli.hint(PropArg, optModule.map(_.properties).getOrElse(Nil))
    call    <- cli.call()
    raw     <- ~call(RawArg).isSuccess
    col     <- ~cli.peek(ColumnArg)
    prop    <- ~cli.peek(PropArg)
    project <- optProject.asTry
    module  <- optModule.asTry
    rows    <- ~module.properties.to[List].sorted
    table   <- ~Tables().show(table, cli.cols, rows, raw, col, prop, "property")
    _       <- ~log.infoWhen(!raw)(conf.focus(project.id, module.id))
    _       <- ~log.rawln(table)
  } yield log.await()

  def remove: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.read(layout, conf)
    schemaArg    <- ~Some(SchemaId.default)
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

    cli       <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    cli       <- cli.hint(PropArg, optModule.to[List].flatMap(_.properties.to[List]))
    cli       <- cli.hint(ForceArg)
    call      <- cli.call()
    propArg   <- call(PropArg)
    project   <- optProject.asTry
    module    <- optModule.asTry
    force     <- ~call(ForceArg).isSuccess

    layer     <- Lenses.updateSchemas(layer)(Lenses.layer.properties(_, project.id,
                      module.id))(_(_) -= propArg)

    _         <- Layer.save(layer, layout)
  } yield log.await()

  def add: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.read(layout, conf)
    schemaArg    <- ~Some(SchemaId.default)
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

    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.read(layout, conf)
    schemaArg    <- ~Some(SchemaId.default)
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

    optSchema       <- ~layer.mainSchema.toOption
    importedSchemas  = optSchema.flatMap(_.importedSchemas(layout, false).toOption)
    allSchemas       = optSchema.toList ::: importedSchemas.toList.flatten
    cli             <- cli.hint(PropArg)
    call            <- cli.call()
    project         <- optProject.asTry
    module          <- optModule.asTry
    propArg         <- call(PropArg)

    layer           <- Lenses.updateSchemas(layer)(Lenses.layer.properties(_, project.id,
                            module.id))(_(_) += propArg)

    _               <- Layer.save(layer, layout)
  } yield log.await()
}
