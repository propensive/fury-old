/*

    Fury, version 0.12.3. Copyright 2018-20 Jon Pretty, Propensive OÜ.

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

case class DependencyCli(cli: Cli)(implicit log: Log) {

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
    _       <- ~log.infoWhen(!raw)(conf.focus(project.id, module.id))
    _       <- ~log.rawln(table)
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

    optModule    =  (for {
      project  <- optProject
      module   <- project.modules.findBy(moduleId).toOption
    } yield module)

    cli       <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    deps      <- ~optModule.to[Set].flatMap(_.dependencies.to[Set])
    cli       <- cli.hint(LinkArg, deps.map(_.id) ++
                     optProject.to[Set].flatMap { p => deps.filter(_.projectId == p.id) }.map(_.moduleId.key))
    cli       <- cli.hint(ForceArg)
    cli       <- cli.hint(HttpsArg)
    call      <- cli.call()
    https     <- ~call(HttpsArg).isSuccess
    linkArg   <- call(LinkArg)
    project   <- optProject.asTry
    module    <- optModule.asTry
    moduleRef <- ModuleRef.parse(project.id, linkArg, false).ascribe(InvalidValue(linkArg))
    force     <- ~call(ForceArg).isSuccess
    layer     <- ~Layer(_.projects(project.id).modules(module.id).dependencies).modify(layer)(_ - moduleRef)
    _         <- Layer.commit(layer, conf, layout)
    _         <- ~Compilation.asyncCompilation(layer, moduleRef, layout, https)
  } yield log.await()

  def add: Try[ExitStatus] = for {
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
    
    hierarchy        <- layer.hierarchy()
    universe         <- hierarchy.universe
    allModuleRefs    <- ~layer.deepModuleRefs(universe)
    deps             <- ~optModule.to[Set].flatMap(_.dependencies)
    allRefs          <- ~(allModuleRefs.filter(!_.hidden).filterNot(deps.contains).map(_.id) ++
                            optProject.to[Set].flatMap(_.modules.filterNot { m =>
                            deps.exists(_.moduleId == m.id) }.map(_.id.key)))
    cli              <- cli.hint(LinkArg, allRefs)
    cli              <- cli.hint(IntransitiveArg)
    call             <- cli.call()
    project          <- optProject.asTry
    module           <- optModule.asTry
    intransitive     <- ~call(IntransitiveArg).isSuccess
    linkArg          <- call(LinkArg)
    moduleRef        <- ModuleRef.parse(project.id, linkArg, intransitive).ascribe(InvalidValue(linkArg))
    layer            <- ~Layer(_.projects(project.id).modules(module.id).dependencies).modify(layer)(_ + moduleRef)
    _                <- Layer.commit(layer, conf, layout)
    _                <- ~Compilation.asyncCompilation(layer, moduleRef, layout, false)
  } yield log.await()
}

case class EnvCli(cli: Cli)(implicit log: Log) {
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
    layer        <- Layer.retrieve(conf)
    cli          <- cli.hint(ProjectArg, layer.projects)
    optProjectId <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject   <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
    cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    moduleId     <- cli.preview(ModuleArg)(optProject.flatMap(_.main))

    optModule    <- ~(for {
                      project  <- optProject
                      module   <- project.modules.findBy(moduleId).toOption
                    } yield module)

    cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    cli          <- cli.hint(EnvArg, optModule.to[List].flatMap(_.environment.to[List]))
    cli          <- cli.hint(ForceArg)
    call         <- cli.call()
    envArg       <- call(EnvArg)
    project      <- optProject.asTry
    module       <- optModule.asTry
    force        <- ~call(ForceArg).isSuccess
    layer        <- ~Layer(_.projects(project.id).modules(module.id).environment).modify(layer)(_ - envArg)
    _            <- Layer.commit(layer, conf, layout)
  } yield log.await()

  def add: Try[ExitStatus] = for {
    layout          <- cli.layout
    conf            <- Layer.readFuryConf(layout)
    layer           <- Layer.retrieve(conf)
    cli             <- cli.hint(ProjectArg, layer.projects)
    optProjectId    <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject      <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
    cli             <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    moduleId        <- cli.preview(ModuleArg)(optProject.flatMap(_.main))

    optModule       =  (for {
      project  <- optProject
      module   <- project.modules.findBy(moduleId).toOption
    } yield module)

    importedLayers  = layer.importedLayers.toOption
    allLayers       = layer :: importedLayers.toList.flatten
    allModules       = allLayers.map(_.moduleRefs).flatten
    cli             <- cli.hint(EnvArg)
    call            <- cli.call()
    project         <- optProject.asTry
    module          <- optModule.asTry
    envArg          <- call(EnvArg)
    layer           <- ~Layer(_.projects(project.id).modules(module.id).environment).modify(layer)(_ + envArg)
    _               <- Layer.commit(layer, conf, layout)
  } yield log.await()
}

case class PermissionCli(cli: Cli)(implicit log: Log) {
  
  def require: Try[ExitStatus] = for {
    layout          <- cli.layout
    conf            <- Layer.readFuryConf(layout)
    layer           <- Layer.retrieve(conf)
    cli             <- cli.hint(ProjectArg, layer.projects)
    optProjectId    <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject      <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
    cli             <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    moduleId        <- cli.preview(ModuleArg)(optProject.flatMap(_.main))

    optModule       =  (for {
      project  <- optProject
      module   <- project.modules.findBy(moduleId).toOption
    } yield module)

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
    permission      =  Permission(str"${classArg.key}:${targetArg}", actionArg)
    layer           <- ~Layer(_.projects(project.id).modules(module.id).policy).modify(layer)(_ +
                           permission)
    _               <- Layer.commit(layer, conf, layout)
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
    layer         <- Layer.retrieve(conf)
    cli           <- cli.hint(ProjectArg, layer.projects)
    optProjectId  <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject    <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
    cli           <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    moduleId      <- cli.preview(ModuleArg)(optProject.flatMap(_.main))

    optModule     =  (for {
      project  <- optProject
      module   <- project.modules.findBy(moduleId).toOption
    } yield module)

    cli           <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    cli           <- cli.hint(PermissionArg, optModule.to[List].flatMap(_.policyEntries))
    cli           <- cli.hint(ForceArg)
    call          <- cli.call()
    permHashes    <- call(PermissionArg).map(_.map(PermissionHash(_)))
    project       <- optProject.asTry
    module        <- optModule.asTry
    hierarchy     <- layer.hierarchy()
    universe      <- hierarchy.universe
    compilation   <- Compilation.fromUniverse(universe, module.ref(project), layout)
    permissions   <- permHashes.traverse(_.resolve(compilation.requiredPermissions))
    force         =  call(ForceArg).isSuccess
    layer         <- ~Layer(_.projects(project.id).modules(module.id).policy).modify(layer)(_.diff(permissions.to[Set]))
    _             <- Layer.commit(layer, conf, layout)
  } yield log.await()
  
  def list: Try[ExitStatus] = for {
    layout        <- cli.layout
    conf          <- Layer.readFuryConf(layout)
    layer         <- Layer.retrieve(conf)
    cli           <- cli.hint(ProjectArg, layer.projects)
    optProjectId  <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject    <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
    cli           <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    moduleId      <- cli.preview(ModuleArg)(optProject.flatMap(_.main))

    optModule     =  (for {
      project  <- optProject
      module   <- project.modules.findBy(moduleId).toOption
    } yield module)
    
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
    layer         <- Layer.retrieve(conf)
    cli           <- cli.hint(ProjectArg, layer.projects)
    optProjectId  <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject    <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
    cli           <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    moduleId      <- cli.preview(ModuleArg)(optProject.flatMap(_.main))

    optModule     =  (for {
      project  <- optProject
      module   <- project.modules.findBy(moduleId).toOption
    } yield module)
    cli           <- cli.hint(ScopeArg, ScopeId.All)
    
    //TODO check if hints still work
    cli           <- cli.hint(PermissionArg, optModule.to[List].flatMap(_.policyEntries))
    call          <- cli.call()
    scopeId       =  call(ScopeArg).getOrElse(ScopeId.Project)
    project       <- optProject.asTry
    module        <- optModule.asTry
    permHashes    <- call(PermissionArg).map(_.map(PermissionHash(_)))
    hierarchy     <- layer.hierarchy()
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
    layer        <- Layer.retrieve(conf)
    cli          <- cli.hint(ProjectArg, layer.projects)
    optProjectId <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject   <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
    cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    moduleId      <- cli.preview(ModuleArg)(optProject.flatMap(_.main))

    optModule     =  (for {
      project  <- optProject
      module   <- project.modules.findBy(moduleId).toOption
    } yield module)

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
    layer        <- Layer.retrieve(conf)
    cli          <- cli.hint(ProjectArg, layer.projects)
    optProjectId <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject   <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
    cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    moduleId      <- cli.preview(ModuleArg)(optProject.flatMap(_.main))

    optModule     =  (for {
      project  <- optProject
      module   <- project.modules.findBy(moduleId).toOption
    } yield module)

    cli       <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    cli       <- cli.hint(PropArg, optModule.to[List].flatMap(_.properties.to[List]))
    cli       <- cli.hint(ForceArg)
    call      <- cli.call()
    propArg   <- call(PropArg)
    project   <- optProject.asTry
    module    <- optModule.asTry
    force     <- ~call(ForceArg).isSuccess
    layer     <- ~Layer(_.projects(project.id).modules(module.id).properties).modify(layer)(_ - propArg)
    _         <- Layer.commit(layer, conf, layout)
  } yield log.await()

  def add: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    cli          <- cli.hint(ProjectArg, layer.projects)
    optProjectId <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject   <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
    cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    moduleId      <- cli.preview(ModuleArg)(optProject.flatMap(_.main))

    optModule     =  (for {
      project  <- optProject
      module   <- project.modules.findBy(moduleId).toOption
    } yield module)

    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    cli          <- cli.hint(ProjectArg, layer.projects)
    optProjectId <- ~cli.peek(ProjectArg).orElse(layer.main)
    optProject   <- ~optProjectId.flatMap(layer.projects.findBy(_).toOption)
    cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    moduleId      <- cli.preview(ModuleArg)(optProject.flatMap(_.main))

    optModule     =  (for {
      project  <- optProject
      module   <- project.modules.findBy(moduleId).toOption
    } yield module)

    importedLayers   = layer.importedLayers.toOption
    allLayers        = layer :: importedLayers.toList.flatten
    cli             <- cli.hint(PropArg)
    call            <- cli.call()
    project         <- optProject.asTry
    module          <- optModule.asTry
    propArg         <- call(PropArg)
    layer           <- ~Layer(_.projects(project.id).modules(module.id).properties).modify(layer)(_ + propArg)
    _               <- Layer.commit(layer, conf, layout)
  } yield log.await()
}
