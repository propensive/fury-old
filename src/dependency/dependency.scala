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

case class DependencyCli(cli: Cli)(implicit log: Log) {

  def list: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    (cli, tryProject, tryModule) <- cli.askProjectAndModule(layer)
    cli     <- cli.hint(RawArg)
    table   =  Tables().dependencies
    cli     <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
    cli     <- cli.hint(LinkArg, tryModule.map(_.dependencies).getOrElse(Set.empty))
    call    <- cli.call()
    col     =  cli.peek(ColumnArg)
    dep     =  cli.peek(LinkArg)
    raw     =  call(RawArg).isSuccess
    project   <- tryProject
    module    <- tryModule
  } yield {
    val rows = module.dependencies.to[List].sorted
    log.infoWhen(!raw)(conf.focus(project.id, module.id))
    log.rawln(Tables().show(table, cli.cols, rows, raw, col, dep, "dependency"))
    log.await()
  }

  def remove: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    (cli, tryProject, tryModule) <- cli.askProjectAndModule(layer)
    dependencyLinks      = for {
      project  <- tryProject
      module <- tryModule
    } yield {
      val fromOtherProjects = for {
        ref <- module.dependencies if ref.projectId != project.id
      } yield ref.id
      val fromSameProject = for {
        ref <- module.dependencies if ref.projectId == project.id
      } yield ref.moduleId.key
      fromOtherProjects ++ fromSameProject
    }
    cli       <- cli.hint(LinkArg, dependencyLinks.getOrElse(Set.empty))    
    cli       <- cli.hint(ForceArg)
    call      <- cli.call()
    linkArg   <- call(LinkArg)
    project   <- tryProject
    module    <- tryModule
    moduleRef <- ModuleRef.parse(project.id, linkArg, false).ascribe(InvalidValue(linkArg))
    force     <- ~call(ForceArg).isSuccess
    layer     <- ~Layer(_.projects(project.id).modules(module.id).dependencies).modify(layer)(_ - moduleRef)
    _         <- Layer.commit(layer, conf, layout)
  } yield {
    Compilation.asyncCompilation(layer, moduleRef, layout)
    log.await()
  }

  def add: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    (cli, tryProject, tryModule) <- cli.askProjectAndModule(layer)
    hierarchy        <- layer.hierarchy()
    universe         <- hierarchy.universe
    allRefs = for {
      project <- tryProject
      module <- tryModule      
    } yield {
      val fromOtherProjects = for {
        otherRef <- layer.deepModuleRefs(universe) if !(otherRef.hidden || module.dependencies.contains(otherRef))
      } yield otherRef.id
      val fromSameProject = for {
        otherModule <- project.modules if !module.dependencies.exists(_.moduleId == otherModule.id)
      } yield otherModule.id.key
      fromOtherProjects ++ fromSameProject
    }
    cli              <- cli.hint(LinkArg, allRefs.getOrElse(Set.empty))
    cli              <- cli.hint(IntransitiveArg)
    call             <- cli.call()
    project          <- tryProject
    module           <- tryModule
    intransitive     =  call(IntransitiveArg).isSuccess
    linkArg          <- call(LinkArg)
    moduleRef        <- ModuleRef.parse(project.id, linkArg, intransitive).ascribe(InvalidValue(linkArg))

    layer            <- ~Layer(_.projects(project.id).modules(module.id).dependencies).modify(layer)(_ +
                            moduleRef)
    
    _                <- Layer.commit(layer, conf, layout)
  } yield {
    Compilation.asyncCompilation(layer, moduleRef, layout)
    log.await()
  }
}

case class EnvCli(cli: Cli)(implicit log: Log) {
  def list: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    (cli, tryProject, tryModule) <- cli.askProjectAndModule(layer)
    cli          <- cli.hint(RawArg)
    table        <- ~Tables().envs
    cli          <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
    cli          <- cli.hint(EnvArg, tryModule.map(_.environment).getOrElse(Nil))
    call         <- cli.call()
    col          <- ~cli.peek(ColumnArg)
    env          <- ~cli.peek(EnvArg)
    raw          <- ~call(RawArg).isSuccess
    project      <- tryProject
    module       <- tryModule
  } yield {
    val rows = module.environment.to[List].sorted
    log.infoWhen(!raw)(conf.focus(project.id, module.id))
    log.rawln(Tables().show(table, cli.cols, rows, raw, col, env, "id"))
    log.await()
  }

  def remove: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    (cli, tryProject, tryModule) <- cli.askProjectAndModule(layer)
    cli          <- cli.hint(EnvArg, tryModule.map(_.environment).getOrElse(Nil))
    cli          <- cli.hint(ForceArg)
    call         <- cli.call()
    envArg       <- call(EnvArg)
    project      <- tryProject
    module       <- tryModule
    force        <- ~call(ForceArg).isSuccess
    layer        <- ~Layer(_.projects(project.id).modules(module.id).environment).modify(layer)(_ - envArg)
    _            <- Layer.commit(layer, conf, layout)
  } yield log.await()

  def add: Try[ExitStatus] = for {
    layout          <- cli.layout
    conf            <- Layer.readFuryConf(layout)
    layer           <- Layer.retrieve(conf)
    (cli, tryProject, tryModule) <- cli.askProjectAndModule(layer)
    importedLayers  = layer.importedLayers.toOption
    allLayers       = layer :: importedLayers.toList.flatten
    allModules      = allLayers.map(_.moduleRefs).flatten
    cli             <- cli.hint(EnvArg)
    call            <- cli.call()
    project         <- tryProject
    module          <- tryModule
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
    (cli, tryProject, tryModule) <- cli.askProjectAndModule(layer)
    cli             <- cli.hint(ScopeArg, ScopeId.All)
    cli             <- cli.hint(NoGrantArg)
    cli             <- cli.hint(ClassArg, Permission.Classes)
    cli             <- cli.hint(PermissionTargetArg)
    cli             <- cli.hint(ActionArg, List("read", "write", "read,write"))
    call            <- cli.call()
    scopeId         =  call(ScopeArg).getOrElse(ScopeId.Project)
    project         <- tryProject
    module          <- tryModule
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
    (cli, tryProject, tryModule) <- cli.askProjectAndModule(layer)
    cli           <- cli.hint(PermissionArg, tryModule.map(_.policyEntries).getOrElse(Set.empty))
    cli           <- cli.hint(ForceArg)
    call          <- cli.call()
    permHashes    <- call(PermissionArg).map(_.map(PermissionHash(_)))
    project         <- tryProject
    module          <- tryModule
    hierarchy     <- layer.hierarchy()
    universe      <- hierarchy.universe
    compilation   <- Compilation.fromUniverse(universe, module.ref(project), layout)
    permissions   <- permHashes.traverse(_.resolve(compilation.requiredPermissions))
    force         =  call(ForceArg).isSuccess
                         
    layer         <- ~Layer(_.projects(project.id).modules(module.id).policy).modify(layer)(_.diff(
                         permissions.to[Set]))

    _             <- Layer.commit(layer, conf, layout)
  } yield log.await()
  
  def list: Try[ExitStatus] = for {
    layout        <- cli.layout
    conf          <- Layer.readFuryConf(layout)
    layer         <- Layer.retrieve(conf)
    (cli, tryProject, tryModule) <- cli.askProjectAndModule(layer)
    cli           <- cli.hint(RawArg)
    table         <- ~Tables().permissions
    cli           <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
    call          <- cli.call()
    col           <- ~cli.peek(ColumnArg)
    raw           <- ~call(RawArg).isSuccess
    project       <- tryProject
    module        <- tryModule
  } yield {
    val rows = module.policyEntries.to[List].sortBy(_.hash.key)
    log.infoWhen(!raw)(conf.focus(project.id, module.id))
    log.rawln(Tables().show[PermissionEntry, PermissionEntry](table, cli.cols, rows, raw, col, None, "hash"))
    log.await()
  }

  def grant: Try[ExitStatus] = for {
    layout        <- cli.layout
    conf          <- Layer.readFuryConf(layout)
    layer         <- Layer.retrieve(conf)
    (cli, tryProject, tryModule) <- cli.askProjectAndModule(layer)
    cli           <- cli.hint(ScopeArg, ScopeId.All)
    cli           <- cli.hint(PermissionArg, tryModule.map(_.policyEntries).getOrElse(Set.empty))
    call          <- cli.call()
    scopeId       =  call(ScopeArg).getOrElse(ScopeId.Project)
    project       <- tryProject
    module        <- tryModule
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
    (cli, tryProject, tryModule) <- cli.askProjectAndModule(layer)
    cli     <- cli.hint(RawArg)
    table   <- ~Tables().props
    cli     <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
    cli     <- cli.hint(PropArg, tryModule.map(_.properties).getOrElse(Nil))
    call    <- cli.call()
    raw     <- ~call(RawArg).isSuccess
    col     <- ~cli.peek(ColumnArg)
    prop    <- ~cli.peek(PropArg)
    project <- tryProject
    module  <- tryModule    
  } yield {
    val rows = module.properties.to[List].sorted
    log.infoWhen(!raw)(conf.focus(project.id, module.id))
    log.rawln(Tables().show(table, cli.cols, rows, raw, col, prop, "property"))
    log.await()
  }

  def remove: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    (cli, tryProject, tryModule) <- cli.askProjectAndModule(layer)
    cli       <- cli.hint(PropArg, tryModule.map(_.properties).getOrElse(Set.empty))
    cli       <- cli.hint(ForceArg)
    call      <- cli.call()
    propArg   <- call(PropArg)
    project   <- tryProject
    module    <- tryModule
    force     <- ~call(ForceArg).isSuccess
    layer     <- ~Layer(_.projects(project.id).modules(module.id).properties).modify(layer)(_ - propArg)
    _         <- Layer.commit(layer, conf, layout)
  } yield log.await()

  def add: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    (cli, tryProject, tryModule) <- cli.askProjectAndModule(layer)
    importedLayers   = layer.importedLayers.toOption
    allLayers        = layer :: importedLayers.toList.flatten
    cli             <- cli.hint(PropArg)
    call            <- cli.call()
    project   <- tryProject
    module    <- tryModule
    propArg         <- call(PropArg)
    layer           <- ~Layer(_.projects(project.id).modules(module.id).properties).modify(layer)(_ + propArg)
    _               <- Layer.commit(layer, conf, layout)
  } yield log.await()
}
