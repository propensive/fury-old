/*

    Fury, version 0.16.1. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury

import fury.model._, fury.core._, fury.text._, Args._

import mercator._

import scala.util._

case class PermissionCli(cli: Cli)(implicit log: Log) {
  
  def require: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    (cli,
     tryProject,
     tryModule)  <- cli.askProjectAndModule(layer)
    cli          <- cli.hint(ScopeArg, ScopeId.All)
    cli          <- cli.hint(NoGrantArg)
    cli          <- cli.hint(ClassArg, Permission.Classes)
    cli          <- cli.hint(PermissionTargetArg)
    cli          <- cli.hint(ActionArg, List("read", "write", "read,write"))
    call         <- cli.call()
    scopeId      =  call(ScopeArg).getOrElse(ScopeId.Project)
    project      <- tryProject
    module       <- tryModule
    classArg     <- call(ClassArg)
    targetArg    <- call(PermissionTargetArg)
    actionArg    =  call(ActionArg).toOption
    grant        =  call(NoGrantArg).isFailure
    permission   =  Permission(str"${classArg.key}:${targetArg}", actionArg)
    layer        <- ~Layer(_.projects(project.id).modules(module.id).policy).modify(layer)(_ + permission)
    _            <- Layer.commit(layer, conf, layout)
    policy       <- ~Policy.read(log)
    newPolicy    =  if(grant) policy.grant(Scope(scopeId, layout, project.id), List(permission)) else policy
    _            <- Policy.save(newPolicy)
  } yield {
    log.info(msg"${PermissionHash(permission.hash)}")
    log.await()
  }

  def obviate: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    (cli,
     tryProject,
     tryModule)  <- cli.askProjectAndModule(layer)
    cli          <- cli.hint(PermissionArg, tryModule.map(_.policyEntries).getOrElse(Set.empty))
    cli          <- cli.hint(ForceArg)
    call         <- cli.call()
    permHashes   <- call(PermissionArg).map(_.map(PermissionHash(_)))
    project      <- tryProject
    module       <- tryModule
    hierarchy    <- layer.hierarchy()
    universe     <- hierarchy.universe
    build        <- Build(universe, Dependency(module.ref(project)), layout)
    permissions  <- permHashes.traverse(_.resolve(build.requiredPermissions))
    force        =  call(ForceArg).isSuccess
                         
    layer        <- ~Layer(_.projects(project.id).modules(module.id).policy).modify(layer)(_.diff(
                        permissions.to[Set]))

    _            <- Layer.commit(layer, conf, layout)
  } yield log.await()
  
  def list: Try[ExitStatus] = for {
    layout        <- cli.layout
    conf          <- Layer.readFuryConf(layout)
    layer         <- Layer.retrieve(conf)
    (cli,
     tryProject, 
     tryModule)   <- cli.askProjectAndModule(layer)
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
    (cli,
     tryProject,
     tryModule)   <- cli.askProjectAndModule(layer)
    cli           <- cli.hint(ScopeArg, ScopeId.All)
    cli           <- cli.hint(PermissionArg, tryModule.map(_.policyEntries).getOrElse(Set.empty))
    call          <- cli.call()
    scopeId       =  call(ScopeArg).getOrElse(ScopeId.Project)
    project       <- tryProject
    module        <- tryModule
    permHashes    <- call(PermissionArg).map(_.map(PermissionHash(_)))
    hierarchy     <- layer.hierarchy()
    universe      <- hierarchy.universe
    build         <- Build(universe, Dependency(module.ref(project)), layout)
    permissions   <- permHashes.traverse(_.resolve(build.requiredPermissions))
    policy        =  Policy.read(log)
    newPolicy     =  policy.grant(Scope(scopeId, layout, project.id), permissions)
    _             <- Policy.save(newPolicy)
  } yield log.await()
}