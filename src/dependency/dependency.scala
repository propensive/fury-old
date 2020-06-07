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

import fury.text._, fury.io._, fury.core._, fury.model._

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
    (cli,
     tryProject,
     tryModule)  <- cli.askProjectAndModule(layer)
    cli          <- cli.hint(RawArg)
    table         = Tables().dependencies
    cli          <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
    cli          <- cli.hint(LinkArg, tryModule.map(_.dependencies).getOrElse(Set.empty))
    call         <- cli.call()
    col           = cli.peek(ColumnArg)
    dep           = cli.peek(LinkArg)
    raw           = call(RawArg).isSuccess
    project      <- tryProject
    module       <- tryModule
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
    (cli,
     tryProject,
     tryModule)  <- cli.askProjectAndModule(layer)

    links         = for(project <- tryProject; module <- tryModule)
                    yield (module.dependencies.filter(_.ref.projectId != project.id).map(_.ref.id) ++
                        module.dependencies.filter(_.ref.projectId == project.id).map(_.ref.moduleId.key))
    
    cli          <- cli.hint(LinkArg, links.getOrElse(Set.empty))    
    cli          <- cli.hint(ForceArg)
    call         <- cli.call()
    linkArg      <- call(LinkArg)
    project      <- tryProject
    module       <- tryModule
    moduleRef    <- ModuleRef.parse(project.id, linkArg, false).ascribe(InvalidValue(linkArg))
    dependency   <- module.dependencies.findBy(moduleRef)
    force        <- ~call(ForceArg).isSuccess
    layer        <- ~Layer(_.projects(project.id).modules(module.id).dependencies).modify(layer)(_ - dependency)
    _            <- Layer.commit(layer, conf, layout)
    _            <- ~Build.asyncBuild(layer, moduleRef, layout)
  } yield log.await()

  def add: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    (cli,
     tryProject,
     tryModule)  <- cli.askProjectAndModule(layer)
    hierarchy    <- layer.hierarchy()
    universe     <- hierarchy.universe
    
    allRefs       = for(project <- tryProject; module <- tryModule)
                    yield {
                      val fromOtherProjects = for {
                        otherRef <- layer.deepModuleRefs(universe)
                                if !(otherRef.hidden || module.dependencies.map(_.ref).contains(otherRef))
                      } yield otherRef.id
                      val fromSameProject = for {
                        otherModule <- project.modules if !module.dependencies.exists(_.ref.moduleId == otherModule.id)
                      } yield otherModule.id.key
                      fromOtherProjects ++ fromSameProject
                    }

    cli          <- cli.hint(LinkArg, allRefs.getOrElse(Set()))
    cli          <- cli.hint(IntransitiveArg)
    call         <- cli.call()
    project      <- tryProject
    module       <- tryModule
    intransitive  = call(IntransitiveArg).isSuccess
    linkArg      <- call(LinkArg)
    ref          <- ModuleRef.parse(project.id, linkArg, intransitive).ascribe(InvalidValue(linkArg))
    layer        <- ~Layer(_.projects(project.id).modules(module.id).dependencies).modify(layer)(_ + Dependency(ref))
    _            <- Layer.commit(layer, conf, layout)
    _            <- ~Build.asyncBuild(layer, ref, layout)
  } yield log.await()
}