/*

    Fury, version 0.17.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

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
import optometry._

import Args._

import scala.util._
import scala.collection.immutable._

case class SourceCli(cli: Cli)(implicit log: Log) {
  def list: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    (cli, tryProject, tryModule) <- cli.askProjectAndModule(layer)
    cli          <- cli.hint(RawArg)
    cli          <- cli.hint(ColumnArg, List("repo", "path", "sources", "files", "size", "lines"))
    cli          <- cli.hint(SourceArg, tryModule.map(_.sources).getOrElse(Nil))
    call         <- cli.call()
    source       <- ~cli.peek(SourceArg)
    col          <- ~cli.peek(ColumnArg)
    raw          <- ~call(RawArg).isSuccess
    project      <- tryProject
    module       <- tryModule
    hierarchy    <- layer.hierarchy()
    universe     <- hierarchy.universe
    checkouts    <- universe.checkout(module.ref(project), hierarchy, layout)
  } yield {
    val rows = module.sources.to[List]
    val table = Tables().sources(checkouts, layout)
    log.info(conf.focus(project.id, module.id))
    log.rawln(Tables().show(table, cli.cols, rows, raw, col, source, "repo"))
    log.await()
  }

  def remove: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    (cli, tryProject, tryModule) <- cli.askProjectAndModule(layer)

    cli          <- cli.hint(SourceArg, tryModule.map(_.sources).getOrElse(Set.empty).map(_.completion))
    call         <- cli.call()
    source       <- call(SourceArg)
    project      <- tryProject
    module       <- tryModule
    
    _            <- if(!module.sources.contains(source)) Failure(InvalidSource(source, module.ref(project)))
                        else Success(())
    _            <- layout.classesDir(module.ref(project)).delete()
    layer        <- ~Layer(_.projects(project.id).modules(module.id).sources).modify(layer)(_ - source)
    _            <- Layer.commit(layer, conf, layout)
    _            <- ~Build.asyncBuild(layer, module.ref(project), layout)
  } yield log.await()

  def add: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    (cli, tryProject, tryModule) <- cli.askProjectAndModule(layer)
    extSrcs      = layer.repos.map(possibleSourceDirectories(_, layout)).flatten
    localSrcs    = layout.pwd.relativeSubdirsContaining(isSourceFileName).map(LocalSource(_, Glob.All))
    cli          <- cli.hint(SourceArg, (extSrcs ++ localSrcs).map(_.completion))
    call         <- cli.call()

    project      <- tryProject
    module       <- tryModule
    source       <- call(SourceArg)

    localId      =  for {
                      localRepo  <- Remote.local(layout).toOption
                      layerMatch <- layer.repos.find(_.remote.simplified == localRepo.simplified)
                    } yield layerMatch.id

    source       <- ~Source.rewriteLocal(source, localId)
    layer        <- ~Layer(_.projects(project.id).modules(module.id).sources).modify(layer)(_ ++ Some(source))
    _            <- Layer.commit(layer, conf, layout)
    _            <- ~Build.asyncBuild(layer, module.ref(project), layout)
  } yield log.await()

  private[this] def isSourceFileName(name: String): Boolean = name.endsWith(".scala") || name.endsWith(".java")

  private[this] def possibleSourceDirectories(repo: Repo, layout: Layout) =
    repo.sourceCandidates(layout)(isSourceFileName).getOrElse(Set.empty)
}

case class ResourceCli(cli: Cli)(implicit val log: Log) extends CliApi {

  lazy val table = Tables().resources
  implicit val columnHints: ColumnArg.Hinter = ColumnArg.hint(~table.headings.map(_.name.toLowerCase))

  private def removeFromSet[T](items: SortedSet[T], item: T): SortedSet[T] = items - item
  private def addToSet[T](items: SortedSet[T], item: T): SortedSet[T] = items + item
  
  lazy val resources = getModule >> (_.resources)
  
  def resourcesLens(project: Project, module: Module) =
    Lens[Layer](_.projects(project.id).modules(module.id).resources)

  def list: Try[ExitStatus] =
    (cli -< ProjectArg -< RawArg -< ModuleArg -< ColumnArg -< ResourceArg).action {
      if(!raw) (conf, getProject >> (_.id), getModule >> (_.id)) >> (_.focus(_, _)) >> (log.info(_))

      
      resources >> (Tables().show(table, cli.cols, _, raw, column, get(ResourceArg).toOption, "resource")) >>
          (log.rawln(_)) >> { x => log.await() }
    }

  def remove: Try[ExitStatus] = (cli -< ProjectArg -< ModuleArg -< ResourceArg).action {
    val lens = (getProject, getModule) >> resourcesLens

    (resources, get(ResourceArg)) >> removeFromSet >>= { resources =>
      (getLayer, lens) >> Layer.set(resources) >> commit >> finish
    }
  }

  def add: Try[ExitStatus] = (cli -< ProjectArg -< ModuleArg -< ResourceArg).action {
    val lens = (getProject, getModule) >> resourcesLens
    (resources, get(ResourceArg)) >> addToSet >>= { resources =>
      (getLayer, lens) >> Layer.set(resources) >> commit >> finish
    }
  }
}
