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
    hierarchy    <- layer.hierarchy
    universe     <- hierarchy.universe
    checkouts    <- universe.checkout(module.ref(project), layout)
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

    layer        <- ~Layer(_.projects(project.id).modules(module.id).sources).modify(layer)(_ - source)
    _            <- Layer.commit(layer, conf, layout)
    _            <- ~Compilation.asyncCompilation(layer, module.ref(project), layout)
  } yield log.await()

  def add: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    (cli, tryProject, tryModule) <- cli.askProjectAndModule(layer)

    extSrcs      = layer.repos.map(possibleSourceDirectories(_, layout)).flatten
    localSrcs    = layout.pwd.relativeSubdirsContaining(isSourceFileName).map(LocalSource(_, Glob.All))
    sharedSrcs   = layout.sharedDir.relativeSubdirsContaining(isSourceFileName).map(SharedSource(_, Glob.All))

    cli          <- cli.hint(SourceArg, (extSrcs ++ localSrcs ++ sharedSrcs).map(_.completion))
    call         <- cli.call()
    project      <- tryProject
    module       <- tryModule
    source       <- call(SourceArg)

    localId      =  for {
                      localRepo <- Remote.local(layout).toOption
                      layerMatch <- layer.repos.find(_.remote.simplified == localRepo.simplified)
                    } yield layerMatch.id

    source       <- ~Source.rewriteLocal(source, localId)
    layer        <- ~Layer(_.projects(project.id).modules(module.id).sources).modify(layer)(_ ++ Some(source))
    _            <- Layer.commit(layer, conf, layout)
    _            <- ~Compilation.asyncCompilation(layer, module.ref(project), layout)
  } yield log.await()

  private[this] def isSourceFileName(name: String): Boolean = name.endsWith(".scala") || name.endsWith(".java")

  private[this] def possibleSourceDirectories(repo: Repo, layout: Layout) =
    repo.sourceCandidates(layout)(isSourceFileName).getOrElse(Set.empty)
}

case class FrontEnd(cli: Cli)(implicit log: Log) {
 
  lazy val layout: Try[Layout] = cli.layout
  lazy val conf: Try[FuryConf] = layout >>= Layer.readFuryConf
  lazy val layer: Try[Layer] = conf >>= (Layer.retrieve(_))

  lazy val projectId: Try[ProjectId] = layer >>= (cli.preview(ProjectArg)() orElse _.main.asTry)
  lazy val project: Try[Project] = (layer, projectId) >>= (_.projects.findBy(_))
  lazy val moduleId: Try[ModuleId] = project >>= (cli.preview(ModuleArg)() orElse _.main.asTry)
  lazy val module: Try[Module] = (project, moduleId) >>= (_.modules.findBy(_))

  implicit val projectHints: ProjectArg.Hinter = ProjectArg.hint(layer >> (_.projects.map(_.id)))
  implicit val moduleHints: ModuleArg.Hinter = ModuleArg.hint(project >> (_.modules.map(_.id)))
  implicit val sourceHints: SourceArg.Hinter = SourceArg.hint(module >> (_.resources))
  implicit val rawHints: RawArg.Hinter = RawArg.hint(())

  implicit val showUnit: StringShow[Unit] = _ => "*"

  lazy val resources: Try[SortedSet[Source]] = module >> (_.resources)

  def resourcesLens(projectId: ProjectId, moduleId: ModuleId) =
    Lens[Layer](_.projects(projectId).modules(moduleId).resources)

  lazy val resource: Try[Source] = cli.preview(SourceArg)()

  private def removeFromSet[T](items: SortedSet[T], item: T): SortedSet[T] = items - item
  private def addToSet[T](items: SortedSet[T], item: T): SortedSet[T] = items + item
  private def commit(layer: Layer): Try[Unit] = (Try(layer), conf, layout) >>= (Layer.commit(_, _, _))
  private def finish[T](result: T): ExitStatus = log.await()

  object Resources {
    lazy val table = Tables().resources
    implicit val columnHints: ColumnArg.Hinter = ColumnArg.hint(~table.headings.map(_.name.toLowerCase))
    
    def list: Try[ExitStatus] = {
      (cli -< ProjectArg -< RawArg -< ModuleArg -< ColumnArg -< SourceArg).action { implicit call =>
        val raw = RawArg().isSuccess
        val column = ColumnArg().toOption
        if(!raw) (conf, projectId, moduleId) >> (_.focus(_, _)) >> (log.info(_))

        
        resources >> (Tables().show(table, cli.cols, _, raw, column, resource.toOption, "source")) >>
            (log.rawln(_)) >> finish
      }
    }

    def remove: Try[ExitStatus] = {
      implicit val sourceHints: SourceArg.Hinter = SourceArg.hint()
      (cli -< ProjectArg -< ModuleArg -< SourceArg).action { implicit call =>
        val lens = (projectId, moduleId) >> resourcesLens
        (resources, SourceArg()) >> removeFromSet >>= { resources =>
          (layer, lens) >> Layer.set(resources) >> commit >> finish
        }
      }
    }

    def add: Try[ExitStatus] = (cli -< ProjectArg -< ModuleArg -< SourceArg).action { implicit call =>
      val lens = (projectId, moduleId) >> resourcesLens
      (resources, SourceArg()) >> addToSet >>= { resources =>
        (layer, lens) >> Layer.set(resources) >> commit >> finish
      }
    }
  }
}
