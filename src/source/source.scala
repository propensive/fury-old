/*

    Fury, version 0.18.28. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury

import fury.text._, fury.core._, fury.model._, fury.io._

import mercator._
import optometry._

import Args._

import scala.util._
import scala.collection.immutable._

case class SourceCli(cli: Cli)(implicit val log: Log) extends CliApi {

  def list: Try[ExitStatus] = {
    implicit val columns: ColumnArg.Hinter = ColumnArg.hint("repo", "path", "sources", "files", "size", "lines")
    implicit val sourcesHint = existingSourcesHint
    (cli -< ProjectArg -< ModuleArg -< SourceArg -< ColumnArg -< RawArg).action {
      (getProject, getModule) >>= { case (project, module) =>
        val snapshots = (universe, getLayout) >>= (_.checkout(module.ref(project), _))
        val tabulation = (snapshots, getLayout) >> (Tables().sources(_, _))
        (tabulation, conf, opt(ColumnArg), opt(SourceArg)) >> { case (table, c, col, source) =>
          log.info(c.focus(project.id, module.id))
          log.rawln(Tables().show(table, cli.cols, module.sources.to[List], raw, col, source, "repo"))
        } >> finish
      }
    }
  }

  def remove: Try[ExitStatus] = {
    implicit val sourcesHint = existingSourcesHint
    (cli -< ProjectArg -< ModuleArg -< SourceArg).action {
      val newSources = (getModuleRef, getSources, getSource) >>= { case (moduleRef, sources, source) =>
        if(sources.contains(source)) Success(sources - source) else Failure(ComponentNotDefined(source, moduleRef))
      }
      val newLayer = (newSources, getLayer, sourcesLens) >> (Layer.set(_)(_, _))
      for {
        _ <- newLayer >>= commit
        _ <- (getLayout, getModuleRef) >>= (_.classesDir(_).delete)
        _ <- (newLayer, getModuleRef, getLayout) >> Build.asyncBuild
      } yield log.await()
    }
  }

  def add: Try[ExitStatus] = {
    implicit val sourcesHint = possibleSourcesHint
    (cli -< ProjectArg -< ModuleArg -< SourceArg).action {
      val newSources = (getLayout, getLayer, getSources, getSource) >> { case (layout, layer, srcs, src) =>
        val findBySimplifiedName = (name: String) => layer.repos.find(_.remote.simplified == name)
        val localId = (Remote.local(layout).toOption >> (_.simplified) >>= findBySimplifiedName) >> (_.id)
        val newSource = Source.rewriteLocal(src, localId)
        srcs + newSource
      }
      val newLayer = (newSources, getLayer, sourcesLens) >> (Layer.set(_)(_, _))
      for {
        _ <- newLayer >>= commit
        _ <- (newLayer, getModuleRef, getLayout) >> Build.asyncBuild
      } yield log.await()
    }
  }

  private[this] lazy val getSources = getModule >> (_.sources)

  private[this] def sourcesLens: Try[Lens[Layer, SortedSet[Source], SortedSet[Source]]] = (getProject, getModule) >> {
    case (p, m) => Lens[Layer](_.projects(p.id).modules(m.id).sources)
  }

  private[this] lazy val existingSourcesHint: SourceArg.Hinter = SourceArg.hint(getSources)

  private[this] lazy val possibleSourcesHint: SourceArg.Hinter = SourceArg.hint((getLayout, getLayer) >> { case (layout, layer) =>
    val extSrcs = layer.repos.map(possibleSourceDirectories(_, layout)).flatten
    val localSrcs = layout.pwd.relativeSubdirsContaining(isSourceFileName).map(LocalSource(_, fury.io.Glob.All))
    extSrcs ++ localSrcs
  })

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
          (log.rawln(_)) >> finish
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
