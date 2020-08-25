/*

    Fury, version 0.18.7. Copyright 2018-20 Jon Pretty, Propensive OÜ.

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

import escritoire._
import mercator._

import scala.util._

case class UniverseCli(cli: Cli)(implicit val log: Log) extends CliApi {
  import Args._

  object repos {
    lazy val table: Tabulation[(RepoSetId, Set[RepoRef])] = Tables().repoSets
    implicit val columnHints: ColumnArg.Hinter = ColumnArg.hint(table.headings.map(_.name.toLowerCase))

    def list: Try[ExitStatus] = (cli -< RepoSetArg -< RawArg -< ColumnArg).action { for {
      col       <- opt(ColumnArg)
      rows      <- universe >> (_.repoSets.to[List])
      repoSetId <- opt(RepoSetArg)
      table     <- ~Tables().show(table, cli.cols, rows, has(RawArg), col, repoSetId >> (_.key), "commit")
      _         <- conf >> (_.focus()) >> (log.infoWhen(!has(RawArg))(_))
      _         <- ~log.rawln(table)
    } yield log.await() }

    def update: Try[ExitStatus] = (cli -< RepoSetArg -< CommitArg -< TagArg -< BranchArg).action {
      for {
        _         <- cli.atMostOne(BranchArg, TagArg)
        _         <- cli.atMostOne(CommitArg, BranchArg)
        _         <- cli.atMostOne(CommitArg, TagArg)
        refSpec   <- (opt(BranchArg), opt(TagArg), opt(CommitArg)) >> (_.orElse(_).orElse(_))
        refSpec   <- refSpec.ascribe(MissingParamChoice(BranchArg, TagArg, CommitArg))
        universe  <- universe
        repoSetId <- get(RepoSetArg)
        repos     <- universe.repoSets.get(repoSetId).ascribe(ItemNotFound(repoSetId))
        hierarchy <- getHierarchy
        someLayer <- hierarchy(repos.head.layer)
        someRepo  <- someLayer.repos.findBy(repos.head.repoId)
        _         <- (getHierarchy, getLayout) >>= (UniverseApi(_).repos.update(repoSetId, refSpec, _)) >>= commit
      } yield log.await()
    }
  }

  object projects {
    lazy val table: Tabulation[(ProjectRef, Project, Set[Pointer])] = Tables().entities
    implicit val columnHints: ColumnArg.Hinter = ColumnArg.hint(table.headings.map(_.name.toLowerCase))

    def list: Try[ExitStatus] = (cli -< RawArg -< ColumnArg -< ProjectArg).action { for {
      col       <- opt(ColumnArg)
      projectId <- opt(ProjectArg)
      universe  <- universe
      
      rows      <- universe.projectRefs.traverse { ref => universe(ref).flatMap { p =>
                     universe.pointers(ref).map((ref, p, _)) }
                   }

      table     <- ~Tables().show(table, cli.cols, rows, has(RawArg), col, projectId >> (_.key), "project")
      _         <- conf >> (_.focus()) >> (log.infoWhen(!has(RawArg))(_))
      _         <- ~log.rawln(table)
    } yield log.await() }

    def proliferate: Try[ExitStatus] = (cli -< ProjectRefArg).action { for {
      projectRef <- get(ProjectRefArg)
      hierarchy  <- getHierarchy >>= (UniverseApi(_).projects.proliferate(projectRef)) >>= commit
    } yield log.await() }

    def diff: Try[ExitStatus] = (cli -< ProjectRefArg -< AgainstProjectArg -< RawArg).action { for {
      universe <- universe
      left     <- get(ProjectRefArg)
      right    <- get(AgainstProjectArg)
      diff     <- getHierarchy >>= (UniverseApi(_).projects.diff(left, right))
      table    <- ~Tables().differences(str"$left", str"$right")
      table    <- ~Tables().show[Difference, Difference](table, cli.cols, diff, has(RawArg), None, None, "difference")
      _        <- ~log.rawln(table)
    } yield log.await() }
  }

  object imports {
    lazy val table: Tabulation[LayerProvenance] = Tables().layerRefs
    implicit val columnHints: ColumnArg.Hinter = ColumnArg.hint(table.headings.map(_.name.toLowerCase))
    implicit val versionHints: LayerVersionArg.Hinter = LayerVersionArg.hint()

    def list: Try[ExitStatus] = (cli -< RawArg -< ColumnArg).action {
      implicit val rowOrdering: Ordering[LayerProvenance] = Ordering[Iterable[ImportId]].on(_.ids)
      val output = (opt(ColumnArg), opt(LayerRefArg), universe >> (_.imports.values.to[List].sorted)) >> { case (col, layerRef, rows) =>
        Tables().show(table, cli.cols, rows, has(RawArg), col, layerRef >> (_.key), "layer")
      }
      for {
        _ <- conf >> (_.focus()) >> (log.infoWhen(!has(RawArg))(_))
        _ <- output >> log.rawln
      } yield log.await()
    }

    def update: Try[ExitStatus] = (cli -< LayerRefArg -< ImportArg -< LayerVersionArg).action {
      val newHierarchy = (getHierarchy, get(LayerRefArg), opt(ImportArg), opt(LayerVersionArg)) >>= (UniverseApi(_).imports.update(_, _, _))
      newHierarchy >> commit >> finish
    }
  }
}

case class UniverseApi(hierarchy: Hierarchy) {

  private[this] lazy val universe: Try[Universe] = hierarchy.universe

  private[this] def findUsages(ref: ShortLayerRef): Try[LayerProvenance] = universe >> (_.imports) >>= (_.findBy(ref))

  object repos {
    def update(repoSetId: RepoSetId, refSpec: RefSpec, layout: Layout)(implicit log: Log): Try[Hierarchy] =
      for {
        repoSets  <- universe >> (_.repoSets)
        repos     <- repoSets.findBy(repoSetId)
        someLayer <- hierarchy(repos.head.layer)
        someRepo  <- someLayer.repos.findBy(repos.head.repoId)
        gitDir    <- someRepo.remote.fetch(layout)
        commit    <- gitDir.resolve(refSpec)
        branch    <- gitDir.chooseBranch(refSpec)
        hierarchy <- hierarchy.updateAll(repos.map { r => (r.layer, r.repoId) }) { (layer, repoId) =>
                       Layer(_.repos(repoId)).modify(layer)(_.copy(branch = branch, commit = commit))
                     }
      } yield hierarchy
  }

  object projects {
    def proliferate(projectRef: ProjectRef)(implicit log: Log): Try[Hierarchy] = for {
      universe  <- universe
      project   <- universe(projectRef)
      pointers  =  universe.projects(projectRef.id).allOrigins
      hierarchy <- hierarchy.updateAll(pointers.map((_, ()))) { (layer, _) =>
                     Layer(_.projects(projectRef.id))(layer) = project
                   }
    } yield hierarchy

    def diff(left: ProjectRef, right: ProjectRef)(implicit log: Log): Try[Seq[Difference]] =
      (universe >>= (_(left)), universe >>= (_(right))) >> (Project.diff.diff)
  }

  object imports {
    def update(oldImport: ShortLayerRef, input: Option[LayerName], version: Option[LayerVersion])(implicit log: Log): Try[Hierarchy] = for {
      provenance  <- findUsages(oldImport)
      newImport   <- input.ascribe(NoPublishedName(oldImport)).orElse(getRemoteName(oldImport, provenance))
      newLayerRef <- Layer.resolve(newImport, version)
      pub         <- Layer.published(newImport, version)
      newLayer    <- Layer.get(newLayerRef, pub)
      _           <- newLayer.verify(false, false, Pointer.Root)
      newHierarchy   <- hierarchy.updateAll(provenance.imports) { (layer, imp) =>
                       val newImport = imp.copy(layerRef = newLayerRef, remote = pub)
                       Layer(_.imports).modify(layer)(_ - imp + newImport)
                     }
    } yield newHierarchy

    private def getRemoteName(layerRef: ShortLayerRef, provenance: LayerProvenance): Try[LayerName] = for {
      layer  <- hierarchy(provenance.imports.head._1)
      imp    <- layer.imports.findBy(layerRef)
      remote <- imp.remote.ascribe(NoPublishedName(layerRef))
    } yield remote.url
  }
}