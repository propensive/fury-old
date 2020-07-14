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

import escritoire._

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
    lazy val table: Tabulation[Entity] = Tables().entities(None)
    implicit val columnHints: ColumnArg.Hinter = ColumnArg.hint(table.headings.map(_.name.toLowerCase))

    def list: Try[ExitStatus] = (cli -< RawArg -< ColumnArg -< ProjectArg).action { for {
      col       <- opt(ColumnArg)
      projectId <- opt(ProjectArg)
      rows      <- universe >> (_.entities.to[List].map(_._2))
      table     <- ~Tables().show(table, cli.cols, rows, has(RawArg), col, projectId >> (_.key), "project")
      _         <- conf >> (_.focus()) >> (log.infoWhen(!has(RawArg))(_))
      _         <- ~log.rawln(table)
    } yield log.await() }

    def proliferate: Try[ExitStatus] = (cli -< LayerArg -< ProjectArg).action { for {
      layerRef  <- get(LayerArg)
      projectId <- get(ProjectArg)
      hierarchy <- getHierarchy >>= (UniverseApi(_).projects.proliferate(layerRef, projectId)) >>= commit
    } yield log.await() }
  }

  object imports {
    lazy val table: Tabulation[LayerEntity] = Tables().layerRefs
    implicit val columnHints: ColumnArg.Hinter = ColumnArg.hint(table.headings.map(_.name.toLowerCase))

    def list: Try[ExitStatus] = (cli -< RawArg -< ColumnArg).action { for {
      col      <- opt(ColumnArg)
      layerRef <- opt(LayerRefArg)
      rows     <- universe >> (_.imports.values.to[List])
      table    <- ~Tables().show(table, cli.cols, rows, has(RawArg), col, layerRef >> (_.key), "layer")
      _        <- conf >> (_.focus()) >> (log.infoWhen(!has(RawArg))(_))
      _        <- ~log.rawln(table)
    } yield log.await() }

    def pull: Try[ExitStatus] = (cli -< LayerRefArg -< ImportArg).action { for {
      layerRef   <- get(LayerRefArg)
      importName <- opt(ImportArg)
      hierarchy  <- getHierarchy >>= (UniverseApi(_).imports.pull(layerRef, importName)) >>= commit
    } yield log.await() }
  }
}

case class UniverseApi(hierarchy: Hierarchy) {

  object repos {
    def update(repoSetId: RepoSetId, refSpec: RefSpec, layout: Layout)(implicit log: Log): Try[Hierarchy] =
      for {
        repoSets  <- hierarchy.universe >> (_.repoSets)
        repos     <- repoSets.get(repoSetId).ascribe(ItemNotFound(repoSetId))
        someLayer <- hierarchy(repos.head.layer)
        someRepo  <- someLayer.repos.findBy(repos.head.repoId)
        gitDir    <- someRepo.remote.fetch(layout)
        commit    <- gitDir.resolve(refSpec)
        branch    <- gitDir.chooseBranch(refSpec)
        hierarchy <- repos.foldLeft(Try(hierarchy)) { case (getHierarchy, RepoRef(repoId, layerRef)) => for {
                       hierarchy <- getHierarchy
                       layer     <- hierarchy(layerRef)
                       layer     <- Try(Layer(_.repos(repoId).commit)(layer) = commit)
                       layer     <- Try(Layer(_.repos(repoId).branch)(layer) = branch)
                       hierarchy <- hierarchy(layerRef) = layer
                     } yield hierarchy }
      } yield hierarchy
  }

  object projects {

    private def hierarchyWithoutProject(importPath: ImportPath, projectId: ProjectId)
                                       (implicit log: Log)
                                       : Try[Hierarchy] = for {
      layer     <- hierarchy(importPath)
      project   <- layer.projects.findBy(projectId)
      layer     <- Try(Layer(_.projects).modify(layer)(_ - project))
      hierarchy <- hierarchy(importPath) = layer
    } yield hierarchy

    def proliferate(importPath: ImportPath, projectId: ProjectId)
                   (implicit log: Log)
                   : Try[Hierarchy] = for {
      layer        <- hierarchy(importPath)
      project      <- layer.projects.findBy(projectId)
      tmpHierarchy <- hierarchyWithoutProject(importPath, projectId)
      universe     <- tmpHierarchy.universe
      entity       <- universe.entities.get(projectId).ascribe(ItemNotFound(projectId))
      hierarchy    <- entity.layers.to[List].foldLeft(Try(hierarchy)) { case (getHierarchy, (layerRef, _)) =>
                        for {
                          hierarchy <- getHierarchy
                          layer     <- hierarchy(layerRef)
                          layer     <- Try(Layer(_.projects(projectId))(layer) = project)
                          hierarchy <- hierarchy(layerRef) = layer
                        } yield hierarchy
                      }
    } yield hierarchy
  }

  object imports {
    def pull(layerRef: ShortLayerRef, input: Option[LayerName])
            (implicit log: Log)
            : Try[Hierarchy] = for {
      universe    <- hierarchy.universe
      layerEntity <- universe.imports.get(layerRef).ascribe(ItemNotFound(layerRef))
      someLayer   <- hierarchy(layerEntity.imports.head._1)
      someImport  <- someLayer.imports.find(_.layerRef.key.startsWith(layerRef.key)).ascribe(ItemNotFound(layerRef))
      importName  <- input.orElse(someImport.remote.map(_.url)).ascribe(NoRemoteInferred())
      newLayerRef <- Layer.resolve(importName)
      pub         <- Layer.published(importName)
      newLayer    <- Layer.get(newLayerRef, pub)
      _           <- newLayer.verify(false, ImportPath.Root)
      hierarchy   <- layerEntity.imports.foldLeft(Try(hierarchy)) { case (getHierarchy, (path, oldImport)) =>
                       for {
                         hierarchy <- getHierarchy
                         layer     <- hierarchy(path)
                         newImport <- ~oldImport.copy(layerRef = newLayerRef, remote = pub)
                         layer     <- Try(Layer(_.imports).modify(layer) { is => is - oldImport + newImport })
                         hierarchy <- hierarchy(path) = layer
                       } yield hierarchy
                     }
      } yield hierarchy
  }
}