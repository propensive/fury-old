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

  lazy val table: Tabulation[(RepoSetId, Set[RepoRef])] = Tables().repoSets

  implicit val columnHints: ColumnArg.Hinter = ColumnArg.hint(table.headings.map(_.name.toLowerCase))

  def list: Try[ExitStatus] = (cli -< RepoSetArg -< RawArg -< ColumnArg).action { for {
    col       <- ~cli.peek(ColumnArg)
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
      _         <- (getHierarchy, getLayout) >>= (UniverseApi(_).update(repoSetId, refSpec, _)) >>= commit
    } yield log.await()
  }
}

case class UniverseApi(hierarchy: Hierarchy) {
  def update(repoSetId: RepoSetId, refSpec: RefSpec, layout: Layout)(implicit log: Log): Try[Hierarchy] = for {
    repoSets  <- hierarchy.universe >> (_.repoSets)
    repos     <- repoSets.get(repoSetId).ascribe(ItemNotFound(repoSetId))
    someLayer <- hierarchy(repos.head.layer)
    someRepo  <- someLayer.repos.findBy(repos.head.repoId)
    gitDir    <- someRepo.remote.fetch(layout)
    commit    <- gitDir.resolve(refSpec)
    branch    <- gitDir.chooseBranch(refSpec)
    hierarchy <- repos.foldLeft(Try(hierarchy)) { case (getHierarchy, RepoRef(repoId, layerRef)) =>
                   for {
                     hierarchy <- getHierarchy
                     layer     <- hierarchy(layerRef)
                     layer     <- Try(Layer(_.repos(repoId).commit)(layer) = commit)
                     layer     <- Try(Layer(_.repos(repoId).branch)(layer) = branch)
                     hierarchy <- hierarchy(layerRef) = layer
                   } yield hierarchy
                 }
  } yield hierarchy
}