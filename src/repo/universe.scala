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

  lazy val table: Tabulation[(Commit, Set[RepoRef])] = Tables().repoSets

  implicit val columnHints: ColumnArg.Hinter = ColumnArg.hint(table.headings.map(_.name.toLowerCase))
  implicit val commitHints: CommitArg.Hinter = CommitArg.hint(universe >> (_.repoSets.map(_._1)))

  def list: Try[ExitStatus] = (cli -< CommitArg -< RawArg -< ColumnArg).action { for {
    col    <- ~cli.peek(ColumnArg)
    rows   <- universe >> (_.repoSets.to[List])
    commit <- opt(CommitArg)
    table  <- ~Tables().show(table, cli.cols, rows, has(RawArg), col, commit >> (_.id), "commit")
    _      <- conf >> (_.focus()) >> (log.infoWhen(!has(RawArg))(_))
    _      <- ~log.rawln(table)
  } yield log.await() }

  def update: Try[ExitStatus] = (cli -< CommitArg -< TagArg -< BranchArg).action {
    ???
  }
}
