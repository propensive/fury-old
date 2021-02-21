/*

    Fury, version 0.33.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

import fury.model._, fury.io._, fury.text._

import gastronomy._
import guillotine._
import jovian._

import scala.util._

case class Snapshot(stashes: Map[StashId, Stash] = Map()) {
  def apply(commit: StashId): Try[Stash] = stashes.get(commit).ascribe(ItemNotFound(commit))

  def ++(that: Snapshot): Snapshot =
    Snapshot(that.stashes.foldLeft(stashes) { case (all, (commit, stash)) =>
      all.updated(commit, all.get(commit).fold(stash) { case old =>
        old.copy(sources = old.sources ++ stash.sources)
      })
    })
}

object Stash {
  implicit val msgShow: MsgShow[Stash] = s =>
    msg"${s.repoId} ${'('}${s.local.fold(msg"${s.remote}${'@'}${s.commit}${'/'}${s.branch}")(_.dir.msg)}${')'}"
}

case class Stash(repoId: RepoId,
                 remote: Remote,
                 local: Option[GitDir],
                 commit: Commit,
                 branch: Branch,
                 sources: List[Path]) {

  def hash: StashId = StashId((commit, local, sources).digest[Md5])
  def path: Path = Installation.srcsDir / hash.hash.encoded[Hex].take(16)

  def absolutePath(relativePath: Path): Path = relativePath in path

  def get(layout: Layout)(implicit log: Log): Try[GitDir] = for {
    repoDir    <- remote.get(layout)
    workingDir <- checkout(layout)
  } yield workingDir

  private def checkout(layout: Layout)(implicit log: Log): Try[GitDir] =
    local.map(Success(_)).getOrElse {
      val sourceDesc: Message = sources match {
        case List() =>
          Message(_.path("*"))
        case head :: Nil =>
          msg"$head"
        case head :: tail =>
          msg"${tail.foldLeft(msg"${'{'}$head") { case (str, next) => msg"$str${','} $next" }}${'}'}"
      }

      if(path.exists && (path / ".unfinished").exists) {
        log.info(msg"Found incomplete checkout of ${sourceDesc}")
        path.delete()
      }

      val gitDir = GitDir(path)(layout.env)

      if(!path.exists) {
        log.info(msg"Checking out $sourceDesc from repository $repoId")
        path.mkdir()
        gitDir.sparseCheckout(remote.path(layout), sources, branch = branch,
            commit = commit, None)/*.flatMap { _ => (path / ".git").delete() }*/.map(gitDir.waive).recoverWith {
          case e: ShellFailure if e.stderr.contains("Sparse checkout leaves no entry on working directory") =>
            Failure(NoSourcesError(repoId, commit, sourceDesc))
          case e: Exception =>
            Failure(e)
        }
      } else Success(gitDir)
    }
}