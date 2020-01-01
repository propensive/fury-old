package fury.core

import fury.model._, fury.io._, fury.strings._

import gastronomy._
import guillotine._

import scala.util._

case class Checkout(repoId: RepoId,
                    repo: Repo,
                    local: Option[Path],
                    commit: Commit,
                    refSpec: RefSpec,
                    sources: List[Path]) {

  def hash: Digest = this.digest[Md5]
  def path: Path = Installation.srcsDir / hash.encoded

  def get(layout: Layout, https: Boolean)(implicit log: Log): Try[Path] = for {
    repoDir    <- repo.get(layout, https)
    workingDir <- checkout(layout)
  } yield workingDir

  private def checkout(layout: Layout)(implicit log: Log): Try[Path] =
    local.map(Success(_)).getOrElse {
      val sourceDesc: UserMsg = sources match {
        case List() =>
          UserMsg { theme => theme.path("*") }
        case head :: Nil =>
          msg"$head"
        case head :: tail =>
          val init = tail.foldLeft(msg"${'{'}$head") { case (str, next) => msg"$str${','} $next" }
          msg"$init${'}'}"
      }

      if(path.exists && !(path / ".done").exists) {
        log.info(msg"Found incomplete checkout of ${sourceDesc}")
        path.delete()
      }

      if(!path.exists) {
        log.info(msg"Checking out $sourceDesc from repository $repoId")
        path.mkdir()
        Shell(layout.env).git.sparseCheckout(repo.path(layout), path, sources, refSpec = refSpec.id,
            commit = commit.id, None).flatMap { _ => (path / ".git").delete() }.map(path.waive).recoverWith {
          case e: ShellFailure if e.stderr.contains("Sparse checkout leaves no entry on working directory") =>
            Failure(NoSourcesError(repoId, commit, sourceDesc))
          case e: Exception => Failure(e)
        }
      } else Success(path)
    }
}