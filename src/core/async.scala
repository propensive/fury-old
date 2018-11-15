/*
  Fury, version 0.1.0. Copyright 2018 Jon Pretty, Propensive Ltd.

  The primary distribution site is: https://propensive.com/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required  by applicable  law or  agreed to  in writing,  software  distributed  under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
  express  or  implied.  See  the  License for  the specific  language  governing  permissions and
  limitations under the License.
                                                                                                  */
package fury

import impromptu._
import mitigation._
import guillotine._
import eucalyptus._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.HashMap

object AsyncRepos {

  def get(repo: Repo)(implicit layout: Layout, shell: Shell): Result[Path, ~ | ShellFailure | InvalidValue | FileWriteError] = for {
    path <- repo.path
    dir  <- ~(path in layout.reposDir)
    dir  <- cloneIfNecessary(dir, repo)
  } yield dir

  private def cloneIfNecessary(dir: Path, repo: Repo)(implicit shell: Shell) = {
    if(!dir.exists) {
      dir.mkdir()
      shell.git.cloneBare(repo, dir)
    }
    Answer(dir)
  }

  def get(sourceRepo: SourceRepo, sources: List[Source])(implicit layout: Layout, shell: Shell): Result[Path, ~ | ShellFailure | InvalidValue | FileWriteError] = for {
    bareRepo <- get(sourceRepo.repo)
    dir      <- ~(layout.refsDir / sourceRepo.refSpec.id)
    dir      <- checkoutIfNecessary(dir, bareRepo, sources, sourceRepo.refSpec)
  } yield dir

  private def checkoutIfNecessary(dir: Path, bareRepo: Path, sources: List[Source], refSpec: RefSpec)(implicit shell: Shell) = {
    if(!dir.exists) {
      dir.mkdir()
      shell.git.sparseCheckout(bareRepo, dir, sources.map(_.path), refSpec.id)
    }
    Answer(dir)
  }

}


// FIXME: Have a way to force a retry
/*object AsyncRepo {
  private val cache: HashMap[(SourceRepo, Path), Async[Result[Path, ~ | FileWriteError |
                         ShellFailure], _, Result[Path, ~ | FileWriteError | ShellFailure]]] =
    HashMap()
  
  private def cloneRepo(repo: SourceRepo)(implicit shell: Shell, layout: Layout) = Async {
    for {
      path <- repo.local.map(Answer(_)).getOrElse(layout.managedRepo(repo.id))
      exists = (path / ".git").exists
      path <- if(exists) Answer(path) else for {
                cloned <- shell.git.clone(repo.repo, path)
                checkedOut <- shell.git.checkout(path, repo.refSpec.id)
              } yield path
    } yield path
  }.started()

  def awaitAll(): Unit = cache.map(_._2.await())

  def get(repo: SourceRepo, retry: Boolean)(implicit layout: Layout, shell: Shell) = synchronized {
    cache.get((repo, layout.pwd)) match {
      case Some(async) =>
        if((!async.future.isCompleted || async.future.value.get.isSuccess || !retry) &&
            repo.directory.map(_.exists).opt.getOrElse(false)) async
        else {
          val async = cloneRepo(repo)
          cache((repo, layout.pwd)) = async
          async
        }
      case None =>
        val async = cloneRepo(repo)
        cache((repo, layout.pwd)) = async
        async
    }
  }

  private def tmpMove(repos: Set[SourceRepo])(implicit layout: Layout, log: Engine[String, String]): Set[SourceRepo] =
    repos.filter(_.local.isEmpty).map {
      case sr@SourceRepo(RepoId(id), repo, refSpec, None) =>
        log.audit(s"Renaming repository $id to $id.tmp")
        sr.directory.foreach { dir =>
          dir.moveTo(dir.rename(_+".tmp"))
        }
        SourceRepo(RepoId(s"$id.tmp"), repo, refSpec, None)
    }

  def prepareRepos[T](repos: Set[SourceRepo])(block: => T)(implicit layout: Layout, shell: Shell, log: Engine[String, String]): Result[T, ~ | ShellFailure] =
    synchronized {
      for {
        current <- currentRepos
      } yield {
        repos.filter(_.local.isEmpty).foldLeft(tmpMove(current)) { (current, next) =>
        current.find(_.repo == next.repo) match {
          case None =>
            log.audit(s"Cloning repository ${next.id.key}")
            cloneRepo(next)
            current
          case Some(found) =>
            log.audit(s"Repurposing repository ${next.id.key}")
            val removed = current - found
            found.directory.foreach { dir =>
              log.audit(shell.git.checkout(dir, next.refSpec.id).toString)
              dir.moveTo(dir.rename(_ => next.id.key))
            }
            removed
        }
      }

      block
    }
  }

  def currentRepos(implicit layout: Layout, shell: Shell, log: Engine[String, String]): Result[Set[SourceRepo], ~ | ShellFailure] =
    layout.reposDir.children.map { dirName =>
      val dir = layout.reposDir / dirName
      for {
        remote <- shell.git.getRemote(dir)
        refSpec <- shell.git.getCommit(dir)
      } yield SourceRepo(RepoId(dir.name), Repo(remote), RefSpec(refSpec), None)
    }.sequence.map(_.to[Set])
}*/
