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

import mitigation._
import guillotine._

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
