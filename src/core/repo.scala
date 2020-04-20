/*

    Fury, version 0.13.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

import fury.model._, fury.strings._, fury.io._

import gastronomy._
import kaleidoscope._

import scala.util._

object Repo {
  implicit val msgShow: MsgShow[Repo] = r => UserMsg(_.url(r.simplified))
  implicit val stringShow: StringShow[Repo] = _.simplified
  implicit val parser: Parser[Repo] = str => Some(parse(str, true))

  case class ExistingLocalFileAsAbspath(absPath: String)

  object ExistingLocalFileAsAbspath {
    def unapply(path: String): Option[String] = Path(path).absolutePath().toOption match {
      case Some(absPath) => absPath.ifExists().map(_.value)
      case None          => None
    }
  }

  def parse(str: String, https: Boolean): Repo = Repo { str match {
    case "." => ""
    case r"gh:$group@([A-Za-z0-9_\-\.]+)/$project@([A-Za-z0-9\._\-]+)" =>
      if(https) str"https://github.com/$group/$project.git"
      else str"git@github.com:$group/$project.git"
    case r"gl:$group@([A-Za-z0-9_\-\.]+)/$project@([A-Za-z0-9\._\-]+)" =>
      if(https) str"https://gitlab.com/$group/$project.git"
      else str"git@gitlab.com:$group/$project.git"
    case r"bb:$group@([A-Za-z0-9_\-\.]+)/$project@([A-Za-z0-9\._\-]+)" =>
      if(https) str"https://bitbucket.com/$group/$project.git"
      str"git@bitbucket.com:$group/$project.git"
    case ExistingLocalFileAsAbspath(abspath) =>
      abspath
    case other =>
      other
  } }

  def local(layout: Layout): Try[Repo] = GitDir(layout.baseDir / ".git")(layout.env).remote
}

case class Repo(ref: String) {
  def hash: Digest = ref.digest[Md5]
  def path(layout: Layout): Path = Installation.reposDir / hash.encoded

  def equivalentTo(repo: Repo): Boolean = repo.simplified == simplified

  def gitDir(layout: Layout): GitDir = GitDir(path(layout))(layout.env)

  def pull(oldCommit: Commit, layout: Layout, https: Boolean)(implicit log: Log): Try[Commit] =
    for {
      _         <- fetch(layout, https)
      newCommit <- GitDir(path(layout))(layout.env).commit
      _         <- ~log.info(if(oldCommit != newCommit) msg"Repository $this updated to new commit $newCommit"
                        else msg"Repository $this has not changed")
    } yield newCommit

  def get(layout: Layout, https: Boolean)(implicit log: Log): Try[GitDir] = {
    if((path(layout) / ".done").exists) Success(GitDir(path(layout))(layout.env))
    else fetch(layout, https)
  }

  def fetch(layout: Layout, https: Boolean)(implicit log: Log): Try[GitDir] = {
    val done = path(layout) / ".done"
    
    if(path(layout).exists && !done.exists) {
      log.info(msg"Found incomplete clone of $this")
      path(layout).delete()
    }
    
    if(path(layout).exists) {
      done.delete()
      gitDir(layout).fetch(None)
      done.touch()
      Success(gitDir(layout))
    } else {
      log.info(msg"Cloning repository at $this")
      path(layout).mkdir()
      gitDir(layout).cloneBare(Repo.parse(ref, https)).map(gitDir(layout).waive)
    }
  }

  def simplified: String = ref match {
    case r"git@github.com:$group@(.*)/$project@(.*?)(\.git)?"     => str"gh:$group/$project"
    case r"git@bitbucket.com:$group@(.*)/$project@(.*?)(\.git)?"  => str"bb:$group/$project"
    case r"git@gitlab.com:$group@(.*)/$project@(.*?)(\.git)?"     => str"gl:$group/$project"
    case r"https://github.com/$group@(.*)/$project@(.*?)(\.git)?" => str"gh:$group/$project"
    case r"https://gitlab.com/$group@(.*)/$project@(.*?)(\.git)?" => str"gl:$group/$project"
    case other                                               => other
  }

  def universal(https: Boolean): Repo = Repo.parse(simplified, https)

  def projectName: Try[RepoId] = {
    val value = simplified.split("/").last
    RepoId.unapply(value).ascribe(InvalidValue(value))
  }
}