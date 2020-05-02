/*

    Fury, version 0.15.1. Copyright 2018-20 Jon Pretty, Propensive OÜ.

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

object Remote {
  implicit val msgShow: MsgShow[Remote] = r => UserMsg(_.url(r.simplified))
  implicit val stringShow: StringShow[Remote] = _.simplified
  implicit val parser: Parser[Remote] = str => Some(parse(str))

  case class ExistingLocalFileAsAbspath(absPath: String)

  object ExistingLocalFileAsAbspath {
    def unapply(path: String): Option[String] = Path(path).absolutePath().toOption match {
      case Some(absPath) => absPath.ifExists().map(_.value)
      case None          => None
    }
  }

  def parse(str: String): Remote = Remote { str match {
    case "." => ""
    case r"gh:$group@([A-Za-z0-9_\-\.]+)/$project@([A-Za-z0-9\._\-]+)" => str
    case r"gl:$group@([A-Za-z0-9_\-\.]+)/$project@([A-Za-z0-9\._\-]+)" => str
    case r"bb:$group@([A-Za-z0-9_\-\.]+)/$project@([A-Za-z0-9\._\-]+)" => str
    case ExistingLocalFileAsAbspath(abspath) =>
      abspath
    case other =>
      other
  } }

  def local(layout: Layout): Try[Remote] = {
    val gitDir =  layout.baseDir / ".git"
    if(gitDir.directory) GitDir(gitDir)(layout.env).remote
    else Failure(RepoNotFound(layout.baseDir))
  }
}

case class Remote(ref: String) {
  def hash: Digest = ref.digest[Md5]
  def path(layout: Layout): Path = Installation.reposDir / hash.encoded

  def equivalentTo(remote: Remote): Boolean = remote.simplified == simplified

  def gitDir(layout: Layout): GitDir = GitDir(path(layout))(layout.env)

  def pull(oldCommit: Commit, layout: Layout)(implicit log: Log): Try[Commit] =
    for {
      _         <- fetch(layout)
      newCommit <- GitDir(path(layout))(layout.env).commit
      _         <- ~log.info(if(oldCommit != newCommit) msg"Repository $this updated to new commit $newCommit"
                        else msg"Repository $this has not changed")
    } yield newCommit

  def get(layout: Layout)(implicit log: Log): Try[GitDir] = {
    if((path(layout) / ".done").exists) Success(GitDir(path(layout))(layout.env))
    else fetch(layout)
  }

  def fetch(layout: Layout)(implicit log: Log): Try[GitDir] = {
    val done = path(layout) / ".done"
    
    if(path(layout).exists && !done.exists) {
      log.info(msg"Found incomplete clone of $this")
      path(layout).delete()
    }
    
    if(path(layout).exists) {
      done.delete()
      gitDir(layout).fetch()
      done.touch()
      Success(gitDir(layout))
    } else {
      log.info(msg"Cloning repository at $this")
      path(layout).mkdir()
      gitDir(layout).cloneBare(this).map(gitDir(layout).waive)
    }
  }

  def simplified: String = ref match {
    case r"git@github.com:$group@(.*)/$project@(.*?)(\.git)?"     => str"gh:$group/$project"
    case r"git@bitbucket.com:$group@(.*)/$project@(.*?)(\.git)?"  => str"bb:$group/$project"
    case r"git@gitlab.com:$group@(.*)/$project@(.*?)(\.git)?"     => str"gl:$group/$project"
    case r"https://github.com/$group@(.*)/$project@(.*?)(\.git)?" => str"gh:$group/$project"
    case r"https://gitlab.com/$group@(.*)/$project@(.*?)(\.git)?" => str"gl:$group/$project"
    case other                                                    => other
  }

  def https: String = simplified match {
    case r"gh:$group@([A-Za-z0-9_\-\.]+)/$project@([A-Za-z0-9\._\-]+)" =>
      str"https://github.com/$group/$project.git"
    case r"gl:$group@([A-Za-z0-9_\-\.]+)/$project@([A-Za-z0-9\._\-]+)" =>
      str"https://gitlab.com/$group/$project.git"
    case r"bb:$group@([A-Za-z0-9_\-\.]+)/$project@([A-Za-z0-9\._\-]+)" =>
      str"https://bitbucket.com/$group/$project.git"
    case other => other
  }
  
  def ssh: String = simplified match {
    case r"gh:$group@([A-Za-z0-9_\-\.]+)/$project@([A-Za-z0-9\._\-]+)" =>
      str"git@bitbucket.com:$group/$project.git"
    case r"gl:$group@([A-Za-z0-9_\-\.]+)/$project@([A-Za-z0-9\._\-]+)" =>
      str"git@github.com:$group/$project.git"
    case r"bb:$group@([A-Za-z0-9_\-\.]+)/$project@([A-Za-z0-9\._\-]+)" =>
      str"git@gitlab.com:$group/$project.git"
    case other => other
  }

  def projectName: Try[RepoId] = {
    val value = simplified.split("/").last
    RepoId.unapply(value).ascribe(InvalidValue(value))
  }
}