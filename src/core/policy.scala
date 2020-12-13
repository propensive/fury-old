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

import fury.ogdl._, fury.io._, fury.text._, fury.model._

import jovian._

import scala.collection.immutable._
import scala.util._

object Policy {
  val CurrentVersion = 1
  def read(implicit log: Log): Policy =
    Ogdl.read[Policy](Installation.policyFile,
        migrate(_)).toOption.getOrElse(Policy(CurrentVersion))

  def save(policy: Policy)(implicit log: Log): Try[Unit] =
    Installation.policyFile.extantParents().writeSync(Ogdl.serialize(Ogdl(policy)))

  private def migrate(ogdl: Ogdl)(implicit log: Log): Ogdl = {
    val version = Try(ogdl.version().toInt).getOrElse(0)
    if(version < Policy.CurrentVersion) {
      migrate((version match {
        case 0 =>
          ogdl.set(policy = ogdl.map { grant =>
            grant.set(permission = grant.permission.set(classRef = Ogdl(grant.permission.className())))
          })
      }).set(version = Ogdl(version + 1)))
    } else ogdl
  }
  
  def standardPrivileges(dir: Path): List[Privilege] =
    List(
      Privilege(GlobalScope, Permission(ClassRef("java.util.PropertyPermission"), "scala.*", Some("read"))),
      Privilege(GlobalScope, Permission(ClassRef("java.util.PropertyPermission"), "user.dir", Some("read"))),
      Privilege(GlobalScope, Permission(ClassRef("java.io.FilePermission"), "-", Some("read,write,delete"))),
      Privilege(GlobalScope, Permission(ClassRef("java.lang.RuntimePermission"), "accessClassInPackage.sun.misc", None)),
      Privilege(GlobalScope, Permission(ClassRef("java.lang.RuntimePermission"), "accessDeclaredMembers", None)),
      Privilege(GlobalScope, Permission(ClassRef("java.lang.reflect.ReflectPermission"), "suppressAccessChecks", None)),
    )
}

case class Policy(version: Int, policy: SortedSet[Privilege] = TreeSet()) {
  def forContext(layout: Layout, projectId: ProjectId/*, layer: Layer*/): Policy =
    Policy(Policy.CurrentVersion, policy.filter {
      case Privilege(DirectoryScope(dir), _) => dir == layout.baseDir
      case Privilege(ProjectScope(id), _)    => projectId == id
      case Privilege(GlobalScope, _)         => true
      //case Grant(LayerScope(hash), _)    => hash == layer.hash
    })

  def grant(scope: Scope, permissions: List[Permission]): Policy =
    copy(policy = policy ++ permissions.map(Privilege(scope, _)))

  def obviate(scope: Scope, permissions: List[Permission]): Policy =
    copy(policy = policy.filterNot(permissions.contains))

  def checkAll(permissions: Iterable[Permission], noSecurity: Boolean): Try[Unit] = {
    val missing = permissions.to[Set] -- policy.map(_.permission)
    if(noSecurity || missing.isEmpty) Success(()) else Failure(NoPermissions(missing))
  }

  def save(file: Path, workDir: Path): Try[Unit] = file.writeSync {
    val sb = new StringBuilder()
    sb.append("grant {\n")
    (Policy.standardPrivileges(workDir) ++ policy).foreach { grant =>
      val p = grant.permission
      val actionAddendum = p.action.fold("") { a => s""", "$a"""" }
      sb.append(str""" permission ${p.classRef} "${p.target}"${actionAddendum};""")
      sb.append('\n')
    }
    sb.append("};\n")
    sb.toString
  }
}
  
