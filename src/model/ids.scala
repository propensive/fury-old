/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.5.0. Copyright 2018-19 Jon Pretty, Propensive Ltd.                                        ║
   ║                                                                                                           ║
   ║ The primary distribution site is: https://propensive.com/                                                 ║
   ║                                                                                                           ║
   ║ Licensed under  the Apache License,  Version 2.0 (the  "License"); you  may not use  this file  except in ║
   ║ compliance with the License. You may obtain a copy of the License at                                      ║
   ║                                                                                                           ║
   ║     http://www.apache.org/licenses/LICENSE-2.0                                                            ║
   ║                                                                                                           ║
   ║ Unless required  by applicable law  or agreed to in  writing, software  distributed under the  License is ║
   ║ distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. ║
   ║ See the License for the specific language governing permissions and limitations under the License.        ║
   ╚═══════════════════════════════════════════════════════════════════════════════════════════════════════════╝
*/
package fury.model

import fury.strings._

import kaleidoscope._
import gastronomy._

import scala.util._

object Kind {
  implicit val msgShow: MsgShow[Kind] = v => UserMsg { t => v.name }
  implicit val stringShow: StringShow[Kind] = _.name
  val all: List[Kind] = List(Library, Compiler, Plugin, Application, Benchmarks)
  def unapply(str: String): Option[Kind] = all.find(_.name == str)
}

sealed abstract class Kind(val name: String, val needsExecution: Boolean)
case object Library extends Kind("library", false)
case object Compiler extends Kind("compiler", false)
case object Plugin extends Kind("plugin", false)
case object Application extends Kind("application", true)
case object Benchmarks extends Kind("benchmarks", true)

abstract class Key(val kind: UserMsg) { def key: String }

object ProjectId {
  implicit val msgShow: MsgShow[ProjectId]       = p => UserMsg(_.project(p.key))
  implicit val stringShow: StringShow[ProjectId] = _.key
  implicit def diff: Diff[ProjectId]             = (l, r) => Diff.stringDiff.diff(l.key, r.key)

  def parse(name: String): Try[ProjectId] = name match {
    case r"[a-z](-?[a-z0-9]+)*" => Success(ProjectId(name))
    case _                      => Failure(InvalidValue(name))
  }
}

case class ProjectId(key: String) extends Key(msg"project")

object ModuleId {
  implicit val msgShow: MsgShow[ModuleId]       = m => UserMsg(_.module(m.key))
  implicit val stringShow: StringShow[ModuleId] = _.key
  implicit def diff: Diff[ModuleId]             = (l, r) => Diff.stringDiff.diff(l.key, r.key)
  final val Core: ModuleId                      = ModuleId("core")

  def parse(name: String): Try[ModuleId] = name match {
    case r"[a-z](-?[a-z0-9]+)*" => Success(ModuleId(name))
    case _                      => Failure(InvalidValue(name))
  }
}

case class ModuleId(key: String) extends Key(msg"module")

case class BinRepoId(id: String)

object BinRepoId {
  implicit val msgShow: MsgShow[BinRepoId] = v => UserMsg(_.repo(v.id))
  implicit val stringShow: StringShow[BinRepoId] = _.id
  final val Central: BinRepoId = BinRepoId("central")
}

object Permission {
  implicit val msgShow: MsgShow[Permission] = stringShow.show
  
  implicit val stringShow: StringShow[Permission] =
    p => str"${p.className} ${p.target} ${p.action.getOrElse("-"): String}"
  
  implicit def diff: Diff[Permission] = Diff.gen[Permission]

  val Classes: List[String] = List(
    "java.awt.AWTPermission",
    "java.io.FilePermission",
    "java.io.SerializablePermission",
    "java.lang.management.ManagementPermission",
    "java.lang.reflect.ReflectPermission",
    "java.lang.RuntimePermission",
    "java.net.NetPermission",
    "java.net.SocketPermission",
    "java.net.URLPermission",
    "java.nio.file.LinkPermission",
    "java.security.AllPermission",
    "java.security.SecurityPermission",
    "java.security.UnresolvedPermission",
    "java.sql.SQLPermission",
    "java.util.logging.LoggingPermission",
    "java.util.PropertyPermission",
    "javax.management.MBeanPermission",
    "javax.management.MBeanServerPermission",
    "javax.management.MBeanTrustPermission",
    "javax.management.remote.SubjectDelegationPermission",
    "javax.net.ssl.SSLPermission",
    "javax.security.auth.AuthPermission",
    "javax.security.auth.kerberos.DelegationPermission",
    "javax.security.auth.kerberos.ServicePermission",
    "javax.security.auth.PrivateCredentialPermission",
    "javax.sound.sampled.AudioPermission",
    "javax.xml.bind.JAXBPermission",
    "javax.xml.ws.WebServicePermission"
  )
}
case class Permission(className: String, target: String, action: Option[String]) {
  def hash: String = this.digest[Sha256].encoded[Hex].toLowerCase
}
