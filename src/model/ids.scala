/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.6.7. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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

import language.higherKinds

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

object ImportPath {
  implicit val msgShow: MsgShow[ImportPath] = ip => UserMsg(_.layer(ip.path))
  implicit val stringShow: StringShow[ImportPath] = _.path
  val Root: ImportPath = ImportPath(".")

  // FIXME: Actually parse it and check that it's valid
  def parse(str: String): Option[ImportPath] =
    Some(ImportPath(str))
}

case class ImportPath(path: String) {
  def parts: List[ImportId] = path.split("/").to[List].tail.map(ImportId(_))
  def /(importId: ImportId): ImportPath = ImportPath(s"$path/${importId.key}")
  def tail: ImportPath = ImportPath(parts.tail.map(_.key).mkString("./", "/", ""))
  def isEmpty: Boolean = parts.length == 0
  def head: ImportId = parts.head
}

case class Focus(layerRef: LayerRef, path: ImportPath = ImportPath("."))

object IpfsRef {
  def parse(str: String): Option[IpfsRef] = str match {
    case r"fury:\/\/$hash@(.{44})" => Some(IpfsRef(hash))
    case _ => None
  }
}

case class IpfsRef(key: String) extends Key(msg"ipfs ref")

object LayerRef {
  implicit val msgShow: MsgShow[LayerRef] = lr => UserMsg(_.layer(lr.key))
  implicit val stringShow: StringShow[LayerRef] = _.key
  implicit def diff: Diff[LayerRef] = (l, r) => Diff.stringDiff.diff(l.key, r.key)
  
  def parse(value: String): Try[LayerRef] = value match {
    case r"[a-f0-9]{64}" => Success(LayerRef(value))
    case _               => Failure(InvalidValue(value))
  }
}

case class LayerRef(key: String) extends Key(msg"layer")

case class Config(showContext: Boolean = true,
                  theme: Theme = Theme.Basic,
                  undoBuffer: Int = 5,
                  timestamps: Boolean = true,
                  pipelining: Boolean = false)

object TargetId {
  implicit val stringShow: StringShow[TargetId] = _.key
  
  def apply(schemaId: SchemaId, projectId: ProjectId, moduleId: ModuleId): TargetId =
    TargetId(str"${schemaId}_${projectId}_${moduleId}")
  
  def apply(schemaId: SchemaId, ref: ModuleRef): TargetId =
    TargetId(schemaId, ref.projectId, ref.moduleId)
}

case class TargetId(key: String) extends AnyVal {
  def moduleId: ModuleId = ModuleId(key.split("_")(2))
  def projectId: ProjectId = ProjectId(key.split("_")(1))
  def schemaId: SchemaId = SchemaId(key.split("_")(0))
  def ref: ModuleRef = ModuleRef(projectId, moduleId)
}

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

object ManifestEntry {
  implicit val stringShow: StringShow[ManifestEntry] = _.key
  implicit val msgShow: MsgShow[ManifestEntry] = v => UserMsg { t => v.key }
  implicit val diff: Diff[ManifestEntry] = (l, r) => Diff.stringDiff.diff(l.pairString, r.pairString)
}

case class ManifestEntry(key: String, value: String) { def pairString: String = str"$key=$value" }

object PermissionHash {
  implicit val stringShow: StringShow[PermissionHash] = _.key
  implicit val msgShow: MsgShow[PermissionHash] = _.key
}
case class PermissionHash(key: String) extends Key(msg"permission")

object ScopeId {
  implicit val stringShow: StringShow[ScopeId] = _.id

  case object Project extends ScopeId("project")
  case object Directory extends ScopeId("directory")
  //case object Layer extends ScopeId("layer")

  val All = List(Project, Directory)//, Layer)

  def unapply(id: String): Option[ScopeId] = All.find(_.id == id)
}

sealed abstract class ScopeId(val id: String) extends scala.Product with scala.Serializable

object Grant {
  implicit val ord: Ordering[Grant] = Ordering[String].on[Grant](_.permission.hash)
  implicit val stringShow: StringShow[Grant] = _.digest[Sha256].encoded
}

case class Grant(scope: Scope, permission: Permission)

object PermissionEntry {
  implicit val msgShow: MsgShow[PermissionEntry] = pe => msg"${pe.hash.key} ${pe.permission}"
  implicit val stringShow: StringShow[PermissionEntry] = pe => pe.hash.key
}

case class PermissionEntry(permission: Permission, hash: PermissionHash)

object EnvVar {
  implicit val msgShow: MsgShow[EnvVar] = e => msg"${e.key}=${e.value}"
  implicit val stringShow: StringShow[EnvVar] = e => str"${e.key}=${e.value}"
  implicit def diff: Diff[EnvVar] = Diff.gen[EnvVar]

  def parse(str: String): Option[EnvVar] = str.split("=", 2) match {
    case Array(key, value) => Some(EnvVar(key, value))
    case Array(key)        => Some(EnvVar(key, ""))
    case _                 => None
  }
}

case class EnvVar(key: String, value: String)

object JavaProperty {
  implicit val msgShow: MsgShow[JavaProperty] = e => msg"${e.key}=${e.value}"
  implicit val stringShow: StringShow[JavaProperty] = e => str"${e.key}=${e.value}"
  implicit def diff: Diff[JavaProperty] = Diff.gen[JavaProperty]

  def parse(str: String): Option[JavaProperty] = str.split("=", 2) match {
    case Array(key, value) => Some(JavaProperty(key, value))
    case Array(key)        => Some(JavaProperty(key, ""))
    case _                 => None
  }
}
case class JavaProperty(key: String, value: String)

object Scope {
  def apply(id: ScopeId, layout: Layout, projectId: ProjectId) = id match {
    case ScopeId.Project => ProjectScope(projectId)
    case ScopeId.Directory => DirectoryScope(layout.base.value)
    //case ScopeId.Layer => LayerScope()
  }
}

sealed trait Scope extends scala.Product with scala.Serializable
case class DirectoryScope(dir: String) extends Scope
case class ProjectScope(name: ProjectId) extends Scope
//case class LayerScope(layerHash: String) extends Scope

object BloopSpec {
  implicit val msgShow: MsgShow[BloopSpec]       = v => msg"${v.org}:${v.name}"
  implicit val stringShow: StringShow[BloopSpec] = bs => str"${bs.org}:${bs.name}"
  implicit def diff: Diff[BloopSpec]             = Diff.gen[BloopSpec]

  def parse(str: String): Try[BloopSpec] = str match {
    case r"$org@([a-z][a-z0-9_\-\.]*):$id@([a-z][a-z0-9_\-\.]*):$version@([0-9a-z][A-Za-z0-9_\-\.]*)" =>
      Success(BloopSpec(org, id, version))
    case _ =>
      Failure(InvalidValue(str))
  }
}

case class BloopSpec(org: String, name: String, version: String)

object LineNo { implicit val msgShow: MsgShow[LineNo] = v => UserMsg(_.lineNo(v.line.toString)) }
case class LineNo(line: Int) extends AnyVal

object AliasCmd {
  implicit val msgShow: MsgShow[AliasCmd] = v => UserMsg(_.module(v.key))
  implicit val stringShow: StringShow[AliasCmd] = _.key
}

case class AliasCmd(key: String)

object Alias {
  implicit val msgShow: MsgShow[Alias] = v => UserMsg(_.module(v.cmd.key))
  implicit val stringShow: StringShow[Alias] = _.cmd.key
}

case class Alias(cmd: AliasCmd, description: String, schema: Option[SchemaId], module: ModuleRef)

object SchemaRef {
  implicit val msgShow: MsgShow[SchemaRef] = v =>
    UserMsg { theme => msg"${v.layerRef}${':'}${v.schema}".string(theme) }

  implicit val stringShow: StringShow[SchemaRef] = sr => str"${sr.layerRef}:${sr.schema}"
  implicit def diff: Diff[SchemaRef]             = Diff.gen[SchemaRef]

  def unapply(value: String): Option[SchemaRef] = value match {
    case r"$layer@([a-fA-F0-9]{64}):$schema@([a-zA-Z0-9\-\.]*[a-zA-Z0-9])$$" =>
      Some(SchemaRef(ImportId(""), LayerRef(layer), SchemaId(schema)))
    case _ =>
      None
  }
}

case class SchemaRef(id: ImportId, layerRef: LayerRef, schema: SchemaId)

object ImportId {
  implicit val msgShow: MsgShow[ImportId]       = m => UserMsg(_.layer(m.key))
  implicit val stringShow: StringShow[ImportId] = _.key
  implicit def diff: Diff[ImportId]             = (l, r) => Diff.stringDiff.diff(l.key, r.key)

  def parse(name: String): Try[ImportId] = name match {
    case r"[a-z](-?[a-z0-9]+)*" => Success(ImportId(name))
    case _                      => Failure(InvalidValue(name))
  }
}

case class ImportId(key: String) extends Key("import")

object ModuleRef {
  implicit val stringShow: StringShow[ModuleRef] = ref => str"${ref.projectId}/${ref.moduleId}"
  implicit val entityName: EntityName[ModuleRef] = EntityName(msg"dependency")
  val JavaRef = ModuleRef(ProjectId("java"), ModuleId("compiler"), false)

  implicit val diff: Diff[ModuleRef] =
    (l, r) => if(l == r) Nil else List(Difference(msg"ref", msg"", msg"$l", msg"$r"))

  implicit val msgShow: MsgShow[ModuleRef] = ref =>
    UserMsg { theme =>
      msg"${theme.project(ref.projectId.key)}${theme.gray("/")}${theme.module(ref.moduleId.key)}".string(theme)
    }

  def parseFull(string: String, intransitive: Boolean): Try[ModuleRef] = string match {
    case r"$projectId@([a-z][a-z0-9\-]*[a-z0-9])\/$moduleId@([a-z][a-z0-9\-]*[a-z0-9])" =>
      Success(ModuleRef(ProjectId(projectId), ModuleId(moduleId), intransitive))
    case _ =>
      Failure(ItemNotFound(ModuleId(string)))
  }

  def parse(projectId: ProjectId, string: String, intransitive: Boolean): Try[ModuleRef] =
    string match {
      case r"$projectId@([a-z](-?[a-z0-9]+)*)\/$moduleId@([a-z](-?[a-z0-9]+)*)" =>
        Success(ModuleRef(ProjectId(projectId), ModuleId(moduleId), intransitive))
      case r"[a-z](-?[a-z0-9]+)*" =>
        Success(ModuleRef(projectId, ModuleId(string), intransitive))
      case _ =>
        Failure(ItemNotFound(ModuleId(string)))
    }
}

case class ModuleRef(projectId: ProjectId,
                     moduleId: ModuleId,
                     intransitive: Boolean = false,
                     hidden: Boolean = false) {

  override def equals(that: Any): Boolean = that match {
    case ModuleRef(p, m, _, _) => projectId == p && moduleId == m
    case _                     => false
  }

  def hide = copy(hidden = true)
  override def hashCode: Int = projectId.hashCode + moduleId.hashCode
  override def toString: String = str"$projectId/$moduleId"
}

object SchemaId {
  implicit val msgShow: MsgShow[SchemaId]       = v => UserMsg(_.schema(v.key))
  implicit val stringShow: StringShow[SchemaId] = _.key
  implicit val diff: Diff[SchemaId] = (l, r) => Diff.stringDiff.diff(l.key, r.key)
  final val default = SchemaId("default")

  def parse(name: String): Try[SchemaId] = name match {
    case r"[a-z](-?[a-zA-Z0-9]+)*" => Success(SchemaId(name))
    case _                         => Failure(InvalidValue(name))
  }
}

case class SchemaId(key: String) extends Key(msg"schema")

object RepoId {
  implicit val msgShow: MsgShow[RepoId]       = r => UserMsg(_.repo(r.key))
  implicit val stringShow: StringShow[RepoId] = _.key
}

case class RepoId(key: String) extends Key(msg"repository")

object Parameter {
  implicit val stringShow: StringShow[Parameter] = _.name
  implicit val msgShow: MsgShow[Parameter] = v => UserMsg(_.param(v.name))
  implicit val diff: Diff[Parameter] = (l, r) => Diff.stringDiff.diff(l.name, r.name)
}

case class Parameter(name: String) { def parameter = str"-$name" }

object License {
  implicit val msgShow: MsgShow[License]       = v => UserMsg(_.license(v.id.key))
  implicit val stringShow: StringShow[License] = _.id.key
  val unknown = LicenseId("unknown")

  val standardLicenses = List(
      License(LicenseId("afl-3.0"), "Academic Free License v3.0"),
      License(LicenseId("apache-2.0"), "Apache license 2.0"),
      License(LicenseId("artistic-2.0"), "Artistic license 2.0"),
      License(LicenseId("bsd-2-clause"), "BSD 2-clause \"Simplified\" license"),
      License(LicenseId("bsd-3-clause"), "BSD 3-clause \"New\" or \"Revised\" license"),
      License(LicenseId("bsl-1.0"), "Boost Software License 1.0"),
      License(LicenseId("bsd-3-clause-clear"), "BSD 3-clause Clear license"),
      License(LicenseId("cc"), "Creative Commons license family"),
      License(LicenseId("cc0-1.0"), "Creative Commons Zero v1.0 Universal"),
      License(LicenseId("cc-by-4.0"), "Creative Commons Attribution 4.0"),
      License(LicenseId("cc-by-sa-4.0"), "Creative Commons Attribution Share Alike 4.0"),
      License(LicenseId("wtfpl"), "Do What The F*ck You Want To Public License"),
      License(LicenseId("ecl-2.0"), "Educational Community License v2.0"),
      License(LicenseId("epl-1.0"), "Eclipse Public License 1.0"),
      License(LicenseId("epl-1.1"), "European Union Public License 1.1"),
      License(LicenseId("agpl-3.0"), "GNU Affero General Public License v3.0"),
      License(LicenseId("gpl"), "GNU General Public License family"),
      License(LicenseId("gpl-2.0"), "GNU General Public License v2.0"),
      License(LicenseId("gpl-3.0"), "GNU General Public License v3.0"),
      License(LicenseId("lgpl"), "GNU Lesser General Public License family"),
      License(LicenseId("lgpl-2.1"), "GNU Lesser General Public License v2.1"),
      License(LicenseId("lgpl-3.0"), "GNU Lesser General Public License v3.0"),
      License(LicenseId("isc"), "ISC"),
      License(LicenseId("lppl-1.3c"), "LaTeX Project Public License v1.3c"),
      License(LicenseId("ms-pl"), "Microsoft Public License"),
      License(LicenseId("mit"), "MIT"),
      License(LicenseId("mpl-2.0"), "Mozilla Public License 2.0"),
      License(LicenseId("osl-3.0"), "Open Software License 3.0"),
      License(LicenseId("postgresql"), "PostgreSQL License"),
      License(LicenseId("ofl-1.1"), "SIL Open Font License 1.1"),
      License(LicenseId("ncsa"), "University of Illinois/NCSA Open Source License"),
      License(LicenseId("unlicense"), "The Unlicense"),
      License(LicenseId("zlib"), "zLib License")
  )
}

object LicenseId {
  implicit val msgShow: MsgShow[LicenseId]       = v => UserMsg(_.license(v.key))
  implicit val stringShow: StringShow[LicenseId] = _.key
}

case class LicenseId(key: String) extends Key(msg"license")

case class License(id: LicenseId, name: String)

object RefSpec {
  implicit val msgShow: MsgShow[RefSpec]       = v => UserMsg(_.version(v.id))
  implicit val stringShow: StringShow[RefSpec] = _.id
  val master: RefSpec = RefSpec("master")
}
case class RefSpec(id: String)

object Commit {
  implicit val stringShow: StringShow[Commit] = _.id
  implicit val msgShow: MsgShow[Commit]       = r => UserMsg(_.version(r.id.take(7)))
}
case class Commit(id: String)
