/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.8.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.                                         ║
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

import fury.strings._, fury.io._, fury.ogdl._

import kaleidoscope._
import gastronomy._

import scala.util._
import scala.collection.immutable.ListMap


import language.higherKinds

import scala.collection.immutable.SortedSet

object ManagedConfig {
  private var config: Config =
    Ogdl.read[Config](Installation.userConfig, identity(_)).toOption.getOrElse(Config())

  def write(newConfig: Config): Try[Unit] = synchronized {
    config = newConfig
    Ogdl.write(config, Installation.userConfig)
  }

  def apply(): Config = config
}

object Kind {
  implicit val msgShow: MsgShow[Kind] = v => UserMsg { t => v.name }
  implicit val stringShow: StringShow[Kind] = _.name
  implicit val parser: Parser[Kind] = unapply(_)
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
  implicit val msgShow: MsgShow[ProjectId] = p => UserMsg(_.project(p.key))
  implicit val stringShow: StringShow[ProjectId] = _.key
  implicit val diff: Diff[ProjectId] = (l, r) => Diff.stringDiff.diff(l.key, r.key)
  implicit val parser: Parser[ProjectId] = unapply(_)
  implicit val keyName: KeyName[ProjectId] = () => msg"project"

  def unapply(name: String): Option[ProjectId] = name.only { case r"[a-z](-?[a-z0-9]+)*" => ProjectId(name) }
}

case class ProjectId(key: String) extends Key(msg"project")

object ModuleId {
  implicit val msgShow: MsgShow[ModuleId] = m => UserMsg(_.module(m.key))
  implicit val stringShow: StringShow[ModuleId] = _.key
  implicit val diff: Diff[ModuleId] = (l, r) => Diff.stringDiff.diff(l.key, r.key)
  implicit val parser: Parser[ModuleId] = unapply(_)
  implicit val keyName: KeyName[ModuleId] = () => msg"module"

  final val Core: ModuleId = ModuleId("core")

  def unapply(name: String): Option[ModuleId] = name.only { case r"[a-z](-?[a-z0-9]+)*" => ModuleId(name) }
}

case class ModuleId(key: String) extends Key(msg"module")

case class Query(fields: ListMap[String, String]) {
  override def toString: String = if(isEmpty) "" else fields.map { case (field, value) => field + "=" + value }.mkString("?", "&", "")
  def isEmpty: Boolean = fields.isEmpty
  def &(field: (String, String)): Query = copy(fields = fields + field)
}

object Query {
  val empty: Query = apply(ListMap.empty)
  implicit val stringShow: StringShow[Query] = _.toString
  def &(field: (String, String)): Query = empty & field
}

object Uri {
  implicit val msgShow: MsgShow[Uri] = uri => UserMsg { theme => theme.uri(theme.underline(uri.key)) }
  implicit val stringShow: StringShow[Uri] = _.key
  implicit val ogdlWriter: OgdlWriter[Uri] = uri => Ogdl(uri.key)
  
  implicit val ogdlReader: OgdlReader[Uri] = _() match {
    case r"$scheme@([a-z]+):\/\/$path@(.*)$$" => Uri(scheme, Path(path))
  }

  implicit val diff: Diff[Uri] = (l, r) => Diff.stringDiff.diff(l.key, r.key)
}

case class Uri(scheme: String, path: Path, query: Query = Query.empty) extends Key(msg"URI") {
  def key: String = str"${scheme}://${path}${query}"
}

object ImportPath {
  implicit val msgShow: MsgShow[ImportPath] = ip => UserMsg(_.layer(ip.path))
  implicit val stringShow: StringShow[ImportPath] = _.path
  implicit val parser: Parser[ImportPath] = unapply(_)
  val Root: ImportPath = ImportPath("/")

  // FIXME: Actually parse it and check that it's valid
  def unapply(str: String): Option[ImportPath] = Some(ImportPath(str))
}

case class ImportPath(path: String) {

  private[this] lazy val rawParts: List[String] = path.split("/").to[List]

  def parts: List[ImportId] = rawParts match {
    case Nil => Nil
    case "" :: tail => tail.map(ImportId(_))
    case _ => Nil
  }

  def /(importId: ImportId): ImportPath = ImportPath(s"$path/${importId.key}")
  def tail: ImportPath = ImportPath(parts.tail.map(_.key).mkString("/", "/", ""))
  def isEmpty: Boolean = parts.length == 0
  def head: ImportId = parts.head
  
  def prefix(importId: ImportId): ImportPath =
    ImportPath((importId :: parts).map(_.key).mkString("/", "/", ""))

  def dereference(relPath: ImportPath): Try[ImportPath] = {
    import java.nio.file.{ Path, Paths }
    val fakePath = Paths.get(this.path).normalize()
    val fakeRelPath = Paths.get(relPath.path)
    def goesAboveRoot(p: Path): Boolean = {
      import scala.collection.JavaConverters._
      p.iterator().asScala.map(_.toString).map{
        case ".." => -1
        case "." => 0
        case _ => 1
      }.scanLeft(0)(_ + _).exists(_ < 0)
    }
    val resolvedFakePath = fakePath.resolve(fakeRelPath)
    if(goesAboveRoot(resolvedFakePath)) Failure(LayersFailure(relPath))
    else {
      val normalizedFakePath = resolvedFakePath.normalize()
      Success(ImportPath(normalizedFakePath.toString))
    }
  }
}

object PublishedLayer {
  implicit val msgShow: MsgShow[PublishedLayer] =
    publishedLayer => UserMsg { theme => theme.layer(publishedLayer.url.path.value)}
  
  implicit val stringShow: StringShow[PublishedLayer] = pl => str"${pl.url}@${pl.version}"

  implicit val keyName: KeyName[PublishedLayer] = () => msg"layer"
  implicit val diff: Diff[PublishedLayer] = (l, r) => 
    Diff.stringDiff.diff(stringShow.show(l), stringShow.show(r))
  
  def parse(str: String): Option[PublishedLayer] = str.only {
    case r"fury:\/\/$d@([a-z][a-z0-9\-\.]*[a-z0-9])\/$p@([a-z0-9\-\/]*)\@${LayerVersion(v)}@(.*)" =>
      PublishedLayer(FuryUri(d, p), v)
  }
}

case class PublishedLayer(url: FuryUri, version: LayerVersion = LayerVersion.Zero)

object LayerVersion {
  implicit val msgShow: MsgShow[LayerVersion] = layerVersion => UserMsg(_.number(stringShow.show(layerVersion)))
  
  implicit val stringShow: StringShow[LayerVersion] =
    layerVersion => str"${layerVersion.major}.${layerVersion.minor}"
    
  def unapply(str: String): Option[LayerVersion] =
    str.only { case r"$major@([0-9]+)\.$minor@([0-9]+)" => LayerVersion(major.toInt, minor.toInt) }

  final val Zero: LayerVersion = LayerVersion(0, 0)
}

case class LayerVersion(major: Int, minor: Int)

object FuryConf {
  implicit val msgShow: MsgShow[FuryConf] = {
    case FuryConf(ref, path, None)                 => msg"$ref/$path"
    case FuryConf(ref, ImportPath.Root, Some(pub)) => msg"$pub${'@'}$ref"
    case FuryConf(ref, path, Some(pub))            => msg"$pub${'@'}$ref${'/'}$path"
  }
}

case class FuryConf(layerRef: LayerRef, path: ImportPath = ImportPath("/"),
    published: Option[PublishedLayer] = None) {

  def focus(): Focus = Focus(layerRef, path, None)
  def focus(projectId: ProjectId): Focus = Focus(layerRef, path, Some((projectId, None)))

  def focus(projectId: ProjectId, moduleId: ModuleId): Focus =
    Focus(layerRef, path, Some((projectId, Some(moduleId))))
}

case class OauthToken(value: String)

object Focus {
  implicit val msgShow: MsgShow[Focus] = { focus =>
    (focus.path match {
      case ImportPath.Root => msg"${'/'}${'/'}${focus.layerRef}"
      case path            => msg"${'/'}${'/'}${focus.layerRef}$path"
    }) + (focus.focus match {
      case None                          => msg""
      case Some((project, None))         => msg"${'#'}$project"
      case Some((project, Some(module))) => msg"${'#'}$project${'/'}$module"
    })
  }
}

case class Focus(layerRef: LayerRef, path: ImportPath, focus: Option[(ProjectId, Option[ModuleId])])

object AsIpfsRef {
  def unapply(str: String): Option[IpfsRef] = str.only { case r"fury:\/\/$hash@(.{46})" => IpfsRef(hash) }
}

object IpfsRef {
  implicit val parser: Parser[IpfsRef] = AsIpfsRef.unapply(_)
  implicit val msgShow: MsgShow[IpfsRef] = ir => UserMsg(_.layer(ir.key))
  implicit val stringShow: StringShow[IpfsRef] = _.key
}

case class IpfsRef(key: String) extends Key(msg"IPFS ref") with LayerName {
  def suggestedName: Option[ImportId] = None
  def uri: Uri = Uri("fury", Path(key))
  def publishedLayer: Option[PublishedLayer] = None
}

object RemoteLayerId {
  implicit val parser: Parser[RemoteLayerId] = unapply(_)
  def unapply(str: String): Option[RemoteLayerId] = str.only {
    case r"$group@([^/]+)/$name@([^/]+)" => RemoteLayerId(Some(group), name)
    case r"$name@([^/]+)"                => RemoteLayerId(None, name)
  }
}

case class RemoteLayerId(group: Option[String], name: String)

case class Catalog(entries: SortedSet[Artifact])

object Artifact {
  implicit val stringShow: StringShow[Artifact] = _.digest[Sha256].encoded[Base64]
}

case class Artifact(path: String, ref: String, version: LayerVersion)

object LayerRef {
  implicit val msgShow: MsgShow[LayerRef] = lr => UserMsg(_.layer(lr.key.drop(2).take(8)))
  implicit val stringShow: StringShow[LayerRef] = _.key
  implicit val diff: Diff[LayerRef] = (l, r) => Diff.stringDiff.diff(l.key, r.key)
  
  def unapply(value: String): Option[LayerRef] = value.only {
    case r"fury:\/\/$value@(Qm[a-zA-Z0-9]{44})" => LayerRef(value)
    case r"[A-F0-9]{64}" => LayerRef(value)
    case r"Qm[a-zA-Z0-9]{44}" => LayerRef(value)
  }
}

case class LayerRef(key: String) extends Key(msg"layer") { def ipfsRef: IpfsRef = IpfsRef(key) }

case class Config(showContext: Boolean = true,
                  theme: Theme = Theme.Basic,
                  undoBuffer: Int = 5,
                  timestamps: Boolean = true,
                  pipelining: Boolean = false,
                  trace: Boolean = false,
                  skipIpfs: Boolean = false,
                  service: String = "furore.dev",
                  token: Option[OauthToken] = None)

object TargetId {
  implicit val stringShow: StringShow[TargetId] = _.key
  
  def apply(projectId: ProjectId, moduleId: ModuleId): TargetId =
    TargetId(str"${projectId}_${moduleId}")
  
  def apply(ref: ModuleRef): TargetId =
    TargetId(ref.projectId, ref.moduleId)

  implicit val keyName: KeyName[TargetId] = () => msg"foobar"

  implicit val uriParser: Parser[TargetId] = parse(_)

  private[this] def parse(bspUri: String): Option[TargetId] = {
    val uriQuery = new java.net.URI(bspUri).getRawQuery.split("&").map(_.split("=", 2)).map { x => x(0) -> x(1) }.toMap
    uriQuery.get("id").map(TargetId(_))
  }

}

object Pid {
  implicit val stringShow: StringShow[Pid] = pid => Integer.toHexString(pid.pid).padTo(5, '0')

  implicit val msgShow: MsgShow[Pid] =
    pid => UserMsg { theme => msg"${theme.active(stringShow.show(pid))}".string(theme) }
}
case class Pid(pid: Int)

case class TargetId(key: String) extends AnyVal {
  def moduleId: ModuleId = ModuleId(key.split("_")(1))
  def projectId: ProjectId = ProjectId(key.split("_")(0))
  def ref: ModuleRef = ModuleRef(projectId, moduleId)
}

case class BinRepoId(id: String)

object BinRepoId {
  implicit val msgShow: MsgShow[BinRepoId] = v => UserMsg(_.repo(v.id))
  implicit val stringShow: StringShow[BinRepoId] = _.id
  final val Central: BinRepoId = BinRepoId("central")
  implicit val parser: Parser[BinRepoId] = unapply(_)

  def unapply(name: String): Option[BinRepoId] = name.only { case r"[a-z]+" => BinRepoId(name) }
}

object Permission {
  implicit val msgShow: MsgShow[Permission] = stringShow.show
  
  implicit val stringShow: StringShow[Permission] =
    p => str"${p.classRef} ${p.target} ${p.action.getOrElse("-"): String}"
  
  implicit val diff: Diff[Permission] = Diff.gen[Permission]

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
case class Permission(classRef: ClassRef, target: String, action: Option[String]) {
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
case class PermissionHash(key: String) extends Key(msg"permission") {
  def resolve(policy: Set[Permission]): Try[Permission] = {
    val allMatches = policy.filter(_.hash.startsWith(key))
    if (allMatches.size == 1) Success(allMatches.head) else Failure(ItemNotFound(key, "permission"))
  }
}

object ScopeId {
  implicit val stringShow: StringShow[ScopeId] = _.id
  implicit val parser: Parser[ScopeId] = unapply(_)

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
  implicit val diff: Diff[EnvVar] = Diff.gen[EnvVar]
  implicit val parser: Parser[EnvVar] = unapply(_)

  def unapply(str: String): Option[EnvVar] = str.split("=", 2).only {
    case Array(key, value) => EnvVar(key, value)
    case Array(key)        => EnvVar(key, "")
  }
}

case class EnvVar(key: String, value: String)

object JavaProperty {
  implicit val msgShow: MsgShow[JavaProperty] = e => msg"${e.key}=${e.value}"
  implicit val stringShow: StringShow[JavaProperty] = e => str"${e.key}=${e.value}"
  implicit val diff: Diff[JavaProperty] = Diff.gen[JavaProperty]
  implicit val parser: Parser[JavaProperty] = unapply(_)

  def unapply(str: String): Option[JavaProperty] = str.split("=", 2).only {
    case Array(key, value) => JavaProperty(key, value)
    case Array(key)        => JavaProperty(key, "")
  }
}
case class JavaProperty(key: String, value: String)

object Scope {
  def apply(id: ScopeId, layout: Layout, projectId: ProjectId): Scope = id match {
    case ScopeId.Project => ProjectScope(projectId)
    case ScopeId.Directory => DirectoryScope(layout.baseDir.value)
    //case ScopeId.Layer => LayerScope()
  }
}

sealed trait Scope extends scala.Product with scala.Serializable
case class DirectoryScope(dir: String) extends Scope
case class ProjectScope(name: ProjectId) extends Scope
//case class LayerScope(layerHash: String) extends Scope

object BloopSpec {
  implicit val msgShow: MsgShow[BloopSpec] = v => msg"${v.org}:${v.name}"
  implicit val stringShow: StringShow[BloopSpec] = bs => str"${bs.org}:${bs.name}"
  implicit val diff: Diff[BloopSpec] = Diff.gen[BloopSpec]
  implicit val parser: Parser[BloopSpec] = unapply(_)
  implicit val keyName: KeyName[BloopSpec] = () => msg"compiler specification"

  def unapply(str: String): Option[BloopSpec] = str.only {
    case r"$org@([a-z][a-z0-9_\-\.]*):$id@([a-z][a-z0-9_\-\.]*):$version@([0-9a-z][A-Za-z0-9_\-\.]*)" =>
      BloopSpec(org, id, version)
  }
}

case class BloopSpec(org: String, name: String, version: String)

object LineNo { implicit val msgShow: MsgShow[LineNo] = v => UserMsg(_.lineNo(v.line.toString)) }
case class LineNo(line: Int) extends AnyVal

object AliasCmd {
  implicit val msgShow: MsgShow[AliasCmd] = v => UserMsg(_.module(v.key))
  implicit val stringShow: StringShow[AliasCmd] = _.key
  implicit val parser: Parser[AliasCmd] = unapply(_)

  def unapply(value: String): Option[AliasCmd] = value.only { case r"[a-z][a-z0-9\-]+" => AliasCmd(value) }
}

case class AliasCmd(key: String)

object Alias {
  implicit val msgShow: MsgShow[Alias] = v => UserMsg(_.module(v.cmd.key))
  implicit val stringShow: StringShow[Alias] = _.cmd.key
}

case class Alias(cmd: AliasCmd, description: String, module: ModuleRef, args: List[String] = Nil)

object Import {
  implicit val msgShow: MsgShow[Import] = v =>
    UserMsg(msg"${v.layerRef}".string(_))

  implicit val stringShow: StringShow[Import] = sr => str"${sr.layerRef}"
  implicit val diff: Diff[Import] = Diff.gen[Import]
  implicit val parser: Parser[Import] = unapply(_)

  def unapply(value: String): Option[Import] = value.only {
    case r"$layer@(Qm[a-zA-Z0-9]+)$$" =>
      Import(ImportId(""), LayerRef(layer), None)
  }
}

case class Import(id: ImportId, layerRef: LayerRef, remote: Option[PublishedLayer] = None)

object LayerName {
  implicit val msgShow: MsgShow[LayerName] = {
    case layerName: IpfsRef => IpfsRef.msgShow.show(layerName)
    case layerName: FuryUri => FuryUri.msgShow.show(layerName)
    case layerName: FileInput => FileInput.msgShow.show(layerName)
  }

  implicit val parser: Parser[LayerName] = parse(_).toOption

  def parse(path: String): Try[LayerName] = {
    val service = ManagedConfig().service
    path match {
      case r"fury:\/\/$ref@([A-Za-z0-9]{44})\/?" =>
        Success(IpfsRef(str"Qm$ref"))
      case r"fury:\/\/$dom@(([a-z]+\.)+[a-z]{2,})\/$loc@(([a-z][a-z0-9]*\/)+[a-z][0-9a-z]*([\-.][0-9a-z]+)*)" =>
        Success(FuryUri(dom, loc))
      case r".*\.fury" =>
        Success(FileInput(Path(path)))
      case r"([a-z][a-z0-9]*\/)+[a-z][0-9a-z]*([\-.][0-9a-z]+)*" =>
        Success(FuryUri(service, path))
      case name =>
        Failure(InvalidLayer(name))
    }
  }
}

sealed trait LayerName {
  def suggestedName: Option[ImportId]
  def publishedLayer: Option[PublishedLayer]
}

object FileInput {
  implicit val stringShow: StringShow[FileInput] = _.path.value
  implicit val msgShow: MsgShow[FileInput] = fi => UserMsg(_.path(fi.path.value))
}

case class FileInput(path: Path) extends LayerName {
  def suggestedName: Option[ImportId] = path.value.split("/").last.split("\\.").head.only {
    case name@r"[a-z]([a-z0-9]+\-)*[a-z0-9]+" => ImportId(name)
  }
  def publishedLayer: Option[PublishedLayer] = None
}

object FuryUri {
  implicit val msgShow: MsgShow[FuryUri] = fl => UserMsg { theme =>
    msg"${theme.layer(str"fury://${fl.domain}/${fl.path}")}".string(theme)
  }

  implicit val stringShow: StringShow[FuryUri] = fl => str"fury://${fl.domain}/${fl.path}"
  implicit val diff: Diff[FuryUri] = (l, r) => Diff.stringDiff.diff(str"$l", str"$r")
  implicit val parser: Parser[FuryUri] = parse(_)
  implicit val ogdlWriter: OgdlWriter[FuryUri] = uri => Ogdl(stringShow.show(uri))
  implicit val ogdlReader: OgdlReader[FuryUri] = str => parser.parse(str()).get
  
  def parse(str: String): Option[FuryUri] =
    str.only { case r"fury:\/\/$d@([a-z][a-z0-9\-\.]*[a-z0-9])\/$p@([a-z0-9\-\/]*)" => FuryUri(d, p) }
}

case class FuryUri(domain: String, path: String) extends LayerName {
  def suggestedName: Option[ImportId] = Some(ImportId(path.split("/")(1)))
  def publishedLayer: Option[PublishedLayer] = Some(PublishedLayer(FuryUri(domain, path)))
}

object ImportId {
  implicit val msgShow: MsgShow[ImportId] = m => UserMsg(_.layer(m.key))
  implicit val stringShow: StringShow[ImportId] = _.key
  implicit val diff: Diff[ImportId] = (l, r) => Diff.stringDiff.diff(l.key, r.key)
  implicit val parser: Parser[ImportId] = unapply(_)

  def unapply(name: String): Option[ImportId] = name.only { case r"[a-z](-?[a-z0-9]+)*" => ImportId(name) }
}

case class ImportId(key: String) extends Key("import")

object PartialBinSpec {
  implicit val parser: Parser[PartialBinSpec] = unapply(_)
  def unapply(value: String): Option[PartialBinSpec] = value.only {
    case r"$g@([^:]+)"                       => PartialBinSpec(g, None, None)
    case r"$g@([^:]+):$a@([^:]*)"            => PartialBinSpec(g, Some(a), None)
    case r"$g@([^:]+):$a@([^:]*):$v@([^:]*)" => PartialBinSpec(g, Some(a), Some(v))
  }
}

case class PartialBinSpec(group: String, artifact: Option[String], version: Option[String])

object BinaryId {
  implicit val msgShow: MsgShow[BinaryId] = b => UserMsg(_.binary(b.key))
  implicit val stringShow: StringShow[BinaryId] = _.key
  implicit val diff: Diff[BinaryId] = (l, r) => Diff.stringDiff.diff(l.key, r.key)
  implicit val parser: Parser[BinaryId] = unapply(_)
  implicit val keyName: KeyName[BinaryId] = () => msg"binary"
  
  def unapply(name: String): Option[BinaryId] = name.only { case r"[a-z]([_\-\.]?[a-z0-9]+)*" => BinaryId(name) }
}

case class BinaryId(key: String) extends Key("binary")

object BinSpec {
  implicit val msgShow: MsgShow[BinSpec] = b => UserMsg(_.binary(b.string))
  implicit val stringShow: StringShow[BinSpec] = _.string
  implicit val diff: Diff[BinSpec] = (l, r) => Diff.stringDiff.diff(l.string, r.string)
  implicit val parser: Parser[BinSpec] = unapply(_)
  implicit val keyName: KeyName[BinSpec] = () => msg"binary specification"

  // FIXME: Parse content better
  def unapply(name: String): Option[BinSpec] = name.only { case r"([^:]+):([^:]+):([^:]+)" => BinSpec(name) }
}

case class BinSpec(string: String)

object Version {
  implicit val msgShow: MsgShow[Version] = b => UserMsg(_.version(b.key))
  implicit val stringShow: StringShow[Version] = _.key
  implicit val diff: Diff[Version] = (l, r) => Diff.stringDiff.diff(l.key, r.key)
  implicit val parser: Parser[Version] = unapply(_)

  def unapply(name: String): Option[Version] = name.only { case r"\d+(\.\d+)*" => Version(name) }
}

case class Version(key: String) extends Key("version")

object ModuleRef {
  implicit val stringShow: StringShow[ModuleRef] = ref => str"${ref.projectId}/${ref.moduleId}"
  implicit val entityName: EntityName[ModuleRef] = EntityName(msg"dependency")
  implicit val parser: Parser[ModuleRef] = parseFull(_, false)
  
  val JavaRef = ModuleRef(ProjectId("java"), ModuleId("compiler"), false)

  implicit val diff: Diff[ModuleRef] =
    (l, r) => if(l == r) Nil else List(Difference(msg"ref", msg"", msg"$l", msg"$r"))

  implicit val msgShow: MsgShow[ModuleRef] = ref =>
    UserMsg { theme =>
      msg"${theme.project(ref.projectId.key)}${theme.gray("/")}${theme.module(ref.moduleId.key)}".string(theme)
    }

  def parseFull(string: String, intransitive: Boolean): Option[ModuleRef] = string.only {
    case r"$projectId@([a-z][a-z0-9\-]*[a-z0-9])\/$moduleId@([a-z][a-z0-9\-]*[a-z0-9])" =>
      ModuleRef(ProjectId(projectId), ModuleId(moduleId), intransitive)
  }

  def parse(projectId: ProjectId, string: String, intransitive: Boolean): Option[ModuleRef] = string.only {
    case r"$projectId@([a-z](-?[a-z0-9]+)*)\/$moduleId@([a-z](-?[a-z0-9]+)*)" =>
      ModuleRef(ProjectId(projectId), ModuleId(moduleId), intransitive)
    case r"[a-z](-?[a-z0-9]+)*" =>
      ModuleRef(projectId, ModuleId(string), intransitive)
  }
}

case class ModuleRef(projectId: ProjectId,
                     moduleId: ModuleId,
                     intransitive: Boolean = false,
                     hidden: Boolean = false) {

  override def equals(that: Any): Boolean =
    that.only { case ModuleRef(p, m, _, _) => projectId == p && moduleId == m }.getOrElse(false)

  def hide = copy(hidden = true)
  override def hashCode: Int = projectId.hashCode + moduleId.hashCode
  override def toString: String = str"$projectId/$moduleId"
}

object ClassRef {
  implicit val msgShow: MsgShow[ClassRef] = r => UserMsg(_.layer(r.key))
  implicit val stringShow: StringShow[ClassRef] = _.key
  implicit val diff: Diff[ClassRef] = (l, r) => Diff.stringDiff.diff(l.key, r.key)
  implicit val parser: Parser[ClassRef] = unapply(_)

  // FIXME: Parse
  def unapply(name: String): Some[ClassRef] = Some(ClassRef(name))
}
  
case class ClassRef(key: String) extends Key(msg"class")

object PluginId {
  implicit val msgShow: MsgShow[PluginId] = r => UserMsg(_.module(r.key))
  implicit val stringShow: StringShow[PluginId] = _.key
  implicit val diff: Diff[PluginId] = (l, r) => Diff.stringDiff.diff(l.key, r.key)
  implicit val parser: Parser[PluginId] = unapply(_)
  
  def unapply(name: String): Some[PluginId] = Some(PluginId(name))
}

case class PluginId(key: String) extends Key(msg"plugin")
  
object ExecName {
  implicit val msgShow: MsgShow[ExecName] = r => UserMsg(_.module(r.key))
  implicit val stringShow: StringShow[ExecName] = _.key
  implicit val diff: Diff[ExecName] = (l, r) => Diff.stringDiff.diff(l.key, r.key)
  implicit val parser: Parser[ExecName] = unapply(_)
  
  def unapply(name: String): Some[ExecName] = Some(ExecName(name))
}

case class ExecName(key: String) extends Key(msg"executable")
  
case class Plugin(id: PluginId, ref: ModuleRef, main: ClassRef)

object RepoId {
  implicit val msgShow: MsgShow[RepoId]       = r => UserMsg(_.repo(r.key))
  implicit val stringShow: StringShow[RepoId] = _.key
  implicit val parser: Parser[RepoId] = unapply(_)
  implicit val keyName: KeyName[RepoId] = () => msg"repo"
  
  def unapply(name: String): Option[RepoId] = name.only { case r"[a-z](-?[a-z0-9]+)*" => RepoId(name) }
}

case class RepoId(key: String) extends Key(msg"repository")

object Opt {
  implicit val stringShow: StringShow[Opt] = _.id.key
  implicit val msgShow: MsgShow[Opt] = v => UserMsg(_.param(v.id.key))
  implicit val diff: Diff[Opt] = (l, r) => Diff.stringDiff.diff(l.id.key, r.id.key)
}

case class Opt(id: OptId, persistent: Boolean, remove: Boolean) {
  def parameter = str"-${id.key}"
  def transform(optDefs: Set[OptDef]): List[String] =
    optDefs.find(_.id == id).map(_.transform).getOrElse(List(str"-${id.key}"))
}


sealed trait Origin
object Origin {
  case object Compiler extends Origin
  case object Local extends Origin
  case object Plugin extends Origin
  case class Module(ref: ModuleRef) extends Origin
}

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
  implicit val parser: Parser[LicenseId] = unapply(_)

  def unapply(value: String): Option[LicenseId] =
    value.only { case r"[a-z]([a-z0-9\-\.]?[a-z0-9])*" => LicenseId(value) }
}

case class LicenseId(key: String) extends Key(msg"license")

case class License(id: LicenseId, name: String)

object RefSpec {
  implicit val msgShow: MsgShow[RefSpec] = v => UserMsg(_.version(v.id))
  implicit val stringShow: StringShow[RefSpec] = _.id
  implicit val parser: Parser[RefSpec] = unapply(_)
  val master: RefSpec = RefSpec("master")
  def unapply(value: String): Option[RefSpec] = Some(RefSpec(value))
}

case class RefSpec(id: String)

object OptDef {
  implicit val stringShow: StringShow[OptDef] = _.transform.mkString(" ")
  implicit val msgShow: MsgShow[OptDef] = v => UserMsg(_.param(v.transform.mkString(" ")))
  implicit val diff: Diff[OptDef] = Diff.gen[OptDef]
}

case class OptDef(id: OptId, description: String, transform: List[String], persistent: Boolean) {
  def opt(compiler: ModuleRef, source: Origin): Provenance[Opt] =
    Provenance(Opt(id, persistent = true, remove = false), compiler, source)
}

case class Provenance[T](value: T, compiler: ModuleRef, source: Origin)

object OptId {
  implicit val stringShow: StringShow[OptId] = _.key
  implicit val msgShow: MsgShow[OptId] = v => UserMsg(_.param(v.key))
  implicit val parser: Parser[OptId] = unapply(_)
  
  def unapply(value: String): Option[OptId] =
    value.only { case r"[a-z]([a-z0-9\-\.]?[a-z0-9])*" => OptId(value) }
}

case class OptId(key: String) extends Key(msg"option")

object Commit {
  implicit val stringShow: StringShow[Commit] = _.id
  implicit val msgShow: MsgShow[Commit]       = r => UserMsg(_.version(r.id.take(7)))
}

case class Commit(id: String)
