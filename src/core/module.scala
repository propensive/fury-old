package fury.core

import fury.model._, fury.strings._, fury.io._

import scala.util._
import scala.collection.immutable._

object Module {
  implicit val msgShow: MsgShow[Module]       = v => UserMsg(_.module(v.id.key))
  implicit val stringShow: StringShow[Module] = _.id.key
  implicit val diff: Diff[Module] = Diff.gen[Module]

  def available(id: ModuleId, project: Project): Try[ModuleId] =
    project.modules.find(_.id == id).fold(Try(id)) { module => Failure(ModuleAlreadyExists(module.id)) }
}

case class Module(id: ModuleId,
                  kind: Kind = Library,
                  main: Option[String] = None,
                  plugin: Option[String] = None,
                  manifest: List[ManifestEntry] = List(),
                  compiler: ModuleRef = ModuleRef.JavaRef,
                  after: SortedSet[ModuleRef] = TreeSet(),
                  params: SortedSet[Parameter] = TreeSet(),
                  sources: SortedSet[Source] = TreeSet(),
                  binaries: SortedSet[Binary] = TreeSet(),
                  resources: SortedSet[Path] = TreeSet(),
                  bloopSpec: Option[BloopSpec] = None,
                  environment: SortedSet[EnvVar] = TreeSet(),
                  properties: SortedSet[JavaProperty] = TreeSet(),
                  policy: SortedSet[Permission] = TreeSet(),
                  hidden: Boolean = false) {

  def allBinaries: SortedSet[Binary] = if(kind == Benchmarks) binaries + Binary.Jmh else binaries
  def compilerDependencies: Set[ModuleRef] = Set(compiler).filter(_ != ModuleRef.JavaRef).map(_.hide)
  def ref(project: Project): ModuleRef = ModuleRef(project.id, id, hidden = hidden)
  def externalSources: SortedSet[ExternalSource] = sources.collect { case src: ExternalSource => src }
  def sharedSources: SortedSet[SharedSource] = sources.collect { case src: SharedSource => src }
  def localSources: SortedSet[Path] = sources.collect { case src: LocalSource => src.path }
  def policyEntries: Set[PermissionEntry] = {
    val prefixLength = Compare.uniquePrefixLength(policy.map(_.hash)).max(3)
    policy.map { p => PermissionEntry(p, PermissionHash(p.hash.take(prefixLength))) }
  }
}
