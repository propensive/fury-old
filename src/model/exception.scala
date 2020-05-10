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
package fury.model

import fury.strings._, fury.io._


case class AlreadyInitialized() extends FuryException
case class AlreadyCheckedOut(repo: RepoId) extends FuryException
case class BranchNotFound(commit: Commit) extends FuryException
case class BspException() extends FuryException
case class BuildServerError(cause: Throwable) extends FuryException
case class CannotUndo() extends FuryException
case class CannotUpdateRepo(repo: RepoId) extends FuryException
case class ConflictingFiles(files: List[Path]) extends FuryException
case class CompilationFailure() extends FuryException
case class CyclesInDependencies(cycle: Set[ModuleRef]) extends FuryException
case class CommitNotInRepo(commit: Commit) extends FuryException
case class DnsLookupFailure(domain: String) extends FuryException
case class DnsResolutionFailure() extends FuryException
case class DownloadFailure(detail: String) extends FuryException
case class EnvPathNotSet() extends FuryException
case class EnvHomeNotSet() extends FuryException
case class ExecutionFailure(exitCode: Int) extends FuryException
case class GraalVMError(message: String) extends FuryException
case class HistoryMissing() extends FuryException
case class HistoryCorrupt() extends FuryException
case class HttpBadGateway(uri: Uri) extends FuryException
case class HttpBadRequest(uri: Uri) extends FuryException
case class HttpError(code: Int, uri: Uri) extends FuryException
case class HttpForbidden(uri: Uri) extends FuryException
case class HttpGatewayTimeout(uri: Uri) extends FuryException
case class HttpInternalServerError(uri: Uri) extends FuryException
case class HttpMethodNotAllowed(uri: Uri) extends FuryException
case class HttpNotFound(uri: Uri) extends FuryException
case class HttpNotImplemented(uri: Uri) extends FuryException
case class HttpServiceUnavailable(uri: Uri) extends FuryException
case class HttpUnauthorized(uri: Uri) extends FuryException
case class ImportHasNoRemote() extends FuryException
case class ImportOnlyFileOrRef() extends FuryException
case class InitFailure() extends FuryException
case class InvalidKind(expected: Kind) extends FuryException
case class InvalidLayer(value: String) extends FuryException
case class InvalidValue(value: String) extends FuryException
case class InvalidVersion() extends FuryException
case class InvalidSource(source: UserMsg, module: ModuleRef) extends FuryException
case class IpfsTimeout() extends FuryException
case class NotOnPath(name: ExecName) extends FuryException
case class NotAuthenticated() extends FuryException
case class LauncherFailure(msg: String) extends FuryException
case class LayersFailure(layer: ImportPath) extends FuryException
case class LayerNotFound(path: Path) extends FuryException
case class LayerContainsLocalSources(refs: List[ModuleRef]) extends FuryException
case class NoRepoCheckedOut() extends FuryException
case class MergeConflicts() extends FuryException
case class MissingCommand() extends FuryException
case class ModuleAlreadyExists(module: ModuleId) extends FuryException
case class NoLatestVersion() extends FuryException
case class NoOtherLayer() extends FuryException
case class NoPermissions(permissions: Set[Permission]) extends FuryException
case class NoSourcesError(repoId: RepoId, commit: Commit, sources: UserMsg) extends FuryException
case class NotInitialized(dir: Path) extends FuryException
case class OfflineException() extends FuryException
case class ProjectAlreadyExists(project: ProjectId) extends FuryException
case class PublishFailure() extends FuryException
case class ProjectConflict(ids: Set[ProjectId]) extends FuryException
case class RemoteNotSynched(repo: RepoId, remote: String) extends FuryException
case class RepoAlreadyForked(repo: RepoId, dir: Path) extends FuryException
case class RepoDirty(repo: RepoId, changes: String) extends FuryException
case class RepoNotForked(repo: RepoId) extends FuryException
case class RepoNotFound(base: Path) extends FuryException
case class RootLayerNotSelected() extends FuryException
case class UnknownBinaryRepository(repoId: BinRepoId) extends FuryException
case class UnknownCommand(command: String) extends FuryException
case class UnknownCompiler() extends FuryException
case class UnknownVersion(version: LayerVersion) extends FuryException
case class UnknownModule(moduleRef: ModuleRef) extends FuryException
case class UnresolvedModules(refs: Map[ModuleRef, Set[ModuleRef]]) extends FuryException
case class UnknownOs(description: String) extends FuryException
case class UnspecifiedBinary(matchingBinaries: List[String]) extends FuryException
case class UnspecifiedMain(module: ModuleId) extends FuryException
case class WorkingDirectoryConflict(fs: List[Path]) extends FuryException

object ItemNotFound {
  def apply[K <: Key: MsgShow](key: K): ItemNotFound = ItemNotFound(implicitly[MsgShow[K]].show(key), key.kind)
}

case class ItemNotFound(item: UserMsg, kind: UserMsg) extends FuryException

object NotUnique {
  def apply[K <: Key: MsgShow](key: K): NotUnique = NotUnique(implicitly[MsgShow[K]].show(key), key.kind)
}

case class NotUnique(item: UserMsg, kind: UserMsg) extends FuryException
