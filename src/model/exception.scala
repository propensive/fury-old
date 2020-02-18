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

import fury.strings._, fury.io._

case class AlreadyInitialized() extends FuryException
case class BspException() extends FuryException
case class BuildServerError(cause: Throwable) extends FuryException
case class CantWatchAndWait() extends FuryException
case class CompilationFailure() extends FuryException
case class CyclesInDependencies(cycle: Set[ModuleRef]) extends FuryException
case class DnsLookupFailure(domain: String) extends FuryException
case class DnsResolutionFailure() extends FuryException
case class DownloadFailure(detail: String) extends FuryException
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
case class ImportOnlyFileOrRef() extends FuryException
case class InitFailure() extends FuryException
case class InvalidKind(expected: Kind) extends FuryException
case class InvalidLayer(value: String) extends FuryException
case class InvalidValue(value: String) extends FuryException
case class IpfsNotOnPath() extends FuryException
case class LauncherFailure(msg: String) extends FuryException
case class LayersFailure(layer: ImportPath) extends FuryException
case class MissingCommand() extends FuryException
case class ModuleAlreadyExists(module: ModuleId) extends FuryException
case class NoLatestVersion() extends FuryException
case class NoPermissions(permissions: Set[Permission]) extends FuryException
case class NoSourcesError(repoId: RepoId, commit: Commit, sources: UserMsg) extends FuryException
case class NotInitialized(dir: Path) extends FuryException
case class OfflineException() extends FuryException
case class ProjectAlreadyExists(project: ProjectId) extends FuryException
case class ProjectConflict(ids: Set[ProjectId]/*, h1: Hierarchy, h2: Hierarchy*/) extends FuryException
case class RepoNotForked(repo: RepoId) extends FuryException
case class SchemaDifferences() extends FuryException
case class UnknownBinaryRepository(repoId: BinRepoId) extends FuryException
case class UnknownCommand(command: String) extends FuryException
case class UnknownCompiler() extends FuryException
case class UnknownModule(moduleRef: ModuleRef) extends FuryException
case class UnknownOs(description: String) extends FuryException
case class UnspecifiedBinary(matchingBinaries: List[String]) extends FuryException
case class UnspecifiedMain(module: ModuleId) extends FuryException

object ItemNotFound {
  def apply[K <: Key: MsgShow](key: K): ItemNotFound = ItemNotFound(implicitly[MsgShow[K]].show(key), key.kind)
}

case class ItemNotFound(item: UserMsg, kind: UserMsg) extends FuryException

object NotUnique {
  def apply[K <: Key: MsgShow](key: K): NotUnique = NotUnique(implicitly[MsgShow[K]].show(key), key.kind)
}

case class NotUnique(item: UserMsg, kind: UserMsg) extends FuryException
