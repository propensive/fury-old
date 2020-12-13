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
package fury

import fury.text._, fury.core._, fury.ogdl._, fury.io._, fury.model._

import guillotine._
import jovian._

import scala.util._

object Recovery {

  def recover(cli: Cli)(result: Try[ExitStatus])(implicit log: Log): ExitStatus = result match {
    case Success(exitStatus) =>
      exitStatus
    case Failure(err) =>
      err match {
        case EarlyCompletions() =>
          Done
        case ProjectConflict(ids) =>
          val table = Tables().entities
          val conflicts = Tables().show(table, cli.cols, ids.to[List], false, None, None: Option[String], "hash")
          log.info(msg"Conflicting projects exist in the build:")
          log.info(conflicts)
          cli.abort(msg"You will need to unify these projects before you can continue.")
        case InitFailure() =>
          cli.abort(msg"Could not start the bloop server.")
        case WorkingDirectoryConflict(files) =>
          cli.abort(msg"""Checking out repo into the current working directory would overwrite the files """+
              msg"""${'{'}${files.map { f => msg"$f" }.reduce { (a, b) => msg"$a${','} $b" }}${'}'}.""")
        case AlreadyCheckedOut(repo) =>
          val grabArg: CliParam = Args.GrabArg
          cli.abort(msg"""There is already a Git repo checked out in the working directory. Use """+
              msg"""${grabArg} to use this existing repo as $repo.""")
        case NoLatestVersion() =>
          cli.abort(msg"Could not determine the current latest version of Fury.")
        case NotOnPath(name) =>
          cli.abort(msg"The required executable $name was not found on the PATH.")
        case NotAuthenticated() =>
          cli.abort(msg"You are not authenticated.")
        case NoRepoCheckedOut() =>
          cli.abort(msg"There is no repository currently checked out.")
        case MergeConflicts() =>
          cli.call()
          cli.layout.foreach(Layer.showMergeConflicts(_))
          cli.abort(msg"It is not possible to continue until merge conflicts have been resolved.")
        case NoOtherLayer() =>
          cli.abort(msg"The layer to compare this layer with has not been specified.")
        case ImportOnlyFileOrRef() =>
          cli.abort(msg"Please specify either a file or a layer reference; not both.")
        case ImportHasNoRemote(path) =>
          cli.abort(msg"The layer $path cannot be updated because its remote is not known.")
        case InstallFailed(software, path) =>
          cli.abort(msg"Installation of $software to $path failed.")
        case FileWriteError(path, e) =>
          cli.abort(msg"Couldn't write to file $path. Cause: ${e.toString}")
        case FileNotFound(path) =>
          cli.abort(
              msg"""Could not find the file at $path.""")
        case NoPermissions(perms) =>
          val prefixLength = Compare.uniquePrefixLength(perms.map(_.hash)).max(3)
          val rows = perms.map { p => PermissionEntry(p, PermissionHash(p.hash.take(prefixLength))) }.to[List]
          
          val permissions = Tables().show[PermissionEntry, PermissionEntry](Tables().permissions, cli.cols,
              rows, false, None, None, "hash")

          cli.abort(msg"""The following permissions are required to run the build:
${permissions}
You can grant these permissions with,

  fury permission grant -P <permission hash>
""")
        case BspException() =>
          cli.abort(msg"It was not possible to establish a BSP connection")
        case LauncherFailure(msg) =>
          cli.abort(msg"Bloop did not start successfully: $msg")
        case LayerNotFound(path) =>
          cli.abort(
            msg"""Could not find the layer file at $path. Run `fury layer init` to create a new layer.""")
        case UnknownLayer(path, service) =>
          cli.abort(msg"The layer ${Pointer(path)} does not exist on $service")
        case e: ServiceException =>
          cli.abort(e.getMessage)
        case BadParams(arg1, arg2) =>
          cli.abort(msg"""The ${arg1} parameter cannot be used at the same time as ${arg2}.""")
        case BadParamValue(param, value) =>
          cli.abort(msg"""The value '$value' is not valid for the parameter $param""")
        case MissingParamChoice(param@_*) =>
          val paramsTxt = param.map { p => msg"$p" }.reduce(_+msg"${','} "+_)
          cli.abort(msg"""One of ${'{'}${paramsTxt}${'}'} must be specified.""")
        case MissingParam(param) =>
          cli.abort(msg"""The ${param} parameter is required.""")
        case CannotUndo() =>
          cli.abort(msg"""The previous action cannot be undone.""")
        case UnresolvedModules(refs) =>
          cli.abort(msg"""The layer refers to modules which cannot be resolved: ${refs.toString}""")
        case PublishFailure() =>
          cli.abort(msg"""The server was not able to publish this layer.""")
        case LayerContainsLocalSources(refs) =>
          val plural = if(refs.size > 1) "s" else ""
          cli.abort(msg"""The module$plural contains references to local sources.""")
        case RootLayerNotSelected(path) =>
          cli.abort(msg"The selected layer is not a root layer (${Pointer.Root}). To publish this layer "+
              msg"($path) anyway, please specify the ${Args.ForceArg: CliParam} parameter.")
        case ModuleIsNotCompiler(ref, compRef) =>
          cli.abort(msg"The module $ref specifies a module ($compRef) which is not a compiler)")
        case DownloadFailure(msg) =>
          cli.abort(msg"Coursier could not complete a download: $msg")
        case DnsResolutionFailure() =>
          cli.abort(msg"Coursier could not download a file because DNS resolution failed.")
        case BranchNotFound(commit) =>
          cli.abort(msg"A branch corresponding to the commit $commit could not be found.")
        case BranchDoesNotExist(branch) =>
          cli.abort(msg"The branch $branch does not exist.")
        case OfflineException() =>
          cli.abort(msg"Coursier could not download a file because you appear to be offline.")
        case UnknownVersion(v) =>
          cli.abort(msg"The version $v does not exist.")
        case UnknownOs(os) =>
          cli.abort(msg"The operating system '$os' was not recognized.")
        case e: MissingCommand =>
          cli.abort(msg"No command was provided.")
        case e: UnknownCommand =>
          cli.abort(msg"The command '${e.command}' was not recognized.")
        case NoRemoteInferred() =>
          cli.abort(msg"A remote was not specified, and could not be inferred from the path.")
        case NoPublishedName(layerRef) =>
          cli.abort(msg"The layer $layerRef does not have a published name.")
        case exoskeleton.InvalidArgValue(param, arg) =>
          cli.abort(msg"The argument '$arg' was not a valid value for the parameter $param.")
        case InvalidLayer(layer) =>
          cli.abort(msg"The argument '$layer' does not represent a valid layer.")
        case e: ConfigFormatError =>
          cli.abort(msg"The configuration file could not be read.")
        case e: InvalidValue =>
          cli.abort(msg"'${e.value}' is not a valid value.")
        case ComponentNotDefined(inner, outer) =>
          cli.abort(msg"The ${inner.kind} ${inner.key} is not defined in ${outer.key}.")
        case e: IpfsTimeout =>
          cli.abort(msg"An IPFS operation timed out.")
        case OgdlException(path, msg) =>
          cli.abort(msg"Failed to read OGDL path ${path.mkString(".")}: $msg")
        case OgdlReadException(path, e) =>
          cli.abort(msg"Could not read OGDL from ${path}. Cause: ${e.toString}.")
        case PathNotGitDir() =>
          cli.abort(msg"The path is not a valid git directory")
        case e: ItemNotFound =>
          cli.abort(msg"The ${e.kind} ${e.item} was not found.")
        case CantResolveLayer(path) =>
          cli.abort(msg"Can't resolve layer $path")
        case RepoNotForked(repo) =>
          cli.abort(msg"The repository ${repo} has not been forked.")
        case RepoNotFound(base) =>
          cli.abort(msg"There is no .git directory in $base.")
        case RepoAlreadyForked(repo, dir) =>
          cli.abort(msg"The repository ${repo} is already forked to ${dir}.")
        case RepoDirty(repo, changes) =>
          cli.abort(msg"The repository ${repo} has uncommitted changes ($changes).")
        case UntrackedFiles(files) =>
          val fileSet = files.map { f => msg"$f" }.reduce(_ + msg"${','} " + _)
          cli.abort(msg"The directory contains untracked file${if(files.size == 1) "" else "s"} ${fileSet}")
        case RemoteNotSynched(repo, remote) =>
          cli.abort(msg"The repository ${repo} has not been synchronized with its remote, $remote.")
        case NoRepl(compiler) =>
          cli.abort(msg"The compiler $compiler does not have a REPL.")
        case CannotUpdateRepo(repo) =>
          cli.abort(msg"Could not update the repository $repo.")
        case ConflictingFiles(files) =>
          val fileSet = files.map { f => msg"$f" }.reduce(_ + msg"${','} " + _)
          cli.abort(msg"The directory contains the file${if(files.size == 1) "" else "s"} ${fileSet} which "+
              msg"would be overwritten by checking out the repository here.")
        case e: NotUnique =>
          cli.abort(msg"The ${e.kind} ${e.item} already exists.")
        case CommitNotInRepo(commit, origin) =>
          cli.abort(msg"The commit $commit does not exist in the repository $origin.")
        case Unspecified(kind) =>
          cli.abort(msg"The $kind has not been specified.")
        case UnknownModule(moduleRef: ModuleRef) =>
          cli.abort(msg"The module reference $moduleRef could not be resolved.")
        case UnspecifiedMain(moduleId: ModuleId) =>
          cli.abort(msg"Main class not defined for module '${moduleId}'.")
        case GraalVMError(message: String) =>
          cli.abort(msg"Problem with GrallVM: '${message}'. Please double-check the PATH")
        case BuildServerError(cause: Throwable) =>
          val stack = rootCause(cause).getStackTrace.mkString("\n  at ")
          cli.abort(msg"Problem with the build server: '${cause.toString}'.${"\n  at "}$stack")
        case InvalidKind(expected: Kind.Id) =>
          cli.abort(msg"The module must be of type ${expected}.")
        case e: UnknownCompiler =>
          cli.abort(msg"This compiler is not known.")
        case UnknownBinaryRepository(repoId: BinRepoId) =>
          cli.abort(msg"The binary repository $repoId could not be resolved.")
        case NoSourcesError(repoId, commit, sources) =>
          cli.abort(msg"The repository $repoId did not contain the sources $sources at commit $commit.")
        case e: ShellFailure =>
          cli.abort(msg"An error occurred while running: ${e.command}${"\n"}${e.stdout}${"\n"}${e.stderr}")
        case e: CompilationFailure =>
          cli.abort(msg"One of the compile tasks failed. Check the logs for details.")
        case ExecutionFailure(exitCode) =>
          cli.abort(msg"One of the run tasks failed (exit code $exitCode). Check the logs for details.")
        case e: AlreadyInitialized =>
          cli.abort(msg"Fury is already initialized in this directory. Use ${Args.ForceArg: CliParam} to override.")
        case CyclesInDependencies(refs) =>
          cli.abort(msg"The build graph contains cycles involving ${'{'}${refs.map { d => msg"$d" }.reduce {
              (l, r) => msg"$l${','} $r"}}${'}'}")
        case UnspecifiedBinary(Nil) =>
          cli.abort(msg"Binary not found.")
        case UnspecifiedBinary(possibleBinaries) =>
          cli.abort(msg"Unable to identify target binary: ${"\n\t"}${possibleBinaries.mkString("\n\t")}")
        case HttpBadRequest(url) =>
          cli.abort(msg"HTTP error 401 (Bad Request) when attempting to access $url.")
        case HttpUnauthorized(url) => 
          cli.abort(msg"HTTP error 402 (Unauthorized) when attempting to access $url.")
        case HttpForbidden(url) => 
          cli.abort(msg"HTTP error 403 (Forbidden) when attempting to access $url.")
        case HttpNotFound(url) => 
          cli.abort(msg"HTTP error 404 (Not Found) when attempting to access $url.")
        case HttpMethodNotAllowed(url) => 
          cli.abort(msg"HTTP error 405 (Method Not Allowed) when attempting to access $url.")
        case HttpInternalServerError(url) => 
          cli.abort(msg"HTTP error 500 (Internal Server Error) when attempting to access $url.")
        case HttpNotImplemented(url) => 
          cli.abort(msg"HTTP error 501 (Not Implemented) when attempting to access $url.")
        case HttpBadGateway(url) => 
          cli.abort(msg"HTTP error 502 (Bad Gateway) when attempting to access $url.")
        case HttpServiceUnavailable(url) => 
          cli.abort(msg"HTTP error 503 (Service Unavailable) when attempting to access $url.")
        case HttpGatewayTimeout(url) => 
          cli.abort(msg"HTTP error 504 (Gateway Timeout) when attempting to access $url.")
        case DnsLookupFailure(domain) => 
          cli.abort(msg"Could not do a DNS lookup for $domain.")
        case HttpError(code, url) => 
          cli.abort(msg"HTTP error ${code.toString} when attempting to access $url.")
        case LayersFailure(layerPath) =>
          cli.abort(msg"Layer name ${layerPath.path} not found.")
        case e =>
          val errorString =
            s"\n$e\n${rootCause(e).getStackTrace.to[List].map(_.toString).join("    at ", "\n    at ", "")}"
          val result = for {
            layout <- cli.layout
            _      <- ~Log().fail(errorString)
            _      <- ~Log().await()
          } yield
            cli.abort(msg"An unexpected error occurred:$errorString")

          def unloggable = cli.abort("An unexpected error occurred which could not be logged to disk.\n\n"+
              errorString) 

          result.recover {
            case e: FileWriteError   => unloggable
            case e: FileNotFound     => unloggable
            case e: EarlyCompletions => cli.abort("")
          }.toOption.getOrElse(unloggable)
      }
  }

  private def rootCause(t: Throwable) = {
    Stream.iterate(t)(_.getCause).takeWhile(_ != null).last
  }
}
