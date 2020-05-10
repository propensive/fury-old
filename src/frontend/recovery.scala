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
package fury

import fury.strings._, fury.core._, fury.ogdl._, fury.io._, fury.model._

import guillotine._

import scala.util._

object Recovery {

  def recover(cli: Cli)(result: Try[ExitStatus])(implicit log: Log): ExitStatus = result match {
    case Success(exitStatus) =>
      exitStatus
    case Failure(err) =>
      err match {
        case EarlyCompletions() =>
          Done
        case ProjectConflict(ps/*, h1, h2*/) =>
          val projectIds = ps.toSeq.sortBy(_.key).map { x => msg"$x" }
          val message = msg"Your dependency tree contains references to two or more conflicting projects: "
          val beginning = projectIds.tail.foldLeft(message + projectIds.head)(_ + ", " + _)
          //val ending = msg". The conflicting hierarchies are located at ${h1.dir} and ${h2.dir}"
          cli.abort(beginning)//+ ending)
        case InitFailure() =>
          cli.abort(msg"Could not start the bloop server.")
        case WorkingDirectoryConflict(files) =>
          cli.abort(msg"""Checking out repo into the current working directory would overwrite the files """+
              msg"""${files.foldLeft(msg"${'{'}") { (a, b) => msg"$a${','} $b" }}${'}'}.""")
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
        case e: ServiceException =>
          cli.abort(e.getMessage)
        case BadParams(arg1, arg2) =>
          cli.abort(msg"""The ${arg1} parameter cannot be used at the same time as ${arg2}.""")
        case MissingParamChoice(param1, param2) =>
          cli.abort(msg"""Either the ${param1} or ${param2} parameter must be specified.""")
        case MissingParam(param) =>
          cli.abort(msg"""The ${param} parameter is required.""")
        case CannotUndo() =>
          cli.abort(msg"""The previous action cannot be undone.""")
        case UnresolvedModules(refs) =>
          cli.abort(msg"""Some modules contain references to other modules which do not exist.""")
        case PublishFailure() =>
          cli.abort(msg"""The server was not able to publish this layer.""")
        case LayerContainsLocalSources(refs) =>
          val plural = if(refs.size > 1) "s" else ""
          cli.abort(msg"""The module$plural contains references to local sources.""")
        case RootLayerNotSelected() =>
          cli.abort(msg"A layer can only be published from the root layer (${ImportPath.Root})")
        case DownloadFailure(msg) =>
          cli.abort(msg"Coursier could not complete a download: $msg")
        case DnsResolutionFailure() =>
          cli.abort(msg"Coursier could not download a file because DNS resolution failed.")
        case BranchNotFound(commit) =>
          cli.abort(msg"A branch corresponding to the commit $commit could not be found.")
        case OfflineException() =>
          cli.abort(msg"Coursier could not download a file because you appear to be offline.")
        case UnknownVersion(v) =>
          cli.abort(msg"The version $v does not exist.")
        case e: MissingCommand =>
          cli.abort(msg"No command was provided.")
        case e: UnknownCommand =>
          cli.abort(msg"The command '${e.command}' was not recognized.")
        case exoskeleton.InvalidArgValue(param, arg) =>
          cli.abort(msg"The argument '$arg' was not a valid value for the parameter $param.")
        case InvalidLayer(layer) =>
          cli.abort(msg"The argument '$layer' does not represent a valid layer.")
        case e: ConfigFormatError =>
          cli.abort(msg"The configuration file could not be read.")
        case e: InvalidValue =>
          cli.abort(msg"'${e.value}' is not a valid value.")
        case InvalidSource(source, module) =>
          cli.abort(msg"Source ${source} is not defined in ${module}.")
        case e: IpfsTimeout =>
          cli.abort(msg"An IPFS operation timed out.")
        case OgdlException(error) =>
          cli.abort(msg"Failed to read OGDL: $error.")
        case OgdlReadException(path, e) =>
          cli.abort(msg"Could not read OGDL from ${path}. Cause: ${e.toString}.")
        case e: ItemNotFound =>
          cli.abort(msg"The ${e.kind} ${e.item} was not found.")
        case RepoNotForked(repo) =>
          cli.abort(msg"The repository ${repo} has not been forked.")
        case RepoNotFound(base) =>
          cli.abort(msg"There is no .git directory in $base.")
        case RepoAlreadyForked(repo, dir) =>
          cli.abort(msg"The repository ${repo} is already forked to ${dir}.")
        case RepoDirty(repo, changes) =>
          cli.abort(msg"The repository ${repo} has uncommitted changes ($changes).")
        case RemoteNotSynched(repo, remote) =>
          cli.abort(msg"The repository ${repo} has not been synchronized with its remote, $remote.")
        case CannotUpdateRepo(repo) =>
          cli.abort(msg"Could not update the repository $repo.")
        case ConflictingFiles(files) =>
          cli.abort(msg"The directory contains files which would be overwritten by checking out the repository"+
              msg" here.")
        case e: NotUnique =>
          cli.abort(msg"The ${e.kind} ${e.item} already exists.")
        case CommitNotInRepo(commit) =>
          cli.abort(msg"The commit $commit does not exist in the repository.")
        case Unspecified(kind) =>
          cli.abort(msg"The $kind has not been specified.")
        case UnknownModule(moduleRef: ModuleRef) =>
          cli.abort(msg"The module reference $moduleRef could not be resolved.")
        case UnspecifiedMain(moduleId: ModuleId) =>
          cli.abort(msg"Main class not defined for module '${moduleId}'.")
        case GraalVMError(message: String) =>
          cli.abort(msg"Problem with GrallVM:'${message}'. Please double-check the PATH")
        case BuildServerError(cause: Throwable) =>
          val stack = rootCause(cause).getStackTrace.mkString("\n  at ")
          cli.abort(msg"Problem with the build server: '${cause.toString}'.${"\n  at "}$stack")
        case InvalidKind(expected: Kind) =>
          cli.abort(msg"The module must be of type '${expected}'.")
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
        case e: ModuleAlreadyExists =>
          cli.abort(msg"The module '${e.module}' already exists.")
        case e: ProjectAlreadyExists =>
          cli.abort(msg"The project '${e.project}' already exists.")
        case e: AlreadyInitialized =>
          cli.abort(msg"Fury is already initialized in this directory. Use ${Args.ForceArg: CliParam} to override.")
        case CyclesInDependencies(refs) =>
          cli.abort(msg"There are dependency cycles containing : [${refs.mkString}]")
        case UnspecifiedBinary(Nil) =>
          cli.abort(msg"Binary not found.")
        case UnspecifiedBinary(possibleBinaries) =>
          cli.abort(msg"Unable to identify target binary: ${"\n\t"}${possibleBinaries.mkString("\n\t")}")
        case HistoryMissing() =>
          cli.abort(msg"The history of changes is missing.")
        case HistoryCorrupt() =>
          cli.abort(msg"The history of changes is corrupt.")
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
            gLog   <- ~Log.log(Pid(0))
            _      <- ~gLog.fail(errorString)
            _      <- ~gLog.await()
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
