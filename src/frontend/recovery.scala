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
package fury

import fury.strings._, fury.core._, fury.io._, fury.model._

import exoskeleton._
import guillotine._

import scala.util._

object Recovery {

  def recover(cli: Cli[CliParam[_]])(result: Try[ExitStatus]): ExitStatus = result match {
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
        case e: SchemaDifferences =>
          cli.abort(
              msg"""You are attempting to make this change to all schemas, however the value you are
trying to change is different in different schemas. To make the change to just
one schema, please specify the schema with --schema/-s, or if you are sure you
want to make this change to all schemas, please add the --force/-F argument.""")
        case InitFailure() =>
          cli.abort(msg"Could not start the bloop server.")
        case FileWriteError(path) =>
          cli.abort(msg"Couldn't write to file $path.")
        case FileNotFound(path) =>
          cli.abort(
              msg"""Could not find the file $path. Run `fury layer init` to create a new layer.""")
        case MissingArg(param) =>
          cli.abort(msg"The parameter $param was not provided.")
        case NoPermissions(perms) =>
          val prefixLength = Compare.uniquePrefixLength(perms.map(_.hash)).max(3)
          val rows = perms.map { p => PermissionEntry(p, PermissionHash(p.hash.take(prefixLength))) }.to[List]
          
          val permissions = Tables(cli.config).show(Tables(cli.config).permissions, cli.cols, rows,
              false)(identity(_)).mkString("\n")

          cli.abort(msg"""The following permissions are required to run the build:
${permissions}
You can grant these permissions with,

  fury permission grant -P <permission hash>
""")
        case NoPreviousRevision =>
          cli.abort(msg"No earlier revision can be found")
        case LauncherFailure(msg) =>
          cli.abort(msg"Bloop did not start successfully: $msg")
        case DownloadFailure(msg) =>
          cli.abort(msg"Coursier could not complete a download: $msg")
        case e: MissingCommand =>
          cli.abort(msg"No command was provided.")
        case e: UnknownCommand =>
          cli.abort(msg"The command '${e.command}' was not recognized.")
        case InvalidArgValue(param, arg) =>
          cli.abort(msg"The argument '$arg' was not a valid value for the parameter '$param'.")
        case e: ConfigFormatError =>
          cli.abort(msg"The configuration file could not be read.")
        case e: InvalidValue =>
          cli.abort(msg"'${e.value}' is not a valid value.")
        case e: ItemNotFound =>
          cli.abort(msg"The ${e.kind} ${e.item} was not found.")
        case e: UnspecifiedProject =>
          cli.abort(msg"The project has not been specified.")
        case e: UnspecifiedRepo =>
          cli.abort(msg"The repository has not been specified.")
        case e: UnspecifiedModule =>
          cli.abort(msg"The module has not been specified.")
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
        case e: ShellFailure =>
          cli.abort(
              msg"An error occurred while running: ${e.command}${"\n\n"}${e.stdout}${"\n"}${e.stderr}")
        case e: CompilationFailure =>
          cli.abort(msg"One of the compile or run tasks failed. Check the logs for details.")
        case e: ModuleAlreadyExists =>
          cli.abort(msg"The module '${e.module}' already exists.")
        case e: ProjectAlreadyExists =>
          cli.abort(msg"The project '${e.project}' already exists.")
        case e: AlreadyInitialized =>
          cli.abort(msg"Fury is already initialized in this directory. Use --force to override.")
        case CyclesInDependencies(refs) =>
          cli.abort(msg"There are dependency cycles containing : [${refs.mkString}]")
        case e =>
          val errorString =
            s"$e\n${rootCause(e).getStackTrace.to[List].map(_.toString).join("    at ", "\n    at ", "")}"
          val result = for {
            layout <- cli.layout
            invoc  <- cli.read()
            io     <- invoc.io()
            _      <- ~layout.errorLogfile.mkParents
            _      <- ~layout.errorLogfile.writeSync(errorString)
            _      <- ~io.await()
          } yield
            cli.abort(msg"An unexpected error occurred which has been logged to ${layout.errorLogfile}")

          def unloggable = cli.abort("An unexpected error occurred which could not be logged to disk.\n\n"+
              errorString) 

          result.recover {
            case e: FileWriteError   => unloggable
            case e: FileNotFound     => unloggable
            case e: EarlyCompletions => cli.abort("")
          }.toOption.get
      }
  }

  private def rootCause(t: Throwable) = {
    Stream.iterate(t)(_.getCause).takeWhile(_ != null).last
  }
}
