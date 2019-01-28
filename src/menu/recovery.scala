/*
  Fury, version 0.4.0. Copyright 2018-19 Jon Pretty, Propensive Ltd.

  The primary distribution site is: https://propensive.com/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required  by applicable  law or  agreed to  in writing,  software  distributed  under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
  express  or  implied.  See  the  License for  the specific  language  governing  permissions and
  limitations under the License.
 */
package fury

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
        case ProjectConflict(ps) =>
          cli.abort(
              msg"""Your dependency tree contains references to two or more conflicting projects: ${ps
                .mkString(", ")}""")
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
        case NoPreviousRevision =>
          cli.abort(msg"No earlier revision can be found")
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
        case UnspecifiedMain(moduleId: ModuleId) =>
          cli.abort(msg"Main class not defined for module '${moduleId}'.")
        case GraalVMError(message: String) =>
          cli.abort(msg"Problem with GrallVM:'${message}'. Please double-check the PATH")
        case InvalidKind(expected: Kind) =>
          cli.abort(msg"The module must be of type '${expected}'.")
        case e: UnknownCompiler =>
          cli.abort(msg"This compiler is not known.")
        case e: ShellFailure =>
          cli.abort(
              msg"An error occurred while running: ${e.command}${"\n\n"}${e.stdout}${"\n"}${e.stderr}")
        case e: ModuleAlreadyExists =>
          cli.abort(msg"The module '${e.module}' already exists.")
        case e: ProjectAlreadyExists =>
          cli.abort(msg"The project '${e.project}' already exists.")
        case e: AlreadyInitialized =>
          cli.abort(msg"Fury is already initialized in this directory. Use --force to override.")
        case CyclesInDependencies(cycle) =>
          cli.abort(msg"Cycle in dependencies : [${cycle.mkString(" -> ")}]")
        case e =>
          val errorString =
            s"$e\n${e.getStackTrace.to[List].map(_.toString).join("    at ", "\n    at ", "")}"
          val result = for {
            layout <- cli.layout
            invoc  <- cli.read()
            io     <- invoc.io()
            _      <- ~layout.errorLogfile.mkParents
            _      <- ~layout.errorLogfile.writeSync(errorString)
            _      <- ~io.await()
          } yield
            cli.abort {
              msg"An unexpected error occurred which has been logged to ${layout.errorLogfile}."
            }

          def unloggable =
            cli.abort(
                "An unexpected error occurred which could not " +
                  s"be logged to disk.\n\n$errorString")

          result.recover {
            case e: FileWriteError   => unloggable
            case e: FileNotFound     => unloggable
            case e: EarlyCompletions => cli.abort("")
          }.toOption.get
      }
  }
}
