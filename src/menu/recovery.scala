/*
  Fury, version 0.1.2. Copyright 2018 Jon Pretty, Propensive Ltd.

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

import mitigation._
import exoskeleton._
import guillotine._

object Recovery {

  def recover(cli: Cli[CliParam[_]])
             (result: Result[ExitStatus, ~ | MissingCommand | FileWriteError |
                 FileNotFound | MissingArg | UnknownCommand | UnknownCompiler | UnspecifiedRepo |
                 ItemNotFound | UnspecifiedProject | UnspecifiedModule | InvalidArgValue |
                 ConfigFormatError | ShellFailure | ModuleAlreadyExists | ProjectAlreadyExists |
                 AlreadyInitialized | InvalidValue | InitFailure | SchemaDifferences |
                 EarlyCompletions | ProjectConflict])
             : ExitStatus = result.recover(
    on[EarlyCompletions].map { case EarlyCompletions() =>
      Done
    },
    on[ProjectConflict].map { case ProjectConflict(ps) =>
      cli.abort(msg"""Your dependency tree contains references to two or more conflicting projects: ${ps.mkString(", ")}""")
    },
    on[SchemaDifferences].map { e =>
      cli.abort(msg"""You are attempting to make this change to all schemas, however the value you are
trying to change is different in different schemas. To make the change to just
one schema, please specify the schema with --schema/-s, or if you are sure you
want to make this change to all schemas, please add the --force/-F argument.""")
    },
    on[InitFailure].map { case InitFailure() =>
      cli.abort(msg"Could not start the bloop server.")
    },
    on[FileWriteError].map { case FileWriteError(path) =>
      cli.abort(msg"Couldn't write to file $path.")
    },
    on[FileNotFound].map { case FileNotFound(path) =>
      cli.abort(msg"""Could not find the file $path. Run `fury init` to create a new layer.""")
    },
    on[MissingArg].map { case MissingArg(param) =>
      cli.abort(msg"The parameter --$param was not provided.")
    },
    on[MissingCommand].map { e =>
      cli.abort(msg"No command was provided.")
    },
    on[UnknownCommand].map { e =>
      cli.abort(msg"The command '${e.command}' was not recognized.")
    },
    on[InvalidArgValue].map { case InvalidArgValue(param, arg) =>
      cli.abort(msg"The argument '$arg' was not a valid value for the parameter '$param'.")
    },
    on[ConfigFormatError].map { err =>
      cli.abort(msg"The configuration file could not be read.")
    },
    on[InvalidValue].map { e =>
      cli.abort(msg"'${e.value}' is not a valid value.")
    },
    on[ItemNotFound].map { e =>
      cli.abort(msg"The ${e.kind} ${e.item} was not found.")
    },
    on[UnspecifiedProject].map { e =>
      cli.abort(msg"The project has not been specified.")
    },
    on[UnspecifiedRepo].map { e =>
      cli.abort(msg"The repository has not been specified.")
    },
    on[UnspecifiedModule].map { e =>
      cli.abort(msg"The module has not been specified.")
    },
    on[UnknownCompiler].map { e =>
      cli.abort(msg"This compiler is not known.")
    },
    on[ShellFailure].map { e =>
      cli.abort(msg"An error occurred while running: ${e.command}${"\n\n"}${e.stdout}${"\n"}${e.stderr}")
    },
    on[ModuleAlreadyExists].map { e =>
      cli.abort(msg"The module '${e.module}' already exists.")
    },
    on[ProjectAlreadyExists].map { e =>
      cli.abort(msg"The project '${e.project}' already exists.")
    },
    on[AlreadyInitialized].map { e =>
      cli.abort(msg"Fury is already initialized in this directory. Use --force to override.")
    },
    otherwise.map { case Surprise(e) =>
      val errorString = s"$e\n${e.getStackTrace.to[List].map(_.toString).join("    at ", "\n    at ", "")}"
      val result = for {
        layout <- cli.layout
        io     <- cli.io()
        _      <- ~layout.errorLogfile.writeSync(errorString)
        _      <- ~io.await()
      } yield cli.abort {
        msg"An unexpected error occurred which has been logged to ${layout.errorLogfile}."
      }
     
      def unloggable = cli.abort("An unexpected error occurred which could not "+
              s"be logged to disk.\n\n$errorString")

      result.recover(
        on[FileWriteError].use(unloggable), on[FileNotFound].use(unloggable),
        on[EarlyCompletions].use(cli.abort(""))
      )
    }
  )
}
