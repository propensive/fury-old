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

import fury.core._, fury.strings._

import scala.util._

object FuryMenu {

  def menu(aliases: List[Action[Cli[CliParam[_]]]]): Menu[Cli[CliParam[_]], _] =
    Menu('main, "main menu", (x: Cli[CliParam[_]]) => Success(x), 'build)(List(
        Action('about, msg"about Fury", BuildCli.about),
        Menu('alias, msg"view and edit command aliases", AliasCli.context, 'list)(
            Action('add, msg"add a command alias to the layer", AliasCli.add),
            Action('remove, msg"remove a command alias from the layer", AliasCli.remove),
            Action('list, msg"list command aliases", AliasCli.list)
        ),
        Action('bsp, msg"start BSP server", Bsp.startServer, false),
        Menu('binary, msg"manage binary dependencies for the module", BinaryCli.context, 'list)(
            Action('add, msg"add a binary dependency to the module", BinaryCli.add),
            Action('remove, msg"remove a binary dependency from the module", BinaryCli.remove),
            Action('list, msg"list binary dependencies for the module", BinaryCli.list)
        ),
        Menu('build, msg"perform build actions", BuildCli.context, 'compile)(
            Action('classpath, msg"show a classpath for a module", BuildCli.classpath),
            Action('compile, msg"compile a module", BuildCli.compile(None, None)),
            Action('describe, msg"describe the build for a module", BuildCli.describe),
            Action('save, msg"save a JAR file", BuildCli.save),
            Action('native, msg"build a native executable using GraalVM", BuildCli.native)
        ),
        Menu('clean, msg"clean fury workspace", CleanCli.context, 'all)(
            Action('all, msg"clean all", CleanCli.cleanAll),
            Action('bloop, msg"clean bloop artifacts", CleanCli.cleanBloop),
            Action('classes, msg"clean compiled classes", CleanCli.cleanClasses),
            Action('repositories, msg"clean repositories", CleanCli.cleanRepos),
            Action('sources, msg"clean checked out sources", CleanCli.cleanSources)
        ),
        Action('completion, msg"ZSH completions", Cli.asCompletion(menu(aliases)), false),
        Menu('config, msg"change system configuration options", ConfigCli.context, 'set)(
            Action('set, msg"change a settings", ConfigCli.set)
        ),
        Menu('dependency, msg"manage dependencies for the module", DependencyCli.context, 'list)(
            Action('add, msg"add a dependency on another module", DependencyCli.add),
            Action('remove, msg"remove a dependency", DependencyCli.remove),
            Action('list, msg"list dependencies for the module", DependencyCli.list)
        ),
        Action('help, msg"help on using Fury", help),
        /*Menu('import, msg"manage imported schemas", ImportCli.context, 'list)(
            Action('add, msg"add an imported schema", ImportCli.add),
            Action('remove, msg"remove a previously imported schema", ImportCli.remove),
            Action('list, msg"list imported schemas", ImportCli.list)
        ),*/
        Menu('module, msg"view and edit modules", ModuleCli.context, 'list)(
            Action('add, msg"add a new module to the project", ModuleCli.add),
            Action('remove, msg"remove a module from the project", ModuleCli.remove),
            Action('list, msg"list modules for the project", ModuleCli.list),
            Action('select, msg"select the current module", ModuleCli.select),
            Action('update, msg"update the module", ModuleCli.update)
        ),
        Menu('param, msg"manage compiler parameters for the module", ParamCli.context, 'list)(
            Action('add, msg"add a compiler parameter to the module", ParamCli.add),
            Action('remove, msg"remove a compiler parameter from the module", ParamCli.remove),
            Action('list, msg"list compiler parameters for the module", ParamCli.list)
        ),
        Menu('project, msg"manage projects", ProjectCli.context, 'list)(
            Action('add, msg"add a new project to the schema", ProjectCli.add),
            Action('remove, msg"remove a project from the schema", ProjectCli.remove),
            Action('list, msg"list projects for the schema", ProjectCli.list),
            Action('select, msg"select the current project", ProjectCli.select),
            Action('update, msg"update a project", ProjectCli.update)
        ),
        Action('prompt, msg"show a context prompt", BuildCli.prompt, false),
        Action('restart, msg"restart the Fury server", BuildCli.notImplemented),
        Menu('source, msg"manage sources for the module", SourceCli.context, 'list)(
            Action('add, msg"add a source directory to the module", SourceCli.add),
            Action('remove, msg"remove a source directory from the module", SourceCli.remove),
            Action('list, msg"list sources for the module", SourceCli.list)
        ),
        Action('stop, msg"gracefully shut down the Fury server", Main.shutdown()),
        Action('kill, msg"kill the Fury server", BuildCli.notImplemented),
        Menu('schema, msg"manage the current schema", SchemaCli.context, 'list)(
            Action('add, msg"add a schema to the layer", SchemaCli.add),
            Action('remove, msg"remove a schema from the layer", SchemaCli.remove),
            Action('diff, msg"compare this schema to another one", SchemaCli.diff),
            Action('list, msg"list schemas for the layer", SchemaCli.list),
            Action('select, msg"select the current schema", SchemaCli.select),
            Action('update, msg"update a schema", SchemaCli.update)
        ),
        Menu('repo, msg"manage source repositories for the schema", RepoCli.context, 'list)(
            Action('add, msg"add a source repository to the schema", RepoCli.add),
            Action('update, msg"update a source repository", RepoCli.update),
            Action('remove, msg"remove a source repository from the schema", RepoCli.remove),
            Action('fork, msg"fork a managed repository locally", RepoCli.fork),
            Action('unfork, msg"restore a source repository to a managed checkout", RepoCli.unfork),
            Action('list, msg"list source repositories", RepoCli.list),
            Action(
                'pull,
                msg"pull the latest version of the source repo from the remote",
                RepoCli.pull)
        ),
        Action('undo, msg"undo the previous modification", BuildCli.undo),
        Menu('layer, msg"view and edit the layer", (t: Cli[CliParam[_]]) => Try(t), 'projects)(
            Action('init, msg"initialize a new Fury layer", LayerCli.init),
            Action('share, msg"share the current layer", LayerCli.share),
            Action('import, msg"import an external layer", LayerCli.doImport),
            Action('clone, msg"clone an external layer", LayerCli.doClone),
            Action('projects, msg"show all available projects", LayerCli.projects)
        )
    ) ::: aliases: _*)

  def help(cli: Cli[CliParam[_]]): Try[ExitStatus] =
    for {
      invoc <- cli.read()
      io    <- invoc.io()
      _     <- ~io.println(s"""|Usage: fury <command> [<subcommands>] [<args>]
                           |
                           |Command and subcommand reference:
                           |${menu(Nil).reference(Theme.NoColor).join("\n")}
                           |
                           |More help is available on the Fury website: https://fury.build/
                           |""".stripMargin)
    } yield io.await()
}
