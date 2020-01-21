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
package fury

import fury.core._, fury.strings._

import scala.util._

object FuryMenu {

  def menu(aliases: List[Action[Cli[CliParam[_]]]])(implicit log: Log): Menu[Cli[CliParam[_]], _] =
    Menu('main, "main menu", (x: Cli[CliParam[_]]) => Success(x), 'build)(List(
        Action('about, msg"about Fury", BuildCli.about),
        Menu('alias, msg"view and edit command aliases", AliasCli.context, 'list)(
            Action('add, msg"add a command alias to the layer", AliasCli.add),
            Action('remove, msg"remove a command alias from the layer", AliasCli.remove),
            Action('list, msg"list command aliases", AliasCli.list)
        ),
        Action('bsp, msg"start BSP server", Bsp.startServer, false),
        Menu('binary, msg"manage binary dependencies for the module", BinaryCli.context, 'list, shortcut = 'b')(
            Action('add, msg"add a binary dependency to the module", BinaryCli.add, shortcut = 'a'),
            Action('remove, msg"remove a binary dependency from the module", BinaryCli.remove, shortcut = 'r'),
            Action('list, msg"list binary dependencies for the module", BinaryCli.list, shortcut = 'l')
        ),
        Menu('build, msg"perform build actions", BuildCli.context, 'run)(
            Action('classpath, msg"show a classpath for a module", BuildCli.classpath),
            Action('console, msg"launch the console for this build", BuildCli.console),
            Action('run, msg"compile a module", BuildCli.compile(None)),
            // FIXME: This should be retained only temporarily
            Action('save, msg"save a module", BuildCli.compile(None)),
            Action('describe, msg"describe the build for a module", BuildCli.describe),
            Action('install, msg"install an application locally", BuildCli.install)
        ),
        Menu('clean, msg"clean fury workspace", CleanCli.context, 'all)(
            Action('all, msg"clean all", CleanCli.cleanAll),
            Action('bloop, msg"clean bloop artifacts", CleanCli.cleanBloop),
            Action('classes, msg"clean compiled classes", CleanCli.cleanClasses),
            Action('logs, msg"clean logs", CleanCli.cleanLogs),
            Action('repositories, msg"clean repositories", CleanCli.cleanRepos),
            Action('sources, msg"clean checked out sources", CleanCli.cleanSources)
        ),
        Action('completion, msg"ZSH completions", Cli.asCompletion(menu(aliases)), false),
        Menu('config, msg"change system configuration options", ConfigCli.context, 'set)(
            Action('set, msg"change a settings", ConfigCli.set),
            Action('auth, msg"authenticate using the distribution service", ConfigCli.auth)
        ),
        Menu('dependency, msg"manage dependencies for the module", DependencyCli.context, 'list, shortcut = 'd')(
            Action('add, msg"add a dependency on another module", DependencyCli.add, shortcut = 'a'),
            Action('remove, msg"remove a dependency", DependencyCli.remove, shortcut = 'r'),
            Action('list, msg"list dependencies for the module", DependencyCli.list, shortcut = 'l')
        ),
        Menu('env, msg"manage application environment variables", EnvCli.context, 'list, shortcut = 'e')(
            Action('add, msg"add an environment variable", EnvCli.add, shortcut = 'a'),
            Action('remove, msg"remove an environment variable", EnvCli.remove, shortcut = 'r'),
            Action('list, msg"list environment variable", EnvCli.list, shortcut = 'l')
        ),
        Action('help, msg"help on using Fury", help),
        Action('kill, msg"kill the Fury server", BuildCli.notImplemented),
        Menu('module, msg"view and edit modules", ModuleCli.context, 'list, shortcut = 'm')(
            Action('add, msg"add a new module to the project", ModuleCli.add, shortcut = 'a'),
            Action('remove, msg"remove a module from the project", ModuleCli.remove, shortcut = 'r'),
            Action('list, msg"list modules for the project", ModuleCli.list, shortcut = 'l'),
            Action('select, msg"select the current module", ModuleCli.select, shortcut = 's'),
            Action('update, msg"update the module", ModuleCli.update, shortcut = 'u')
        ),
        Menu('option, msg"manage compiler options for the module", OptionCli.context, 'list)(
            Action('add, msg"add a compiler option to the module", OptionCli.add),
            Action('define, msg"introduce a new compiler option for dependents", OptionCli.define),
            Action('remove, msg"remove a compiler option from the module", OptionCli.remove),
            Action('undefine, msg"remove a compiler option definition", OptionCli.undefine),
            Action('list, msg"list compiler options for the module", OptionCli.list)
        ),
        Menu('permission, msg"manage application permissions", PermissionCli.context, 'list)(
            Action('grant, msg"grant an application permissions on your system", PermissionCli.grant),
            Action('list, msg"list application permissions", PermissionCli.list),
            Action('obviate, msg"remove an application permission", PermissionCli.obviate),
            Action('require, msg"add an application permission", PermissionCli.require)
        ),
        Menu('project, msg"manage projects", ProjectCli.context, 'list, shortcut = 'p')(
            Action('add, msg"add a new project to the schema", ProjectCli.add, shortcut = 'a'),
            Action('remove, msg"remove a project from the schema", ProjectCli.remove, shortcut = 'r'),
            Action('list, msg"list projects for the schema", ProjectCli.list, shortcut = 'l'),
            Action('select, msg"select the current project", ProjectCli.select, shortcut = 's'),
            Action('update, msg"update a project", ProjectCli.update, shortcut = 'u')
        ),
        Action('prompt, msg"show a context prompt", BuildCli.prompt, false),
        Menu('property, msg"manage application -D properties", PropertyCli.context, 'list)(
            Action('add, msg"add a -D property", PropertyCli.add),
            Action('remove, msg"remove a -D property", PropertyCli.remove),
            Action('list, msg"list -D properties", PropertyCli.list)
        ),
        Menu('resource, msg"manage resources for the module", ResourceCli.context, 'list, shortcut = 's')(
            Action('add, msg"add a resource directory to the module", ResourceCli.add, shortcut = 'a'),
            Action('remove, msg"remove a resource directory from the module", ResourceCli.remove,
                shortcut = 'r'),
            Action('list, msg"list resources for the module", ResourceCli.list, shortcut = 'l')
        ),
        Action('restart, msg"restart the Fury server", BuildCli.notImplemented),
        Menu('source, msg"manage sources for the module", SourceCli.context, 'list, shortcut = 's')(
            Action('add, msg"add a source directory to the module", SourceCli.add, shortcut = 'a'),
            Action('remove, msg"remove a source directory from the module", SourceCli.remove, shortcut = 'r'),
            Action('list, msg"list sources for the module", SourceCli.list, shortcut = 'l')
        ),
        Action('stop, msg"gracefully shut down the Fury server", ((_: Cli[CliParam[_]]) => Lifecycle.shutdown())),
        Menu('repo, msg"manage source repositories for the schema", RepoCli.context, 'list, shortcut = 'r')(
            Action('add, msg"add a source repository to the schema", RepoCli.add, shortcut = 'a'),
            Action('update, msg"update a source repository", RepoCli.update, shortcut = 'u'),
            Action('remove, msg"remove a source repository from the schema", RepoCli.remove, shortcut = 'r'),
            Action('fork, msg"fork a managed repository locally", RepoCli.fork, shortcut = 'f'),
            Action('unfork, msg"restore a source repository to a managed checkout", RepoCli.unfork),
            Action('list, msg"list source repositories", RepoCli.list, shortcut = 'l'),
            Action(
                'pull,
                msg"pull the latest version of the source repo from the remote",
                RepoCli.pull, shortcut = 'p')
        ),
        Action('upgrade, msg"upgrade to the latest version of Fury", BuildCli.upgrade),
        //Action('undo, msg"undo the previous modification", BuildCli.undo),
        Menu('layer, msg"view and edit the layer", (t: Cli[CliParam[_]]) => Try(t), 'projects, shortcut = 'l')(
            Action('clone, msg"clone an external layer", LayerCli.clone, shortcut = 'c'),
            Action('export, msg"export a layer to a file", LayerCli.export, shortcut = 'e'),
            Action('extract, msg"extract a layer file", LayerCli.extract),
            Action('import, msg"import an external layer", LayerCli.addImport, shortcut = 'i'),
            Action('init, msg"initialize a new Fury layer", LayerCli.init),
            Action('list, msg"list imported layers", LayerCli.list, shortcut = 'l'),
            Action('projects, msg"show all available projects", LayerCli.projects),
            Action('publish, msg"publish a layer", LayerCli.publish, shortcut = 'p'),
            Action('unimport, msg"remove a previously imported layer", LayerCli.unimport),
            Action('select, msg"select a layer", LayerCli.select, shortcut = 's'),
            Action('share, msg"share this layer", LayerCli.share),
        )
    ) ::: aliases: _*)

  def help(cli: Cli[CliParam[_]])(implicit log: Log): Try[ExitStatus] =
    for {
      call  <- cli.call()
      _     <- ~log.raw(s"""|Usage: fury <command> [<subcommands>] [<args>]
                           |
                           |Command and subcommand reference:
                           |${menu(Nil).reference(ManagedConfig().theme).join("\n")}
                           |
                           |More help is available on the Fury website: https://fury.build/
                           |""".stripMargin)
    } yield log.await()
}
