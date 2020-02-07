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

  def menu(aliases: List[Action])(implicit log: Log): Menu = Menu('main, "main menu", 'build)(List(
    Menu('about, msg"inspect resource usage, current tasks etc.", 'resources, needsLayer = false)(
      Action('resources, msg"display information about available CPUs and memory usage", AboutCli(_).resources, needsLayer = false),
      Action('tasks, msg"list the tasks that are being executed", AboutCli(_).tasks, needsLayer = false),
      Action('connections, msg"list open BSP connections", AboutCli(_).connections, needsLayer = false),
      Action('colors, msg"show the color settings of the terminal", AboutCli(_).colors, needsLayer = false)
    ),
    Menu('alias, msg"view and edit command aliases", 'list)(
      Action('add, msg"add a command alias to the layer", AliasCli(_).add),
      Action('remove, msg"remove a command alias from the layer", AliasCli(_).remove),
      Action('list, msg"list command aliases", AliasCli(_).list)
    ),
    Action('bsp, msg"start BSP server", Bsp.startServer, show = false),
    Menu('binary, msg"manage binary dependencies for the module", 'list, shortcut = 'b')(
      Action('add, msg"add a binary dependency to the module", BinaryCli(_).add, shortcut = 'a'),
      Action('remove, msg"remove a binary dependency from the module", BinaryCli(_).remove, shortcut = 'r'),
      Action('list, msg"list binary dependencies for the module", BinaryCli(_).list, shortcut = 'l')
    ),
    Menu('build, msg"perform build actions", 'run)(
      Action('classpath, msg"show a classpath for a module", BuildCli(_).classpath),
      Action('console, msg"launch the console for this build", BuildCli(_).console),
      Action('run, msg"compile a module", BuildCli(_).compile(None)),
      // FIXME: This should be retained only temporarily
      Action('save, msg"save a module", BuildCli(_).compile(None)),
      Action('describe, msg"describe the build for a module", BuildCli(_).describe),
      Action('install, msg"install an application locally", BuildCli(_).install)
    ),
    Menu('clean, msg"clean fury workspace", 'compiler)(
      Action('all, msg"clean all", CleanCli(_).cleanAll, needsLayer = false),
      Action('compiler, msg"clean the caches managed by the compiler", BuildCli(_).clean),
      Action('targets, msg"clean the compile targets definition files", CleanCli(_).cleanBloop, needsLayer = false),
      Action('classes, msg"clean compiled classes", CleanCli(_).cleanClasses, needsLayer = false),
      Action('logs, msg"clean logs", CleanCli(_).cleanLogs, needsLayer = false),
      Action('repositories, msg"clean repositories", CleanCli(_).cleanRepos, needsLayer = false),
      Action('sources, msg"clean checked out sources", CleanCli(_).cleanSources, needsLayer = false)
    ),
    Action('completion, msg"ZSH completions", Cli.asCompletion(menu(aliases)), show = false),
    Menu('config, msg"change system configuration options", 'set, needsLayer = false)(
      Action('set, msg"change a settings", ConfigCli(_).set, needsLayer = false),
      Action('auth, msg"authenticate using the distribution service", ConfigCli(_).auth, needsLayer = false)
    ),
    Menu('dependency, msg"manage dependencies for the module", 'list, shortcut = 'd')(
      Action('add, msg"add a dependency on another module", DependencyCli(_).add, shortcut = 'a'),
      Action('remove, msg"remove a dependency", DependencyCli(_).remove, shortcut = 'r'),
      Action('list, msg"list dependencies for the module", DependencyCli(_).list, shortcut = 'l')
    ),
    Menu('env, msg"manage application environment variables", 'list, shortcut = 'e')(
      Action('add, msg"add an environment variable", EnvCli(_).add, shortcut = 'a'),
      Action('remove, msg"remove an environment variable", EnvCli(_).remove, shortcut = 'r'),
      Action('list, msg"list environment variable", EnvCli(_).list, shortcut = 'l')
    ),
    Action('help, msg"help on using Fury", help, needsLayer = false),
    Menu('module, msg"view and edit modules", 'list, shortcut = 'm')(
      Action('add, msg"add a new module to the project", ModuleCli(_).add, shortcut = 'a'),
      Action('remove, msg"remove a module from the project", ModuleCli(_).remove, shortcut = 'r'),
      Action('list, msg"list modules for the project", ModuleCli(_).list, shortcut = 'l'),
      Action('select, msg"select the current module", ModuleCli(_).select, shortcut = 's'),
      Action('update, msg"update the module", ModuleCli(_).update, shortcut = 'u')
    ),
    Menu('option, msg"manage compiler options for the module", 'list)(
      Action('add, msg"add a compiler option to the module", OptionCli(_).add),
      Action('define, msg"introduce a new compiler option for dependents", OptionCli(_).define),
      Action('remove, msg"remove a compiler option from the module", OptionCli(_).remove),
      Action('undefine, msg"remove a compiler option definition", OptionCli(_).undefine),
      Action('list, msg"list compiler options for the module", OptionCli(_).list)
    ),
    Menu('permission, msg"manage application permissions", 'list)(
      Action('grant, msg"grant an application permissions on your system", PermissionCli(_).grant),
      Action('list, msg"list application permissions", PermissionCli(_).list),
      Action('obviate, msg"remove an application permission", PermissionCli(_).obviate),
      Action('require, msg"add an application permission", PermissionCli(_).require)
    ),
    Menu('project, msg"manage projects", 'list, shortcut = 'p')(
      Action('add, msg"add a new project to the schema", ProjectCli(_).add, shortcut = 'a'),
      Action('remove, msg"remove a project from the schema", ProjectCli(_).remove, shortcut = 'r'),
      Action('list, msg"list projects for the schema", ProjectCli(_).list, shortcut = 'l'),
      Action('select, msg"select the current project", ProjectCli(_).select, shortcut = 's'),
      Action('update, msg"update a project", ProjectCli(_).update, shortcut = 'u')
    ),
    Action('prompt, msg"show a context prompt", BuildCli(_).prompt, show = false),
    Menu('property, msg"manage application -D properties", 'list)(
      Action('add, msg"add a -D property", PropertyCli(_).add),
      Action('remove, msg"remove a -D property", PropertyCli(_).remove),
      Action('list, msg"list -D properties", PropertyCli(_).list)
    ),
    Menu('resource, msg"manage resources for the module", 'list)(
      Action('add, msg"add a resource directory to the module", FrontEnd(_).Resources.add, shortcut = 'a'),
      Action('remove, msg"remove a resource directory from the module", FrontEnd(_).Resources.remove),
      Action('list, msg"list resources for the module", FrontEnd(_).Resources.list, shortcut = 'l')
    ),
    Action('restart, msg"restart the Fury server", BuildCli(_).notImplemented, needsLayer = false),
    Menu('source, msg"manage sources for the module", 'list, shortcut = 's')(
      Action('add, msg"add a source directory to the module", SourceCli(_).add, shortcut = 'a'),
      Action('remove, msg"remove a source directory from the module", SourceCli(_).remove, shortcut = 'r'),
      Action('list, msg"list sources for the module", SourceCli(_).list, shortcut = 'l')
    ),
    
    Action('stop, msg"gracefully shut down the Fury server", ((_: Cli) => Lifecycle.shutdown()),
        needsLayer = false),
    
    Menu('repo, msg"manage source repositories for the schema", 'list, shortcut = 'r')(
      Action('add, msg"add a source repository to the schema", RepoCli(_).add, shortcut = 'a'),
      Action('update, msg"update a source repository", RepoCli(_).update, shortcut = 'u'),
      Action('remove, msg"remove a source repository from the schema", RepoCli(_).remove, shortcut = 'r'),
      Action('fork, msg"fork a managed repository locally", RepoCli(_).fork, shortcut = 'f'),
      Action('unfork, msg"restore a source repository to a managed checkout", RepoCli(_).unfork),
      Action('list, msg"list source repositories", RepoCli(_).list, shortcut = 'l'),
      Action('pull, msg"pull the latest remote version of the source repo", RepoCli(_).pull, shortcut = 'p')
    ),
    Action('upgrade, msg"upgrade to the latest version of Fury", BuildCli(_).upgrade, needsLayer = false),
    Action('undo, msg"undo the previous modification", LayerCli(_).undo),
    Menu('layer, msg"view and edit the layer", 'projects, shortcut = 'l', needsLayer = false)(
      Action('clone, msg"clone an external layer", LayerCli(_).cloneLayer, shortcut = 'c', needsLayer = false),
      Action('export, msg"export a layer to a file", LayerCli(_).export, shortcut = 'e'),
      Action('extract, msg"extract a layer file", LayerCli(_).extract, needsLayer = false),
      Action('import, msg"import an external layer", LayerCli(_).addImport, shortcut = 'i'),
      Action('init, msg"initialize a new Fury layer", LayerCli(_).init, needsLayer = false),
      Action('list, msg"list imported layers", LayerCli(_).list, shortcut = 'l'),
      Action('projects, msg"show all available projects", LayerCli(_).projects),
      Action('publish, msg"publish a layer", LayerCli(_).publish, shortcut = 'p'),
      Action('unimport, msg"remove a previously imported layer", LayerCli(_).unimport),
      Action('select, msg"select a layer", LayerCli(_).select, shortcut = 's'),
      Action('share, msg"share this layer", LayerCli(_).share),
    )
  ) ::: aliases: _*)

  def help(cli: Cli)(implicit log: Log): Try[ExitStatus] =
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
