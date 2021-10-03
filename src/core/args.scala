/*

    Fury, version 0.36.0. Copyright 2018-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

import fury.text._, fury.io._, fury.model._

import exoskeleton.Param.{Extractor => TExtractor}
import exoskeleton._
import jovian._

object Args {

  implicit def extractor[T: Parser]: TExtractor[T] = _.headOption.flatMap(implicitly[Parser[T]].parse(_))

  implicit private val stringList: TExtractor[List[String]] = x => Some(x.to[List])

  val AllArg = CliParam[Unit]('a', 'all, "update all repositories")

  val BinaryRepoArg = CliParam[BinRepoId]('r', 'repo, "specify the repository from which to fetch binaries")
  val AliasArg = CliParam[AliasCmd]('a', 'alias, "specify a command alias")
  val ActionArg = CliParam[String]('A', 'action, "specify a permission action")
  val BinaryArg = CliParam[BinaryId]('b', 'binary, "specify a binary")
  val BareArg = CliParam[Unit]('b', 'bare, "do not import a default layer when initializing the new layer")
  val ColumnArg = CliParam[String]('c', 'column, "specify the column")
  val PartialBinSpecArg = CliParam[PartialBinSpec]('b', 'binary, "specify a binary")
  val BinaryNameArg = CliParam[BinaryId]('n', 'name, "specify the name for the binary")
  val BinSpecArg = CliParam[BinSpec]('b', 'binary, "specify a binary dependency")
  val CompilerArg = CliParam[CompilerRef]('c', 'compiler, "specify a compiler")
  val ClassArg = CliParam[ClassRef]('C', 'class, "specify a class name")
  
  val IgnoreErrorsArg = CliParam[Unit]('I', Symbol("ignore-errors"),
      "ignore compilation errors and launch the console anyway")

  val CommitArg = CliParam[Commit]('c', 'commit, "the Git commit hash")
  val RepoSetArg = CliParam[RepoSetId]('r', 'repo, "the commit hash of the collection of repos")

  val GrabArg = CliParam[Unit]('C', 'commandeer,
      "have Fury commandeer the Git repo in the working directory")

  val GithubActionsArg = CliParam[Unit]('G', Symbol("github-actions"),
      "set up continuous integration with Github Actions")

  val GitArg = CliParam[Unit]('g', Symbol("setup-git"), "set the layer up as a Git repository")
  val NoSecurityArg = CliParam[Unit]('S', Symbol("disable-security-manager"), "disable the security manager")
  val NoIpfsArg = CliParam[Boolean]('I', Symbol("no-ipfs"), "use HTTP gateways instead of the IPFS daemon")
  val DefaultCompilerArg = CliParam[CompilerRef]('c', 'compiler, "specify a default compiler")
  val DefaultImportArg = CliParam[LayerName]('l', 'layer, "specify a default external layer to import")
  val DocsArg = CliParam[Unit]('D', 'docs, "clone the layer into the user's default documents directory")
  val LinkArg = CliParam[String]('d', 'dependency, "specify a dependency to another module")
  val SourceArg = CliParam[Source]('s', 'source, "specify a source directory")
  val IncludeTypeArg = CliParam[IncludeType.Id]('t', 'type, "specify the type of include")
  val IncludeArg = CliParam[IncludeId]('i', 'include, "specify the include")
  val ResourceArg = CliParam[Source]('s', 'resource, "specify a resource file or directory")
  val PathArg = CliParam[Path]('f', 'path, "specify the path")
  val PathStringArg = CliParam[String]('f', 'path, "specify the path")
  val DebugArg = CliParam[String]('D', 'debug, "specify a module to debug")
  val DescriptionArg = CliParam[String]('D', 'description, "specify a brief description of the project")
  val OptDescriptionArg = CliParam[String]('D', 'description, "specify a brief description of the option")
  val EditorArg = CliParam[Unit]('e', 'edit, "open the layer in your editor")
  val EnvArg = CliParam[EnvVar]('e', 'env, "specify the environment variable in the form KEY=value")
  val ForceArg = CliParam[Unit]('F', 'force, "force this operation")
  val PersistentArg = CliParam[Unit]('P', 'persistent, "this option change should apply to all dependants")
  val PublicArg = CliParam[Unit]('P', 'public, "this layer should be published publicly")
  val BreakingArg = CliParam[Unit]('B', 'breaking, "this build contains breaking changes")
  val FatJarArg = CliParam[Unit]('F', Symbol("fat-jar"), "package the module along with all its dependencies")
  val JsArg = CliParam[Unit]('e', 'js, "use Scala.JS to output an JavaScript file")
  val HttpsArg = CliParam[Unit]('H', 'https, "use HTTPS to resolve repo aliases instead of SSH")
  val HiddenArg = CliParam[Boolean]('h', 'hidden, "hide this module")

  val IntransitiveArg = CliParam[Unit]('I', 'intransitive,
      "specify if this dependency should not be included transitively")

  val IgnoreArg = CliParam[Unit]('I', 'ignore, "ignore layer errors")
  val KeyArg = CliParam[String]('k', 'key, "GPG key")
  val ImportArg = CliParam[LayerName]('l', 'layer, "specify an external layer to import")
  val ImportIdArg = CliParam[ImportId]('i', 'import, "specify an import by ID")
  val LayerArg = CliParam[Pointer]('l', 'layer, "specify the layer")
  val LayerRefArg = CliParam[ShortLayerRef]('i', 'import, "specify the import by its hash")
  val LicenseArg = CliParam[LicenseId]('L', 'license, "license for code in this project")
  val ModuleArg = CliParam[ModuleId]('m', 'module, "specify a module")
  val MainArg = CliParam[ClassRef]('M', 'main, "specify a main class")
  
  val PipeliningArg = CliParam[Boolean]('P', Symbol("pipelining"),
      "use compilation pipelining (on, off) (experimental)")
  
  val PluginArg = CliParam[PluginId]('P', 'plugin, "specify the name of the plugin")
  val ProjectNameArg = CliParam[ProjectId]('n', 'name, "specify a name for the project")
  val RemoteLayerArg = CliParam[RemoteLayerId]('n', 'name, "specify a name at which to publish the layer")
  val RepoNameArg = CliParam[RepoId]('n', 'name, "specify a name for the repository")
  val WorkspaceNameArg = CliParam[WorkspaceId]('n', 'name, "specify a name for the workspace")
  val ExecNameArg = CliParam[ExecName]('n', 'name, "specify a name for the executable")
  val ImportNameArg = CliParam[ImportId]('n', 'name, "specify a name for the import")
  val RawArg = CliParam[Unit]('R', 'raw, "display raw output")
  val LayerVersionArg = CliParam[LayerVersion]('v', 'version, "a specific version of the layer")
  val ModuleNameArg = CliParam[ModuleId]('n', 'name, "specify a name for the module")
  val ModuleRefArg = CliParam[String]('d', 'dependency, "specify the dependency to include from")

  val SpecArg = CliParam[BloopSpec]('C', Symbol("compiler-spec"),
      "specify a specification for the compiler in the form <organization>:<compiler ID>:<version>")

  val ProjectArg = CliParam[ProjectId]('p', 'project, "specify a project")
  val ProjectArg2 = CliParam[ProjectId]('P', 'project, "specify a project")
  val ProjectRefArg = CliParam[ProjectRef]('p', 'project, "specify a project")
  val AgainstProjectArg = CliParam[ProjectRef]('a', 'against, "specify a project to compare against")
  val OptArg = CliParam[OptId]('o', 'option, "compiler option")
  val PermissionArg = CliParam[List[String]]('P', 'permission, "permission entries")
  val PropArg = CliParam[JavaProperty]('D', 'property, "Java -D property")
  val QuietArg = CliParam[Unit]('q', 'quiet, "show less output")
  val RepoArg = CliParam[RepoId]('r', 'repo, "specify a repository")
  val WorkspaceArg = CliParam[WorkspaceId]('w', 'workspace, "specify a workspace")
  val RecursiveArg = CliParam[Unit]('r', 'recursive, "perform the operation recursively")
  val RetryArg = CliParam[Unit]('R', 'retry, "reattempt to download a remote repository")
  val ReplArg = CliParam[ClassRef]('R', 'repl, "REPL class to use for console")
  
  val TimeoutArg = CliParam[Int]('T', 'timeout,
      "stop application if it is still running after this time (seconds)")
  
  val ExpiryArg = CliParam[Int]('T', 'ttl, "the TTL for the layer in days")
  private val allReporters = Reporter.all.map(_.name).mkString(", ")
  val ReporterArg = CliParam[Reporter]('o', 'output, s"format for build output ($allReporters)")
  val ScopeArg = CliParam[ScopeId]('S', 'scope, "specify the permission scope (layer, directory, project)")
  val ServiceArg = CliParam[DomainName]('S', 'service, "specify the default remote layer service")
  val ScratchArg = CliParam[Unit]('s', 'scratch, "create the repo as a scratch space")
  val TargetArg = CliParam[String]('T', 'target, "target file/directory")
  val TransformArg = CliParam[Unit]('t', 'transform, "transform the option into the parameters following --")
  val TraceArg = CliParam[Boolean]('D', Symbol("bsp-trace"), "Write the BSP communication trace to a file")
  val PermissionTargetArg = CliParam[String]('T', 'target, "permission target")
  val NoGrantArg = CliParam[Unit]('0', Symbol("no-grant"), "do not grant the permission automatically")
  val TagArg = CliParam[Tag]('t', 'tag, "git tag")
  val BranchArg = CliParam[Branch]('B', 'branch, "git branch")
  val ThemeArg = CliParam[Theme]('T', 'theme, "specify a color theme")
  val TimestampsArg = CliParam[Boolean]('L', 'timestamps, "show timestamps (on, off)")
  val UrlArg = CliParam[String]('u', 'url, "specify a URL")
  val RemoteArg = CliParam[Remote]('g', 'remote, "specify a Git remote URL")
  val UnparsedRemoteArg = CliParam[String]('r', 'url, "specify a remote URL")
  val KindArg = CliParam[Kind.Id]('t', 'type, "Type of module (lib, app, plugin, compiler, bench)")
  val VerboseArg = CliParam[Unit]('v', 'verbose, "Show more output")
  val VersionArg = CliParam[Version]('v', 'version, "The published version of the binary")
  val WaitArg = CliParam[Unit]('W', 'wait, "Wait for the first successful compilation")
  val WatchArg = CliParam[Unit]('w', 'watch, "Watch for file changes")
  val SingleColumnArg = CliParam[Unit]('1', Symbol("single-column"), "Print values in a single column")
  val ParamNoArg = Param[Int](' ', 'paramNo)
}
