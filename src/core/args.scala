/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.7.9. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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
package fury.core

import fury.strings._, fury.io._, fury.model._

import exoskeleton.Param.{Extractor => TExtractor}
import exoskeleton._

object Args {
  implicit private val schemaId: TExtractor[SchemaId] = _.headOption.flatMap(SchemaId.parse(_).toOption)
  implicit private val importLayer: TExtractor[ImportLayer] = _.headOption.flatMap(ImportLayer.parse(_))
  implicit private val aliasCmd: TExtractor[AliasCmd] = _.headOption.map(AliasCmd(_))
  implicit private val parameter: TExtractor[Parameter] = _.headOption.map(Parameter(_))
  implicit private val envVar: TExtractor[EnvVar] = _.headOption.flatMap(EnvVar.parse(_))
  implicit private val javaProp: TExtractor[JavaProperty] = _.headOption.flatMap(JavaProperty.parse(_))
  implicit private val licenseId: TExtractor[LicenseId] = _.headOption.map(LicenseId(_))
  implicit private val repoId: TExtractor[RepoId] = _.headOption.map(RepoId(_))
  implicit private val binRepoId: TExtractor[BinRepoId] = _.headOption.map(BinRepoId(_))
  implicit private val moduleId: TExtractor[ModuleId] = _.headOption.flatMap(ModuleId.parse(_).toOption)
  implicit private val projectId: TExtractor[ProjectId] = _.headOption.flatMap(ProjectId.parse(_).toOption)
  implicit private val importId: TExtractor[ImportId] = _.headOption.flatMap(ImportId.parse(_).toOption)
  implicit private val path: TExtractor[Path] = _.headOption.flatMap(Path.unapply(_))
  implicit private val kindKey: TExtractor[Kind] = _.headOption.flatMap(Kind.unapply(_))
  implicit private val version: TExtractor[RefSpec] = _.headOption.map(RefSpec(_))
  implicit private val theme: TExtractor[Theme] = _.headOption.flatMap(Theme.unapply(_))
  implicit private val ipfsRef: TExtractor[IpfsRef] = _.headOption.flatMap(IpfsRef.parse(_))
  implicit private val importPath: TExtractor[ImportPath] = _.headOption.flatMap(ImportPath.parse(_))
  implicit private val reporter: TExtractor[Reporter] = _.headOption.flatMap(Reporter.unapply(_))
  implicit private val scopeId: TExtractor[ScopeId] = _.headOption.flatMap(ScopeId.unapply(_))

  implicit private val boolean: TExtractor[Boolean] = _.headOption map {
    case "on" | "true" | "1" => true
    case "off" | "false" | "0" => false
  }
  implicit private val stringList: TExtractor[List[String]] = x => Some(x.to[List])

  val AllArg = CliParam[Unit]('a', 'all, "update all repositories")

  val BinaryRepoArg = CliParam[BinRepoId]('r', 'repo, "Specify the repository from which to fetch binaries")
  val AliasArg = CliParam[AliasCmd]('a', 'alias, "specify a command alias")
  val ActionArg = CliParam[String]('A', 'action, "specify a permission action")
  val BinaryArg = CliParam[String]('b', 'binary, "specify a binary dependency")
  val CloneRefArg = CliParam[IpfsRef]('l', 'layer, "layer reference")
  val CompilerArg = CliParam[String]('c', 'compiler, "specify a compiler")
  val ClassArg = CliParam[String]('C', 'class, "specify a class name")
  val DefaultCompilerArg = CliParam[String]('c', 'compiler, "specify a default compiler")
  val LinkArg = CliParam[String]('l', 'link, "specify a dependency link to another module")
  val SourceArg = CliParam[String]('d', 'source, "specify a source directory")
  val DirArg = CliParam[Path]('d', 'dir, "specify the new repository destination directory")
  val DebugArg = CliParam[String]('D', 'debug, "specify a module to debug")
  val DescriptionArg = CliParam[String]('D', 'description, "specify a brief description of the project")
  val EnvArg = CliParam[EnvVar]('e', 'env, "specify the environment variable in the form KEY=value")
  val ForceArg = CliParam[Unit]('F', 'force, "force this operation")
  val FatJarArg = CliParam[Unit]('F', Symbol("fat-jar"), "package the module along with all its dependencies")
  val FileArg = CliParam[Path]('f', 'file, "destination file")
  val HttpsArg = CliParam[Unit]('H', 'https, "use HTTPS to resolve repository aliases instead of SSH")
  val HiddenArg = CliParam[Boolean]('h', 'hidden, "hide this module")
  val ImportArg = CliParam[ImportLayer]('l', Symbol("layer"), "specify an external layer to import")
  val ImportIdArg = CliParam[ImportId]('l', Symbol("layer"), "specify a layer to unimport")
  val ImportSchemaArg = CliParam[SchemaId]('i', Symbol("import"), "specify the external schema to import")

  val IntransitiveArg = CliParam[Unit]('I', 'intransitive,
      "specify if this dependency should not be included transitively")

  val KeyArg = CliParam[String]('k', 'key, "GPG key")
  val LayerArg = CliParam[ImportPath]('l', 'layer, "specify the layer")
  val LicenseArg = CliParam[LicenseId]('L', 'license, "license for code in this project")
  val ModuleArg = CliParam[ModuleId]('m', 'module, "specify a module")
  val MainArg = CliParam[String]('M', 'main, "specify a main class")
  val PipeliningArg = CliParam[Boolean]('P', Symbol("pipelining"), "use compilation pipelining (on, off) (experimental)")
  val PluginArg = CliParam[String]('P', 'plugin, "specify the name of the plugin")
  val ProjectNameArg = CliParam[ProjectId]('n', 'name, "specify a name for the project")
  val RepoNameArg = CliParam[RepoId]('n', 'name, "specify a name for the repository")
  val SchemaNameArg = CliParam[SchemaId]('n', 'name, "specify a name for the schema")
  val ImportNameArg = CliParam[ImportId]('n', 'name, "specify a name for the import")
  val RawArg = CliParam[Unit]('R', 'raw, "display raw output")
  val ModuleNameArg = CliParam[ModuleId]('n', 'name, "specify a name for the module")

  val BloopSpecArg = CliParam[String]('C', Symbol("compiler-spec"),
      "specify a specification for the compiler in the form <organization>:<compiler ID>:<version>")

  val ProjectArg = CliParam[ProjectId]('p', 'project, "specify a project")
  val ParamArg = CliParam[Parameter]('P', 'param, "compiler parameter")
  val PermissionArg = CliParam[List[String]]('P', 'permission, "permission entries")
  val PropArg = CliParam[JavaProperty]('D', 'param, "Java -D property")
  val QuietArg = CliParam[Unit]('q', 'quiet, "show less output")
  val RepoArg = CliParam[RepoId]('r', 'repo, "specify a repository")
  val RecursiveArg = CliParam[Unit]('r', 'recursive, "perform the operation recursively")
  val RetryArg = CliParam[Unit]('R', 'retry, "reattempt to download a remote repository")
  
  private val allReporters = Reporter.all.map(_.name).mkString(", ")
  val ReporterArg = CliParam[Reporter]('o', 'output, s"format for build output ($allReporters)")
  val SchemaArg = CliParam[SchemaId]('s', 'schema, "specify a schema")
  val ScopeArg = CliParam[ScopeId]('S', 'scope, "specify the permission scope (layer, directory, project)")
  val ServiceArg = CliParam[String]('S', 'service, "specify the default remote layer service")
  val TargetArg = CliParam[String]('T', 'target, "target file/directory")
  val PermissionTargetArg = CliParam[String]('T', 'target, "permission target")
  val NoGrantArg = CliParam[String]('0', Symbol("no-grant"), "do not grant the permission automatically")
  val TagArg = CliParam[String]('t', 'tag, "git tag")
  val ThemeArg = CliParam[Theme]('T', 'theme, "specify a color theme")
  val TimestampsArg = CliParam[Boolean]('L', 'timestamps, "show timestamps (on, off)")
  val UrlArg = CliParam[String]('u', 'url, "specify a URL")
  val CompareArg = CliParam[SchemaId]('w', Symbol("with"), "specify a schema to compare with")

  val KindArg = CliParam[Kind]('t', 'type,
      "Type of module (library, application, plugin, compiler, benchmarks)")

  val VerboseArg = CliParam[Unit]('v', 'verbose, "Show more output")
  val VersionArg = CliParam[RefSpec]('V', 'version, "Git branch, tag or commit")
  val WatchArg = CliParam[Unit]('w', 'watch, "Watch for file changes")
  val ParamNoArg = Param[Int](' ', 'paramNo)
}
