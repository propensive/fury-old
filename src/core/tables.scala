/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.8.0. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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

import fury.strings._, fury.model._

import escritoire.{Ansi => _, _}
import kaleidoscope._

import scala.collection.immutable.SortedSet
import scala.util._

case class Tables() {

  implicit val theme: Theme = ManagedConfig().theme

  def show[T, S: MsgShow](table: Tabulation[T],
                          cols: Int,
                          rows: Seq[T],
                          raw: Boolean)
                         (main: T => S)
                         : Seq[String] =
    if(raw) rows.map(main).map { e => implicitly[MsgShow[S]].show(e).string(Theme.NoColor) }
    else table.tabulate(cols, rows, Some(theme.gray()))

  def contextString(layer: Layer, showSchema: Boolean, elements: UserMsg*): UserMsg =
    (if(showSchema) elements else elements.tail).foldLeft(msg"${'/'}${'/'}$layer") { (l, r) => msg"$l${'/'}$r" }

  implicit private val parameter: AnsiShow[SortedSet[Parameter]] = _.map(_.name).map {
    case s @ r"X.*" => Ansi.brightYellow("-" + s)
    case s @ r"D.*" => Ansi.yellow("-" + s)
    case s @ r"J.*" => Ansi.magenta("-" + s)
    case s @ r"Y.*" => Ansi.red("-" + s)
    case s          => Ansi.green("-" + s)
  }.join("\n")

  implicit private def option[T: AnsiShow]: AnsiShow[Option[T]] = {
    case None    => "-"
    case Some(v) => implicitly[AnsiShow[T]].show(v)
  }

  private def refinedModuleDep(projectId: ProjectId): AnsiShow[SortedSet[ModuleRef]] = _.map {
    case ModuleRef(p, m, intransitive, _) =>
      val extra = (if(intransitive) msg"*" else msg"")
      if(p == projectId) msg"${theme.module(m.key)}$extra"
      else msg"${theme.project(p.key)}${theme.gray("/")}${theme.module(m.key)}$extra"
  }.foldLeft(msg"")(_ + _ + "\n").string(theme)

  implicit private def compilerRef(
      implicit show: AnsiShow[ModuleRef]
    ): AnsiShow[Option[ModuleRef]] = {
    case None      => s"${Ansi.yellow("java")}"
    case Some(ref) => show.show(ref)
  }

  private def bar(n: Int) = msg"${theme.gray("■" * n)}"

  private def commitPath(r: SourceRepo) = r.local match {
    case Some(dir) => msg"${theme.path(dir.value)}"
    case None      => msg"${theme.version(r.commit.id.take(7))}"
  }

  def differences(left: String, right: String): Tabulation[Difference] = Tabulation[Difference](
    Heading("Type", _.entity),
    Heading("Diff", _.label),
    Heading(left, _.left),
    Heading(right, _.right)
  )

  def modules(projectId: ProjectId, current: Option[ModuleId]): Tabulation[Module] = Tabulation[Module](
    Heading("", m => Some(m.id) == current),
    Heading("Module", _.id),
    Heading("Dependencies", (m: Module) => m.after, width = FlexibleWidth)(refinedModuleDep(projectId)),
    Heading("Sources", _.sources),
    Heading("Binaries", m => bar(m.allBinaries.size)),
    Heading("Compiler", _.compiler),
    Heading("Params", m => bar(m.params.size)),
    Heading("Type", _.kind),
    Heading("Details", m => m.kind match {
      case Compiler => m.bloopSpec.fold(msg"${'-'}") { c => msg"$c" }
      case Application => m.main.fold(msg"${'-'}") { a => msg"$a" }
      case _ => msg"${'-'}"
    })
  )

  val aliases: Tabulation[Alias] = Tabulation(
    Heading("Alias", _.cmd),
    Heading("Description", _.description),
    Heading("Module", _.module)
  )

  val dependencies: Tabulation[ModuleRef] = Tabulation[ModuleRef](
    Heading("Project", _.projectId),
    Heading("Module", _.moduleId)
  )

  val sources: Tabulation[Source] = Tabulation(
    Heading("Repo", _.repoIdentifier),
    Heading("Path", _.path)
  )

  val params: Tabulation[Parameter] = Tabulation(
    Heading("Param", _.name)
  )

  val permissions: Tabulation[PermissionEntry] = Tabulation(
    Heading("Hash", _.hash),
    Heading("Class", _.permission.className),
    Heading("Target", _.permission.target),
    Heading("Action", _.permission.action.getOrElse("-")),
  )

  val envs: Tabulation[EnvVar] = Tabulation(
    Heading("Key", _.key),
    Heading("Value", _.value)
  )
  
  val props: Tabulation[JavaProperty] = Tabulation(
    Heading("Key", _.key),
    Heading("Value", _.value)
  )

  val binaries: Tabulation[Binary] = Tabulation(
    Heading("Service", _.binRepo),
    Heading("Group", _.group),
    Heading("Artifact", _.artifact),
    Heading("Version", _.version)
  )

  def imports(current: Option[SchemaId]): Tabulation[(SchemaRef, Try[Schema])] = Tabulation(
    Heading("", s => Some(s._1.schema.key) == current),
    Heading("ID", _._1.id),
    Heading("Ref", _._1.layerRef),
    Heading("Schema", _._1.schema),
    Heading("Projects", s => s._2.toOption.map { s => bar(s.projects.size) }.getOrElse(msg"-")),
    Heading("Repos", s => s._2.toOption.map { s => bar(s.sourceRepoIds.size) }.getOrElse(msg"-")),
    Heading("Imports", s => s._2.toOption.map { s => bar(s.imports.size) }.getOrElse(msg"-"))
  )

  def schemas(current: Option[SchemaId]): Tabulation[Schema] = Tabulation(
    Heading("", s => Some(s.id) == current),
    Heading("Schema", _.id),
    Heading("Projects", s => bar(s.projects.size)),
    Heading("Repos", s => bar(s.sourceRepoIds.size)),
    Heading("Imports", s => bar(s.imports.size))
  )

  def projects(current: Option[ProjectId]): Tabulation[Project] = Tabulation[Project](
    Heading("", p => Some(p.id) == current),
    Heading("Project", _.id),
    Heading("Modules", p => bar(p.modules.size)),
    Heading("Description", _.description),
    Heading("License", _.license),
    Heading("Compiler", _.compiler)
  )

  def repositories(layout: Layout): Tabulation[SourceRepo] = Tabulation(
    Heading("Repo", _.id),
    Heading("Remote", _.repo),
    Heading("Branch/Tag", _.track),
    Heading("Commit/Path", commitPath(_))
  )
}
