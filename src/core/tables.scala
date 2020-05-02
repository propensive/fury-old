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
package fury.core

import fury.strings._, fury.model._, fury.io._

import escritoire.{Ansi => _, _}
import kaleidoscope._

import scala.collection.immutable.SortedSet
import scala.util._
import guillotine.Environment

case class Tables() {

  implicit val theme: Theme = ManagedConfig().theme

  def show[T, S: MsgShow](table: Tabulation[T],
                             cols: Int,
                             rows: Traversable[T],
                             raw: Boolean,
                             column: Option[String] = None,
                             row: Option[S] = None,
                             main: String = "id")
                            : String = {

    val mainHeading = table.headings.find(_.name.toLowerCase == main.toLowerCase).getOrElse(table.headings(0))
    val showRows = row.fold(rows) { row => rows.filter { r =>
      mainHeading.get(r) == implicitly[MsgShow[S]].show(row).string(theme)
    } }
    val showTable = column.fold(table) { col =>
      Tabulation(table.headings.filter(_.name.toLowerCase == col.toLowerCase): _*)
    }
    
    if(raw) {
      val col = column.flatMap { col =>
        table.headings.find(_.name.toLowerCase == col.toLowerCase)
      }.getOrElse(mainHeading)

      showRows.map(col.get).join("\n")
    } else showTable.tabulate(cols, showRows.to[Seq], Some(theme.gray())).join("\n")
  }

  implicit private val parameter: AnsiShow[SortedSet[Opt]] = _.map(_.id.key).map {
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

  implicit private val origin: AnsiShow[Origin] = {
    case Origin.Local       => theme.italic(theme.param("local"))
    case Origin.Module(ref) => msg"$ref".string(theme)
    case Origin.Plugin      => theme.italic(theme.param("plugin"))
    case Origin.Compiler    => theme.italic(theme.param("compiler"))
  }

  private def refinedModuleDep(universe: Universe, projectId: ProjectId): AnsiShow[SortedSet[ModuleRef]] =
    _.map {
      case ref@ModuleRef(id, intransitive, _) =>
        val extra = (if(intransitive) msg"*" else msg"")
        val missing = if(universe.getMod(ref).isFailure) msg" ${theme.hazard("!")}" else msg""
        if(ref.projectId == projectId) msg"${theme.module(ref.moduleId.key)}$extra$missing"
        else msg"${theme.project(ref.projectId.key)}${'/'}${theme.module(ref.moduleId.key)}$extra$missing"
    }.foldLeft(msg"")(_ + _ + "\n").string(theme)

  implicit private def compilerRef(
      implicit show: AnsiShow[ModuleRef]
    ): AnsiShow[Option[ModuleRef]] = {
    case None      => s"${Ansi.yellow("java")}"
    case Some(ref) => show.show(ref)
  }

  def differences(left: String, right: String): Tabulation[Difference] = Tabulation[Difference](
    Heading("Type", _.entity),
    Heading("Diff", _.label),
    Heading(left, _.left),
    Heading(right, _.right)
  )

  def modules(projectId: ProjectId, current: Option[ModuleId], universe: Universe): Tabulation[Module] =
    Tabulation[Module](
      Heading("", m => Some(m.id) == current),
      Heading("Module", _.id),
      Heading("Dependencies", (m: Module) =>
        m.dependencies, width = FlexibleWidth)(refinedModuleDep(universe, projectId)
      ),
      Heading("Sources", _.sources),
      Heading("Binaries", m => m.allBinaries.size),
      Heading("Compiler", _.compiler),
      Heading("Options", m => m.opts.size),
      Heading("Type", _.kind),
      Heading("Details", m => m.kind match {
        case Compiler => m.bloopSpec.fold(msg"${'-'}") { c => msg"$c" }
        case Application => m.main.fold(msg"${'-'}") { a => msg"$a" }
        case _ => msg"${'-'}"
      })
    )

  val aliases: Tabulation[Alias] = Tabulation(
    Heading("Alias", _.id),
    Heading("Description", _.description),
    Heading("Module", _.module),
    Heading("Arguments", _.args.mkString("'", "', '", "'"))
  )

  val dependencies: Tabulation[ModuleRef] = Tabulation[ModuleRef](
    Heading("Dependency", identity),
    Heading("Intransitive", _.intransitive)
  )

  def sources(checkouts: Checkouts, layout: Layout): Tabulation[Source] = Tabulation(
    Heading("Repo", _.repoIdentifier),
    Heading("Path", _.dir),
    Heading("Sources", _.glob),
    Heading("Files", _.fileCount(checkouts, layout).getOrElse(0)),
    Heading("Size", _.totalSize(checkouts, layout).getOrElse(ByteSize(0))),
    Heading("Lines", _.linesOfCode(checkouts, layout).getOrElse(0))
  )

  val resources: Tabulation[Source] = Tabulation(
    Heading("Repo", _.repoIdentifier),
    Heading("Base", _.dir),
    Heading("Resources", _.glob)
  )

  val opts: Tabulation[Provenance[Opt]] = Tabulation(
    Heading("", o => if(o.value.remove) "-" else "+"),
    Heading("Param", _.value.id),
    Heading("Persistent", o => if(o.value.persistent) "Yes" else "No"),
    Heading("Origin", _.source),
  )

  val permissions: Tabulation[PermissionEntry] = Tabulation(
    Heading("Hash", _.hash),
    Heading("Class", _.permission.classRef),
    Heading("Target", _.permission.target),
    Heading("Action", _.permission.action.getOrElse("-")),
  )

  val envs: Tabulation[EnvVar] = Tabulation(
    Heading("Key", _.id),
    Heading("Value", _.value)
  )
  
  val props: Tabulation[JavaProperty] = Tabulation(
    Heading("Key", _.id),
    Heading("Value", _.value)
  )

  def software(env: Environment): Tabulation[Software] = Tabulation(
    Heading("Command", _.name),
    Heading("Name", _.description),
    Heading("Version", _.version(env)),
    Heading("Website", _.website),
    Heading("Path", _.path(env))
  )

  val binaries: Tabulation[Binary] = Tabulation(
    Heading("Binary", _.id),
    Heading("Service", _.binRepo),
    Heading("Group", _.group),
    Heading("Artifact", _.artifact),
    Heading("Version", _.version)
  )

  val imports: Tabulation[(Import, Try[Layer])] = Tabulation(
    Heading("ID", _._1.id),
    Heading("Ref", _._1.layerRef),
    Heading("Projects", s => s._2.toOption.fold(msg"${'-'}")(_.projects.size)),
    Heading("Repos", s => s._2.toOption.fold(msg"${'-'}")(_.repoIds.size)),
    Heading("Imports", s => s._2.toOption.fold(msg"${'-'}")(_.imports.size))
  )

  def projects(current: Option[ProjectId]): Tabulation[Project] = Tabulation[Project](
    Heading("", p => Some(p.id) == current),
    Heading("Project", _.id),
    Heading("Modules", p => p.modules.size),
    Heading("Description", _.description),
    Heading("License", _.license),
    Heading("Compiler", _.compiler)
  )

  def repos(layout: Layout)(implicit log: Log): Tabulation[Repo] = Tabulation(
    Heading("Repo", _.id),
    Heading("Remote", _.remote),
    Heading("Branch/Tag", _.branch),
    Heading("Commit", _.commit),
    Heading("Path", _.local),
    Heading("Changes", r => if(r.local.isEmpty) None else r.changes(layout).toOption.flatten)
  )
}
