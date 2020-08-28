/*

    Fury, version 0.18.19. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

import fury.text._, fury.model._, fury.io._

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

  implicit private def set[T: MsgShow]: AnsiShow[Set[T]] = _.to[List] match {
    case Nil       => "-"
    case List(one) => msg"$one".string(theme)
    case many      => many.map { v => msg"$v" }.reduce(_+"\n"+_).string(theme)
  }

  implicit private val origin: AnsiShow[Origin] = {
    case Origin.Local       => theme.italic(theme.param("local"))
    case Origin.Module(ref) => msg"$ref".string(theme)
    case Origin.Plugin      => theme.italic(theme.param("plugin"))
    case Origin.Compiler    => theme.italic(theme.param("compiler"))
  }

  private def refinedModuleDep(universe: Universe, projectId: ProjectId): AnsiShow[SortedSet[Dependency]] =
    _.map {
      case dependency@Dependency(ref) =>
        val extra = (if(dependency.intransitive) msg"*" else msg"")
        val missing = if(universe(ref).isFailure) msg" ${theme.hazard("!")}" else msg""
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
      Heading("Type", _.kind)
    )

  val aliases: Tabulation[Alias] = Tabulation(
    Heading("Alias", _.id),
    Heading("Description", _.description),
    Heading("Module", _.module),
    Heading("Arguments", _.args.mkString("'", "', '", "'"))
  )

  val dependencies: Tabulation[Dependency] = Tabulation[Dependency](
    Heading("Dependency", identity),
    Heading("Intransitive", _.intransitive)
  )

  def sources(snapshots: Snapshots, layout: Layout): Tabulation[Source] = Tabulation(
    Heading("Repo", _.repoIdentifier),
    Heading("Path", _.dir),
    Heading("Sources", _.glob),
    Heading("Files", _.fileCount(snapshots, layout).getOrElse(0)),
    Heading("Size", _.totalSize(snapshots, layout).getOrElse(ByteSize(0))),
    Heading("Lines", _.linesOfCode(snapshots, layout).getOrElse(0))
  )

  val resources: Tabulation[Source] = Tabulation(
    Heading("Repo", _.repoIdentifier),
    Heading("Base", _.dir),
    Heading("Resources", _.glob)
  )

  val includes: Tabulation[Include] = Tabulation(
    Heading("Include", _.id),
    Heading("Dependency", _.ref),
    Heading("Type", _.kind),
    Heading("Path", _.path)
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
    Heading("Imports", s => s._2.toOption.fold(msg"${'-'}")(_.imports.size)),
    Heading("Published as", s => s._1.remote.fold(msg"${'-'}") { pub => msg"${pub}" })
  )

  private def showPointers(pointers: Iterable[Pointer]): UserMsg = {
    val fewPaths = pointers.take(4).map { k => msg"$k" }.reduce(_ + "\n" + _)
    if(pointers.size > 4) fewPaths+"\n"+msg"...and ${pointers.size - 4} more." else fewPaths
  }

  val entities: Tabulation[(ProjectRef, Project, Set[Pointer])] = Tabulation(
    Heading("Project", _._1),
    Heading("Description", _._2.description),
    Heading("Layers", p => showPointers(p._3))
  )

  def projects(current: Option[ProjectId]): Tabulation[Project] = Tabulation(
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

  val snapshots: Tabulation[Snapshot] = Tabulation(
    Heading("ID", _.hash),
    Heading("Remote", _.remote),
    //Heading("Local", _.local),
    Heading("Commit", _.commit),
    Heading("Branch", _.branch)
  )

  val repoSets: Tabulation[(RepoSetId, Set[RepoRef])] = Tabulation(
    Heading("Commit", _._1),
    Heading("IDs", _._2.map(_.repoId).to[Set].map { id => id: UserMsg }.reduce { (l, r) => msg"$l, $r" }),
    Heading("Layers", _._2.map(_.layer: UserMsg).reduce { (l, r) => l+"\n"+r })
  )
  
  val layerRefs: Tabulation[LayerProvenance] = {
    implicit val pointerOrdering = Ordering.Iterable[ImportId].on[Pointer](_.parts)
    def sortedList[T : MsgShow : Ordering](entries: Iterable[T]): UserMsg =
      entries.to[List].sorted.map { v => v: UserMsg }.reduce { (l, r) => l+"\n"+r }

    Tabulation(
      Heading("Import", _.ref),
      Heading("IDs", _.ids),
      Heading("Remotes", _.published),
      Heading("Imported by", provenance => sortedList(provenance.imports.keySet))
    )
  }
}
