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
package fury.core

import fury.strings._

import escritoire.{Ansi => _, _}
import kaleidoscope._

import scala.collection.immutable.SortedSet
import scala.util._

case class Tables(config: Config) {

  implicit val theme: Theme = config.theme

  def show[T, S: MsgShow](
      table: Tabulation[T],
      cols: Int,
      rows: Seq[T],
      raw: Boolean
    )(main: T => S
    ): Seq[String] =
    if (raw) rows.map(main).map { e =>
      implicitly[MsgShow[S]].show(e).string(Theme.NoColor)
    } else table.tabulate(cols, rows)

  def contextString(layer: UserMsg, showSchema: Boolean, elements: UserMsg*): UserMsg =
    (if (showSchema) elements else elements.tail).foldLeft(layer) { (l, r) =>
      msg"$l/$r"
    }

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

  private def refinedModuleDep(projectId: ProjectId): AnsiShow[SortedSet[ModuleRef]] =
    _.map {
      case ModuleRef(p, m, intransitive, _) =>
        val extra = (if (intransitive) msg"*" else msg"")
        if (p == projectId) msg"${theme.module(m.key)}$extra"
        else msg"${theme.project(p.key)}${theme.gray("/")}${theme.module(m.key)}$extra"
    }.foldLeft(msg"")(_ + _ + "\n").string(theme)

  implicit private def compilerRef(
      implicit show: AnsiShow[ModuleRef]
    ): AnsiShow[Option[ModuleRef]] = {
    case None      => s"${Ansi.yellow("java")}"
    case Some(ref) => show.show(ref)
  }

  def bar(n: Int) = msg"${theme.gray("■" * n)}"

  def commitPath(r: SourceRepo) = r.local match {
    case Some(dir) => msg"${theme.path(dir.value)}"
    case None      => msg"${theme.version(r.commit.id.take(7))}"
  }

  def differences(left: String, right: String): Tabulation[Difference] =
    Tabulation[Difference](
        Heading("TYPE", _.entity),
        Heading("DIFF", _.label),
        Heading(left.toUpperCase, _.left),
        Heading(right.toUpperCase, _.right)
    )

  def modules(projectId: ProjectId, current: Option[ModuleId]): Tabulation[Module] =
    Tabulation[Module](
        Heading("", m => Some(m.id) == current),
        Heading("MODULE", _.id),
        Heading("TYPE", _.kind),
        Heading("DEPENDENCIES", (m: Module) => m.after, width = FlexibleWidth)(
            refinedModuleDep(projectId)),
        Heading("SRCS", _.sources),
        Heading("BINS", m => bar(m.allBinaries.size)),
        Heading("COMPILER", _.compiler),
        Heading("TYPE", _.kind),
        Heading("PARAMS", m => bar(m.params.size))
    )

  val aliases: Tabulation[Alias] = Tabulation(
      Heading("ALIAS", _.cmd),
      Heading("DESC", _.description),
      Heading("MODULE", _.module)
  )

  val dependencies: Tabulation[ModuleRef] = Tabulation[ModuleRef](
      Heading("PROJECT", _.projectId),
      Heading("MODULE", _.moduleId)
  )

  val sources: Tabulation[Source] = Tabulation(
      Heading("REPO", _.repoIdentifier),
      Heading("PATH", _.path)
  )

  val params: Tabulation[Parameter] = Tabulation(
      Heading("PARAM", _.name)
  )

  val binaries: Tabulation[Binary] = Tabulation(
      Heading("SERVICE", _.binRepo),
      Heading("GROUP", _.group),
      Heading("ARTIFACT", _.artifact),
      Heading("VERSION", _.version)
  )

  def imports(current: Option[SchemaId]): Tabulation[(SchemaRef, Try[Schema])] = Tabulation(
      Heading("", s => Some(s._1.schema.key) == current),
      Heading("REPO", _._1.repo),
      Heading("SCHEMA", _._1.schema),
      Heading(
          "PROJECTS",
          s =>
            s._2.toOption.map { s =>
              bar(s.projects.size)
            }.getOrElse(msg"-")),
      Heading(
          "REPOS",
          s =>
            s._2.toOption.map { s =>
              bar(s.sourceRepoIds.size)
            }.getOrElse(msg"-")),
      Heading(
          "IMPORTS",
          s =>
            s._2.toOption.map { s =>
              bar(s.imports.size)
            }.getOrElse(msg"-"))
  )

  def schemas(current: Option[SchemaId]): Tabulation[Schema] = Tabulation(
      Heading("", s => Some(s.id) == current),
      Heading("SCHEMA", _.id),
      Heading("PROJECTS", s => bar(s.projects.size)),
      Heading("REPOS", s => bar(s.sourceRepoIds.size)),
      Heading("IMPORTS", s => bar(s.imports.size))
  )

  def projects(current: Option[ProjectId]): Tabulation[Project] = Tabulation[Project](
      Heading("", p => Some(p.id) == current),
      Heading("PROJECT", _.id),
      Heading("MODULES", p => bar(p.modules.size)),
      Heading("DESC", _.description),
      Heading("LICENSE", _.license),
      Heading("COMPILER", _.compiler)
  )

  def repositories(layout: Layout): Tabulation[SourceRepo] = Tabulation(
      Heading("REPO", _.id),
      Heading("REMOTE", _.repo),
      Heading("TRACK", _.track),
      Heading("COMMIT/PATH", commitPath(_))
  )

}
