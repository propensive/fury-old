/*

    Fury, version 0.33.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury

import fury._, io._, core._, model._, text._

import jovian._

import Args._

import scala.util._

case class IncludeCli(cli: Cli) extends CliApi {

  implicit lazy val possibleFileHints: SourceArg.Hinter = SourceArg.hint((getLayout, getLayer) >> {
    case (layout, layer) =>
      val extSrcs = layer.repos.map { repo => repo.listFiles(layout).getOrElse(Nil).map { f => RepoSource(repo.id, f, Glob.All) } }.flatten
      val localSrcs = layout.pwd.relativeSubdirsContaining { _ => true }.map(LocalSource(_, Glob.All))
      
      /*val workspaceFiles = layer.workspaces.map { workspace => workspace.listFiles(layout).map { f =>
        WorkspaceSource(workspace.id, f)
      } }.flatten*/

      extSrcs ++ localSrcs// ++ workspaceFiles
    })

  def add: Try[ExitStatus] = (cli -< LayerArg -< ProjectArg -< ModuleArg -< IncludeTypeArg -< PathArg
      -?< (ModuleRefArg, getIncludeType >> oneOf(ClassesDir, Jarfile, JsFile))
      -?< (SourceArg, getIncludeType >> oneOf(FileRef, TarFile, TgzFile))).action {
    for {
      hierarchy <- getHierarchy
      pointer   <- getPointer
      projectId <- getProject >> (_.id)
      moduleId  <- getModule >> (_.id)
      kind      <- getInclude
      path      <- get(PathArg)
      hierarchy <- IncludeApi(hierarchy).add(pointer, projectId, moduleId, kind, path) >>= commit
    } yield cli.job.await()
  }
  
  def update: Try[ExitStatus] = {
    (cli -< LayerArg -< ProjectArg -< ModuleArg -< IncludeArg -< IncludeTypeArg -< PathArg
        -?< (ModuleRefArg, getIncludeType >> oneOf(ClassesDir, Jarfile))
        -?< (SourceArg, getIncludeType >> oneOf(FileRef, TarFile, TgzFile))).action {
      for {
        hierarchy <- getHierarchy
        pointer   <- getPointer
        projectId <- getProject >> (_.id)
        moduleId  <- getModule >> (_.id)
        ref       <- optDependency
        id        <- get(IncludeArg)
        kind      <- opt(IncludeTypeArg).map(_ => getInclude)
        path      <- opt(PathArg)
        hierarchy <- IncludeApi(hierarchy).update(pointer, projectId, moduleId, id, ref, kind.toOption, path) >>= commit
      } yield cli.job.await()
    }
  }
  
  def list: Try[ExitStatus] = {
    val tabulation = Tables().includes
    implicit val columns: ColumnArg.Hinter = ColumnArg.hint(tabulation.headings.map(_.name.toLowerCase))
    (cli -< LayerArg -< ProjectArg -< ModuleArg -< IncludeArg -< ColumnArg -< RawArg).action {
      for {
        project   <- getProject
        module    <- getModule
        pointer   <- getPointer
        hierarchy <- getHierarchy
        focus     <- hierarchy.focus(pointer, project.id, module.id)
        include   <- opt(IncludeArg)
        col       <- opt(ColumnArg)
        _         <- ~log.info(focus)
        _         <- ~log.raw(Tables().show(tabulation, cli.cols, module.includes, raw, col, include, "include")+"\n")
      } yield cli.job.await()
    }
  }

  def remove: Try[ExitStatus] = {
    val tabulation = Tables().includes
    implicit val columns: ColumnArg.Hinter = ColumnArg.hint(tabulation.headings.map(_.name.toLowerCase))
    (cli -< LayerArg -< ProjectArg -< ModuleArg -< IncludeArg).action {
      for {
        project   <- getProject
        module    <- getModule
        pointer   <- getPointer
        hierarchy <- getHierarchy
        include   <- get(IncludeArg)
        hierarchy <- IncludeApi(hierarchy).remove(pointer, project.id, module.id, include) >>= commit
      } yield cli.job.await()
    }
  }
}

case class IncludeApi(hierarchy: Hierarchy) {
  def remove(pointer: Pointer, projectId: ProjectId, moduleId: ModuleId, id: IncludeId)
            : Try[Hierarchy] = hierarchy.on(pointer) { layer => for {
    include <- layer.projects.findBy(projectId) >>= (_.modules.findBy(moduleId)) >>= (_.includes.findBy(id))
  } yield Layer(_.projects(projectId).modules(moduleId).includes).modify(layer)(_ - include) }

  def add(pointer: Pointer, projectId: ProjectId, moduleId: ModuleId, kind: IncludeType, path: Path)
         : Try[Hierarchy] = hierarchy.on(pointer) { layer =>
    val lens = Layer(_.projects(projectId).modules(moduleId).includes)
    val include = Include(IncludeId(path), kind)
    
    if(lens(layer).map(_.id).contains(include.id)) Failure(NotUnique(include.id))
    else Success(lens.modify(layer)(_ + include))
  }

  def update(pointer: Pointer,
             projectId: ProjectId,
             moduleId: ModuleId,
             id: IncludeId,
             ref: Option[ModuleRef],
             kind: Option[IncludeType],
             path: Option[Path])
            : Try[Hierarchy] = hierarchy.on(pointer) { layer => for {
    lens       <- ~Layer(_.projects(projectId).modules(moduleId).includes)
    _          <- path.map(IncludeId(_)).fold(~id)(lens(layer).unique(_))
    oldInclude <- lens(layer).findBy(id)
    newInclude <- ~kind.fold(oldInclude) { v => oldInclude.copy(kind = v) }
    newInclude <- ~path.map(IncludeId(_)).fold(newInclude) { v => newInclude.copy(id = v) }
  } yield Layer(_.projects(projectId).modules(moduleId).includes).modify(layer)(_ - oldInclude + newInclude) }

}
