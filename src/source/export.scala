/*

    Fury, version 0.17.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury

import fury.io._, fury.core._, fury.model._

import Args._

import scala.util._

case class ExportCli(cli: Cli)(implicit val log: Log) extends CliApi {

  def add: Try[ExitStatus] = (cli -< LayerArg -< ProjectArg -< ModuleArg -< ExportNameArg -< ExportTypeArg -<
      PathArg -< ModuleRefArg).action {
    for {
      hierarchy <- getHierarchy
      pointer   <- getPointer
      projectId <- getProject >> (_.id)
      moduleId  <- getModule >> (_.id)
      ref       <- getDependency
      name      <- getExportName
      kind      <- getExportType
      path      <- get(PathArg)
      hierarchy <- ExportApi(hierarchy).add(pointer, projectId, moduleId, name, ref, kind, path) >>= commit
    } yield log.await()
  }
  
  def update: Try[ExitStatus] = (cli -< LayerArg -< ProjectArg -< ModuleArg -< ExportArg -< ExportNameArg -<
      ExportTypeArg -< PathArg -< ModuleRefArg).action {
    for {
      hierarchy <- getHierarchy
      pointer   <- getPointer
      projectId <- getProject >> (_.id)
      moduleId  <- getModule >> (_.id)
      ref       <- optDependency
      id        <- get(ExportArg)
      name      <- opt(ExportNameArg)
      kind      <- opt(ExportTypeArg)
      path      <- opt(PathArg)
      hierarchy <- ExportApi(hierarchy).update(pointer, projectId, moduleId, id, name, ref, kind, path) >>= commit
    } yield log.await()
  }
  
  def list: Try[ExitStatus] = {
    val tabulation = Tables().exports
    implicit val columns: ColumnArg.Hinter = ColumnArg.hint(tabulation.headings.map(_.name.toLowerCase))
    (cli -< LayerArg -< ProjectArg -< ModuleArg -< ExportArg -< ColumnArg -< RawArg).action {
      for {
        project   <- getProject
        module    <- getModule
        pointer   <- getPointer
        hierarchy <- getHierarchy
        focus     <- hierarchy.focus(pointer, project.id, module.id)
        export    <- opt(ExportArg)
        col       <- opt(ColumnArg)
        _         <- ~log.info(focus)
        _         <- ~log.rawln(Tables().show(tabulation, cli.cols, module.exports, raw, col, export, "export"))
      } yield finish(())
    }
  }

  def remove: Try[ExitStatus] = {
    val tabulation = Tables().exports
    implicit val columns: ColumnArg.Hinter = ColumnArg.hint(tabulation.headings.map(_.name.toLowerCase))
    (cli -< LayerArg -< ProjectArg -< ModuleArg -< ExportArg).action {
      for {
        project   <- getProject
        module    <- getModule
        pointer   <- getPointer
        hierarchy <- getHierarchy
        export    <- get(ExportArg)
        hierarchy <- ExportApi(hierarchy).remove(pointer, project.id, module.id, export) >>= commit
      } yield log.await()
    }
  }
}

case class ExportApi(hierarchy: Hierarchy) {
  def remove(pointer: Pointer, projectId: ProjectId, moduleId: ModuleId, exportId: ExportId)
            (implicit log: Log)
            : Try[Hierarchy] = hierarchy.on(pointer) { layer => for {
    export <- layer.projects.findBy(projectId) >>= (_.modules.findBy(moduleId)) >>= (_.exports.findBy(exportId))
  } yield Layer(_.projects(projectId).modules(moduleId).exports).modify(layer)(_ - export) }

  def add(pointer: Pointer,
          projectId: ProjectId,
          moduleId: ModuleId,
          name: ExportId,
          ref: ModuleRef,
          kind: ExportType,
          path: Path)
         (implicit log: Log)
         : Try[Hierarchy] = hierarchy.on(pointer) { layer =>
    val lens = Layer(_.projects(projectId).modules(moduleId).exports)
    val export = Export(name, ref, kind, path)
    
    if(lens(layer).map(_.id).contains(name)) Failure(NotUnique(name))
    else Success(lens.modify(layer)(_ + export))
  }

  def update(pointer: Pointer,
             projectId: ProjectId,
             moduleId: ModuleId,
             id: ExportId,
             name: Option[ExportId],
             ref: Option[ModuleRef],
             kind: Option[ExportType],
             path: Option[Path])
            (implicit log: Log)
            : Try[Hierarchy] = hierarchy.on(pointer) { layer => for {
    lens      <- ~Layer(_.projects(projectId).modules(moduleId).exports)
    _         <- name.fold(~id)(lens(layer).unique(_))
    oldExport <- lens(layer).findBy(id)
    newExport <- ~name.fold(oldExport) { v => oldExport.copy(id = v) }
    newExport <- ~ref.fold(newExport) { v => newExport.copy(ref = v) }
    newExport <- ~kind.fold(newExport) { v => newExport.copy(kind = v) }
    newExport <- ~path.fold(newExport) { v => newExport.copy(path = v) }
  } yield Layer(_.projects(projectId).modules(moduleId).exports).modify(layer)(_ - oldExport + newExport) }
}
