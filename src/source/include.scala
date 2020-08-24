/*

    Fury, version 0.18.2. Copyright 2018-20 Jon Pretty, Propensive OÜ.

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

case class IncludeCli(cli: Cli)(implicit val log: Log) extends CliApi {

  def add: Try[ExitStatus] = (cli -< LayerArg -< ProjectArg -< ModuleArg -< IncludeNameArg -< IncludeTypeArg -<
      PathArg -< ModuleRefArg).action {
    for {
      hierarchy <- getHierarchy
      pointer   <- getPointer
      projectId <- getProject >> (_.id)
      moduleId  <- getModule >> (_.id)
      ref       <- getDependency
      name      <- getIncludeName
      kind      <- getIncludeType
      path      <- get(PathArg)
      hierarchy <- IncludeApi(hierarchy).add(pointer, projectId, moduleId, name, ref, kind, path) >>= commit
    } yield log.await()
  }
  
  def update: Try[ExitStatus] = (cli -< LayerArg -< ProjectArg -< ModuleArg -< IncludeArg -< IncludeNameArg -<
      IncludeTypeArg -< PathArg -< ModuleRefArg).action {
    for {
      hierarchy <- getHierarchy
      pointer   <- getPointer
      projectId <- getProject >> (_.id)
      moduleId  <- getModule >> (_.id)
      ref       <- optDependency
      id        <- get(IncludeArg)
      name      <- opt(IncludeNameArg)
      kind      <- opt(IncludeTypeArg)
      path      <- opt(PathArg)
      hierarchy <- IncludeApi(hierarchy).update(pointer, projectId, moduleId, id, name, ref, kind, path) >>= commit
    } yield log.await()
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
        _         <- ~log.rawln(Tables().show(tabulation, cli.cols, module.includes, raw, col, include, "include"))
      } yield finish(())
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
      } yield log.await()
    }
  }
}

case class IncludeApi(hierarchy: Hierarchy) {
  def remove(pointer: Pointer, projectId: ProjectId, moduleId: ModuleId, includeId: IncludeId)
            (implicit log: Log)
            : Try[Hierarchy] = hierarchy.on(pointer) { layer => for {
    include <- layer.projects.findBy(projectId) >>= (_.modules.findBy(moduleId)) >>= (_.includes.findBy(includeId))
  } yield Layer(_.projects(projectId).modules(moduleId).includes).modify(layer)(_ - include) }

  def add(pointer: Pointer,
          projectId: ProjectId,
          moduleId: ModuleId,
          name: IncludeId,
          ref: ModuleRef,
          kind: IncludeType,
          path: Path)
         (implicit log: Log)
         : Try[Hierarchy] = hierarchy.on(pointer) { layer =>
    val lens = Layer(_.projects(projectId).modules(moduleId).includes)
    val include = Include(name, ref, kind, path)
    
    if(lens(layer).map(_.id).contains(name)) Failure(NotUnique(name))
    else Success(lens.modify(layer)(_ + include))
  }

  def update(pointer: Pointer,
             projectId: ProjectId,
             moduleId: ModuleId,
             id: IncludeId,
             name: Option[IncludeId],
             ref: Option[ModuleRef],
             kind: Option[IncludeType],
             path: Option[Path])
            (implicit log: Log)
            : Try[Hierarchy] = hierarchy.on(pointer) { layer => for {
    lens       <- ~Layer(_.projects(projectId).modules(moduleId).includes)
    _          <- name.fold(~id)(lens(layer).unique(_))
    oldInclude <- lens(layer).findBy(id)
    newInclude <- ~name.fold(oldInclude) { v => oldInclude.copy(id = v) }
    newInclude <- ~ref.fold(newInclude) { v => newInclude.copy(ref = v) }
    newInclude <- ~kind.fold(newInclude) { v => newInclude.copy(kind = v) }
    newInclude <- ~path.fold(newInclude) { v => newInclude.copy(path = v) }
  } yield Layer(_.projects(projectId).modules(moduleId).includes).modify(layer)(_ - oldInclude + newInclude) }
}
