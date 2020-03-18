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

import fury.strings._, fury.io._, fury.core._, fury.model._

import guillotine._
import mercator._
import optometry._

import Args._

import scala.util._
import scala.collection.immutable._


case class SourceCli(cli: Cli)(implicit log: Log) {
  def list: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    schemaArg    <- ~Some(SchemaId.default)
    schema       <- ~layer.schemas.findBy(schemaArg.getOrElse(layer.main)).toOption
    cli          <- cli.hint(ProjectArg, schema.map(_.projects).getOrElse(Nil))
    optProjectId <- ~schema.flatMap { s => cli.peek(ProjectArg).orElse(s.main) }
    optProject   <- ~schema.flatMap { s => optProjectId.flatMap(s.projects.findBy(_).toOption) }
    cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    moduleId     <- cli.preview(ModuleArg)(optProject.flatMap(_.main))
    
    optModule    =  (for {
                      project  <- optProject
                      module   <- project.modules.findBy(moduleId).toOption
                    } yield module)

    cli     <- cli.hint(RawArg)
    table   <- ~Tables().sources
    cli     <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
    cli     <- cli.hint(SourceArg, optModule.map(_.sources).getOrElse(Nil))
    call    <- cli.call()
    source  <- ~cli.peek(SourceArg)
    col     <- ~cli.peek(ColumnArg)
    raw     <- ~call(RawArg).isSuccess
    project <- optProject.asTry
    module  <- optModule.asTry
    rows    <- ~module.sources.to[List]
    table   <- ~Tables().show(table, cli.cols, rows, raw, col, source, "repo")
    _       <- ~log.info(conf.focus(project.id, module.id))
    _       <- ~log.rawln(table)
  } yield log.await()

  def remove: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    schemaArg    <- ~Some(SchemaId.default)
    schema       <- ~layer.schemas.findBy(schemaArg.getOrElse(layer.main)).toOption
    cli          <- cli.hint(ProjectArg, schema.map(_.projects).getOrElse(Nil))
    optProjectId <- ~schema.flatMap { s => cli.peek(ProjectArg).orElse(s.main) }
    optProject   <- ~schema.flatMap { s => optProjectId.flatMap(s.projects.findBy(_).toOption) }
    cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    moduleId     <- cli.preview(ModuleArg)(optProject.flatMap(_.main))

    optModule    =  (for {
      project  <- optProject
      module   <- project.modules.findBy(moduleId).toOption
    } yield module)

    cli         <- cli.hint(SourceArg, optModule.to[List].flatMap(_.sources).map(_.completion))
    call        <- cli.call()
    source      <- call(SourceArg)
    project     <- optProject.asTry
    module      <- optModule.asTry
    _           <- if(!module.sources.contains(source)) Failure(InvalidSource(source, module.ref(project))) else Success(())

    layer       <- Lenses.updateSchemas(layer)(Lenses.layer.sources(_, project.id,
                        module.id))(_(_) -= source)
    
    _           <- Layer.commit(layer, conf, layout)
    schema      <- layer.schemas.findBy(SchemaId.default)
    _           <- ~Compilation.asyncCompilation(schema, module.ref(project), layout, false)
  } yield log.await()

  def add: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    layer        <- Layer.retrieve(conf)
    schemaArg    <- ~Some(SchemaId.default)
    schema       <- ~layer.schemas.findBy(schemaArg.getOrElse(layer.main)).toOption
    cli          <- cli.hint(ProjectArg, schema.map(_.projects).getOrElse(Nil))
    optProjectId <- ~schema.flatMap { s => cli.peek(ProjectArg).orElse(s.main) }
    optProject   <- ~schema.flatMap { s => optProjectId.flatMap(s.projects.findBy(_).toOption) }
    cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    moduleId     <- cli.preview(ModuleArg)(optProject.flatMap(_.main))

    optModule    =  (for {
      project  <- optProject
      module   <- project.modules.findBy(moduleId).toOption
    } yield module)

    schema     <- layer.schemas.findBy(SchemaId.default)

    extSrcs    =  optProject.to[List].flatMap { project => schema.repos.map(possibleSourceDirectories(_, layout)) }.flatten
    
    compiler   <- ~optModule.map(_.compiler).getOrElse(ModuleRef.JavaRef)

    localSrcs  <- ~layout.pwd.relativeSubdirsContaining { n => n.endsWith(".scala") || n.endsWith(".java")
                      }.map(LocalSource(_, Glob.All))

    sharedSrcs <- ~layout.sharedDir.relativeSubdirsContaining { n => n.endsWith(".scala") || n.endsWith(
                      ".java") }.map(SharedSource(_, Glob.All))

    cli        <- cli.hint(SourceArg, (extSrcs ++ localSrcs ++ sharedSrcs).map(_.completion))
    call       <- cli.call()
    project    <- optProject.asTry
    module     <- optModule.asTry
    source     <- call(SourceArg)

    layer      <- Lenses.updateSchemas(layer)(Lenses.layer.sources(_, project.id, 
                      module.id))(_(_) ++= Some(source))
    
    _          <- Layer.commit(layer, conf, layout)
    schema      <- layer.schemas.findBy(SchemaId.default)
    _          <- ~Compilation.asyncCompilation(schema, module.ref(project), layout, false)
  } yield log.await()

  private[this] def possibleSourceDirectories(sourceRepo: SourceRepo, layout: Layout) = {
    val sourceFileExtensions = Seq(".scala", ".java")
    sourceRepo.sourceCandidates(layout, false)(n => sourceFileExtensions.exists(n.endsWith)).getOrElse(Set.empty)
  }
}

case class FrontEnd(cli: Cli)(implicit log: Log) {
 
  lazy val layout: Try[Layout] = cli.layout
  lazy val conf: Try[FuryConf] = layout >>= Layer.readFuryConf
  lazy val layer: Try[Layer] = conf >>= (Layer.retrieve(_, false))
  lazy val schema: Try[Schema] = layer >>= (_.schemas.findBy(SchemaId.default))

  lazy val projectId: Try[ProjectId] = schema >>= (cli.preview(ProjectArg)() orElse _.main.asTry)
  lazy val project: Try[Project] = (schema, projectId) >>= (_.projects.findBy(_))
  lazy val moduleId: Try[ModuleId] = project >>= (cli.preview(ModuleArg)() orElse _.main.asTry)
  lazy val module: Try[Module] = (project, moduleId) >>= (_.modules.findBy(_))

  implicit val projectHints: ProjectArg.Hinter = ProjectArg.hint(schema >> (_.projects.map(_.id)))
  implicit val moduleHints: ModuleArg.Hinter = ModuleArg.hint(project >> (_.modules.map(_.id)))
  implicit val sourceHints: SourceArg.Hinter = SourceArg.hint(module >> (_.resources))
  implicit val rawHints: RawArg.Hinter = RawArg.hint(())

  implicit val showUnit: StringShow[Unit] = _ => "*"

  lazy val resources: Try[SortedSet[Source]] = module >> (_.resources)

  def resourcesLens(projectId: ProjectId, moduleId: ModuleId) =
    Lens[Layer](_.schemas(on(SchemaId.default)).projects(on(projectId)).modules(on(moduleId)).resources)

  lazy val resource: Try[Source] = cli.preview(SourceArg)()

  private def removeFromSet[T](items: SortedSet[T], item: T): SortedSet[T] = items - item
  private def addToSet[T](items: SortedSet[T], item: T): SortedSet[T] = items + item
  private def commit(layer: Layer): Try[Unit] = (Try(layer), conf, layout) >>= (Layer.commit(_, _, _, false))
  private def finish[T](result: T): ExitStatus = log.await()

  object Resources {
    lazy val table = Tables().resources
    implicit val columnHints: ColumnArg.Hinter = ColumnArg.hint(~table.headings.map(_.name.toLowerCase))
    
    def list: Try[ExitStatus] = {
      (cli -< ProjectArg -< RawArg -< ModuleArg -< ColumnArg -< SourceArg).action { implicit call =>
        val raw = RawArg().isSuccess
        val column = ColumnArg().toOption
        if(!raw) (conf, projectId, moduleId) >> (_.focus(_, _)) >> (log.info(_))

        
        resources >> (Tables().show(table, cli.cols, _, raw, column, resource.toOption, "source")) >>
            (log.rawln(_)) >> finish
      }
    }

    def remove: Try[ExitStatus] = {
      implicit val sourceHints: SourceArg.Hinter = SourceArg.hint()
      (cli -< ProjectArg -< ModuleArg -< SourceArg).action { implicit call =>
        val lens = (projectId, moduleId) >> resourcesLens
        (resources, SourceArg()) >> removeFromSet >>= { resources =>
          (layer, lens) >> Lenses.set(resources) >> commit >> finish
        }
      }
    }

    def add: Try[ExitStatus] = (cli -< ProjectArg -< ModuleArg -< SourceArg).action { implicit call =>
      val lens = (projectId, moduleId) >> resourcesLens
      (resources, SourceArg()) >> addToSet >>= { resources =>
        (layer, lens) >> Lenses.set(resources) >> commit >> finish
      }
    }
  }
}
