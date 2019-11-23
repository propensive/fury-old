/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.7.12. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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
import Args._
import scala.util._

object SourceCli {

  case class Context(override val cli: Cli[CliParam[_]],
                     override val layout: Layout,
                     override val layer: Layer,
                     optSchema: Option[Schema],
                     optProject: Option[Project],
                     optModule: Option[Module])
             extends MenuContext(cli, layout, layer, optSchema.map(_.id)) {

    def defaultSchemaId: SchemaId  = optSchemaId.getOrElse(layer.main)
    def defaultSchema: Try[Schema] = layer.schemas.findBy(defaultSchemaId)
  }

  def context(cli: Cli[CliParam[_]]) = for {
    layout       <- cli.layout
    layer        <- Layer.read(Log.silent, layout)
    cli          <- cli.hint(SchemaArg, layer.schemas)
    schemaArg    <- ~cli.peek(SchemaArg)
    schema       <- ~layer.schemas.findBy(schemaArg.getOrElse(layer.main)).toOption
    cli          <- cli.hint(ProjectArg, schema.map(_.projects).getOrElse(Nil))
    optProjectId <- ~schema.flatMap { s => cli.peek(ProjectArg).orElse(s.main) }
    optProject   <- ~schema.flatMap { s => optProjectId.flatMap(s.projects.findBy(_).toOption) }
    cli          <- cli.hint(ModuleArg, optProject.to[List].flatMap(_.modules))
    optModuleId  <- ~cli.peek(ModuleArg).orElse(optProject.flatMap(_.main))
    
    optModule    <- Success { for {
                      project  <- optProject
                      moduleId <- optModuleId
                      module   <- project.modules.findBy(moduleId).toOption
                    } yield module }

  } yield new Context(cli, layout, layer, schema, optProject, optModule)

  def list(ctx: Context): Try[ExitStatus] = {
    import ctx._
    for {
      cli     <- cli.hint(RawArg)
      invoc   <- cli.read()
      log     <- invoc.logger()
      raw     <- ~invoc(RawArg).isSuccess
      project <- optProject.ascribe(UnspecifiedProject())
      module  <- optModule.ascribe(UnspecifiedModule())
      rows    <- ~module.sources.to[List]
      table   <- ~Tables().show(Tables().sources, cli.cols, rows, raw)(_.repoIdentifier)
      schema  <- defaultSchema
      
      _       <- ~(if(!raw) log.println(Tables().contextString(layout.baseDir, layer.showSchema, schema,
                     project, module)))

      _       <- ~log.println(table.mkString("\n"))
    } yield log.await()
  }

  def remove(ctx: Context): Try[ExitStatus] = {
    import ctx._
    for {
      cli         <- cli.hint(SourceArg, optModule.to[List].flatMap(_.sources))
      cli         <- cli.hint(ForceArg)
      invoc       <- cli.read()
      log         <- invoc.logger()
      sourceArg   <- invoc(SourceArg)
      source      <- ~Source.unapply(sourceArg)
      project     <- optProject.ascribe(UnspecifiedProject())
      module      <- optModule.ascribe(UnspecifiedModule())
      sourceToDel <- ~module.sources.find(Some(_) == source)
      force       <- ~invoc(ForceArg).isSuccess

      layer       <- Lenses.updateSchemas(optSchemaId, layer, force)(Lenses.layer.sources(_, project.id,
                         module.id))(_(_) --= sourceToDel)
      
      _           <- ~Layer.save(log, layer, layout)

      _           <- ~optSchema.foreach(Compilation.asyncCompilation(log, _, module.ref(project), layout,
                         false))

    } yield log.await()
  }

  def add(ctx: Context): Try[ExitStatus] = {
    import ctx._
    for {
      repos      <- defaultSchema.map(_.repos)

      extSrcs    <- optProject.to[List].flatMap { project =>
                     repos.map(_.sourceCandidates(Log.silent, layout, false) { n =>
                       n.endsWith(".scala") || n.endsWith(".java")
                     })
                   }.sequence.map(_.flatten)

      localSrcs  <- ~layout.pwd.relativeSubdirsContaining { n => n.endsWith(".scala") || n.endsWith(".java")
                        }.map(LocalSource(_))

      sharedSrcs <- ~layout.sharedDir.relativeSubdirsContaining { n => n.endsWith(".scala") || n.endsWith(
                        ".java") }.map(SharedSource(_))

      cli        <- cli.hint(SourceArg, extSrcs ++ localSrcs ++ sharedSrcs)
      invoc      <- cli.read()
      log        <- invoc.logger()
      project    <- optProject.ascribe(UnspecifiedProject())
      module     <- optModule.ascribe(UnspecifiedModule())
      sourceArg  <- invoc(SourceArg)
      source     <- ~Source.unapply(sourceArg)

      layer      <- Lenses.updateSchemas(optSchemaId, layer, true)(Lenses.layer.sources(_, project.id, 
                        module.id))(_(_) ++= source)
      
      _          <- ~Layer.save(log, layer, layout)

      _          <- ~optSchema.foreach(Compilation.asyncCompilation(log, _, module.ref(project), layout,
                        false))

    } yield log.await()
  }
}
