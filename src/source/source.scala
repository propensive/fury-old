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

  def context(cli: Cli[CliParam[_]])(implicit log: Log) = for {
    layout       <- cli.layout
    layer        <- Layer.read(layout)
    schemaArg    <- ~Some(SchemaId.default)
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

  def list(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli     <- cli.hint(RawArg)
      call    <- cli.call()
      raw     <- ~call(RawArg).isSuccess
      project <- optProject.ascribe(UnspecifiedProject())
      module  <- optModule.ascribe(UnspecifiedModule())
      rows    <- ~module.sources.to[List]
      table   <- ~Tables().show(Tables().sources, cli.cols, rows, raw)(_.repoIdentifier)
      schema  <- defaultSchema
      
      _       <- ~(if(!raw) log.info(Tables().contextString(layer, layer.showSchema, schema,
                     project, module)))

      _       <- ~log.rawln(table.mkString("\n"))
    } yield log.await()
  }

  def remove(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli         <- cli.hint(SourceArg, optModule.to[List].flatMap(_.sources))
      cli         <- cli.hint(ForceArg)
      call        <- cli.call()
      sourceArg   <- call(SourceArg)
      source      <- ~Source.unapply(sourceArg)
      project     <- optProject.ascribe(UnspecifiedProject())
      module      <- optModule.ascribe(UnspecifiedModule())
      sourceToDel <- ~module.sources.find(Some(_) == source)
      force       <- ~call(ForceArg).isSuccess

      layer       <- Lenses.updateSchemas(optSchemaId, layer, force)(Lenses.layer.sources(_, project.id,
                         module.id))(_(_) --= sourceToDel)
      
      _           <- ~Layer.save(layer, layout)

      _           <- ~optSchema.foreach(Compilation.asyncCompilation(_, module.ref(project), layout,
                         false))

    } yield log.await()
  }

  def add(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      repos      <- defaultSchema.map(_.repos)

      extSrcs    <- optProject.to[List].flatMap { project =>
                     repos.map(_.sourceCandidates(layout, false) { n =>
                       n.endsWith(".scala") || n.endsWith(".java")
                     })
                   }.sequence.map(_.flatten)
      
      compiler   <- ~optModule.map(_.compiler).getOrElse(ModuleRef.JavaRef)

      localSrcs  <- ~layout.pwd.relativeSubdirsContaining { n => n.endsWith(".scala") || n.endsWith(".java")
                        }.map(LocalSource(_, Glob.All))

      sharedSrcs <- ~layout.sharedDir.relativeSubdirsContaining { n => n.endsWith(".scala") || n.endsWith(
                        ".java") }.map(SharedSource(_, Glob.All))

      cli        <- cli.hint(SourceArg, extSrcs ++ localSrcs ++ sharedSrcs)
      call       <- cli.call()
      project    <- optProject.ascribe(UnspecifiedProject())
      module     <- optModule.ascribe(UnspecifiedModule())
      sourceArg  <- call(SourceArg)
      source     <- ~Source.unapply(sourceArg)

      layer      <- Lenses.updateSchemas(optSchemaId, layer, true)(Lenses.layer.sources(_, project.id, 
                        module.id))(_(_) ++= source)
      
      _          <- ~Layer.save(layer, layout)

      _          <- ~optSchema.foreach(Compilation.asyncCompilation(_, module.ref(project), layout,
                        false))

    } yield log.await()
  }
}

object ResourceCli {

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

  def context(cli: Cli[CliParam[_]])(implicit log: Log) = for {
    layout       <- cli.layout
    layer        <- Layer.read(layout)
    schema       <- ~layer.schemas.findBy(SchemaId.default).toOption
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

  def list(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli     <- cli.hint(RawArg)
      call    <- cli.call()
      raw     <- ~call(RawArg).isSuccess
      project <- optProject.ascribe(UnspecifiedProject())
      module  <- optModule.ascribe(UnspecifiedModule())
      rows    <- ~module.resources.to[List]
      table   <- ~Tables().show(Tables().resources, cli.cols, rows, raw)(_.repoIdentifier)
      schema  <- defaultSchema
      
      _       <- ~(if(!raw) log.info(Tables().contextString(layer, layer.showSchema, schema,
                     project, module)))

      _       <- ~log.rawln(table.mkString("\n"))
    } yield log.await()
  }

  def remove(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      cli           <- cli.hint(SourceArg, optModule.to[List].flatMap(_.resources))
      cli           <- cli.hint(ForceArg)
      call          <- cli.call()
      resourceArg   <- call(SourceArg)
      resource      <- ~Source.unapply(resourceArg)
      project       <- optProject.ascribe(UnspecifiedProject())
      module        <- optModule.ascribe(UnspecifiedModule())
      resourceToDel <- ~module.resources.find(Some(_) == resource)
      force         <- ~call(ForceArg).isSuccess

      layer         <- Lenses.updateSchemas(optSchemaId, layer, force)(Lenses.layer.resources(_, project.id,
                           module.id))(_(_) --= resourceToDel)
      
      _             <- ~Layer.save(layer, layout)

      _             <- ~optSchema.foreach(Compilation.asyncCompilation(_, module.ref(project), layout,
                           false))

    } yield log.await()
  }

  def add(ctx: Context)(implicit log: Log): Try[ExitStatus] = {
    import ctx._
    for {
      repos       <- defaultSchema.map(_.repos)
      cli         <- cli.hint(SourceArg)
      call        <- cli.call()
      project     <- optProject.ascribe(UnspecifiedProject())
      module      <- optModule.ascribe(UnspecifiedModule())
      resourceArg <- call(SourceArg)
      resource    <- ~Source.unapply(resourceArg)

      layer       <- Lenses.updateSchemas(optSchemaId, layer, true)(Lenses.layer.resources(_, project.id, 
                         module.id))(_(_) ++= resource)
      
      _           <- ~Layer.save(layer, layout)
      _           <- ~optSchema.foreach(Compilation.asyncCompilation(_, module.ref(project), layout, false))
    } yield log.await()
  }
}
