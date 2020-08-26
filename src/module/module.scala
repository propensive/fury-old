/*

    Fury, version 0.18.8. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury

import fury.text._,  fury.core._, fury.model._

import mercator._
import optometry._

import scala.collection.immutable.SortedSet
import scala.util._

import Args._

case class ModuleCli(cli: Cli)(implicit val log: Log) extends CliApi{
  
  private def resolveToCompiler(ref: CompilerRef): Try[CompilerRef] = ref match {
    case javac: Javac => ~javac
    case bsp@BspCompiler(ref) =>
      getCompilerRefs >>= { available => if(available.contains(ref)) ~bsp else Failure(UnknownModule(ref)) }
  }

  def select: Try[ExitStatus] = (cli -< ProjectArg -< ModuleArg).action {
    (requiredModule >> (_.id) >> (Option(_)), getLayer, mainModuleLens) >> (Layer.set(_)(_, _)) >> commit >> finish
  }

  def list: Try[ExitStatus] = {
    implicit val columns: ColumnArg.Hinter = ColumnArg.hint(getTable >> (_.headings.map(_.name)))
    (cli -< ProjectArg -< ModuleArg -< ColumnArg -< RawArg).action {
      val output = (getTable, getProject >> (_.modules), opt(ColumnArg), opt(ModuleArg)) >>
        (Tables().show(_, cli.cols, _, raw, _, _, "module"))
      (conf, getProject, output) >> { case (c, p, o) =>
        log.infoWhen(!raw)(c.focus(p.id))
        log.rawln(o)
        log.await()
      }
    }
  }

  def add: Try[ExitStatus] = {
    (cli -< ProjectArg -< KindArg -< ModuleNameArg -< HiddenArg -< CompilerArg
      -?< (PluginArg, getKindName >> oneOf(Plugin))
      -?< (MainArg, getKindName >> oneOf(App, Plugin, Bench))
      -?< (ReplArg, getKindName >> oneOf(Compiler))
      -?< (TimeoutArg, getKindName >> oneOf(App))
      -?< (SpecArg, getKindName >> oneOf(Compiler))
      ).action {
      val newModule = getModuleName >> (x => Module(id = x)) >>= updatedFromCli
      val newModules = (getProject >> (_.modules), newModule) >> (_ + _)
      
      val newLayer = for {
        layer              <- (newModules, getLayer, modulesLens) >> (Layer.set(_)(_, _))
        
        layer              <- (newModule >> (_.id) >> (Option(_)), ~layer, mainModuleLens) >>
                                  (Layer.set(_)(_, _))

        module             <- newModule
        project            <- getProject
        setDefaultCompiler <- (getProject, opt(CompilerArg)) >> (_.compiler.isEmpty && _.isDefined)
        lens               <- defaultCompilerLens
      } yield {
        
        val newLayer = if(setDefaultCompiler) {
          log.info(msg"Setting default compiler for ${project.id} to ${module.compiler}")
          Layer.set(Option(module.compiler))(layer, lens)
        } else layer
        
        log.info(msg"Set current module to ${module.id}")
        
        newLayer
      }
      for {
        _ <- newLayer >>= commit
        _ <- (newLayer, (newModule, getProject) >> (_.ref(_)), getLayout) >> Build.asyncBuild
      } yield log.await()
    }
  }

  def remove: Try[ExitStatus] = {
    (cli -< ProjectArg -< ModuleArg).action {
      val newModules = (getProject >> (_.modules), requiredModule) >> (_ - _)
      
      val newLayer = for {
        newLayer  <- (newModules, getLayer, modulesLens) >> (Layer.set(_)(_, _))
        cleanMain <- (getProject >> (_.main), requiredModule >> (_.id)) >> (_.contains(_))
        lens      <- mainModuleLens
      } yield if(cleanMain) Layer.set(Option.empty[ModuleId])(newLayer, lens) else newLayer
      
      for(_ <- newLayer >>= commit; _ <- (newLayer, getModuleRef, getLayout) >> Build.asyncBuild)
      yield log.await()
    }
  }

  def update: Try[ExitStatus] = {
    (cli -< ProjectArg -< ModuleArg -< KindArg -< ModuleNameArg -< HiddenArg -< CompilerArg
        -?< (PluginArg, getKindName >> oneOf(Plugin))
        -?< (MainArg, getKindName >> oneOf(App, Plugin, Bench))
        -?< (ReplArg, getKindName >> oneOf(Compiler))
        -?< (TimeoutArg, getKindName >> oneOf(App, Container))
        -?< (WorkspaceArg, getKindName >> oneOf(App, Container))
        -?< (ImageArg, getKindName >> oneOf(Container))
        -?< (SpecArg, getKindName >> oneOf(Compiler))
      ).action {

      val newModule = getModule >>= renamedFromCli >>= updatedFromCli
      val newModules = (getProject >> (_.modules), newModule) >> (_ + _)
      
      val newLayer = for {
        layer       <- (newModules, getLayer, modulesLens) >> (Layer.set(_)(_, _))
        updateMain  <- (getProject >> (_.main), getModule >> (_.id)) >> (_.contains(_))
        lens        <- mainModuleLens
        newModuleId <- newModule >> (_.id)
      } yield if(updateMain) Layer.set(Option(newModuleId))(layer, lens) else layer
      
      for(_ <- newLayer >>= commit; _ <- (newLayer, getModuleRef, getLayout) >> Build.asyncBuild)
      yield log.await()
    }
  }

  private[this] def renamedFromCli(base: Module): Try[Module] = opt(ModuleNameArg) >> (_.fold(base)(x => base.copy(id = x)))

  private[this] def updatedFromCli(base: Module): Try[Module] = for {
    kind     <- ~cliKind.getOrElse(base.kind)
    compiler <- cliCompiler >> (_.getOrElse(base.compiler))
  } yield base.copy(compiler = compiler, kind = kind, hidden = has(HiddenArg))

  private[this] lazy val cliKind: Try[Kind] = getKindName >>= (_ match {
    case Lib       => ~Lib()
    case App       => (get(MainArg), get(TimeoutArg).orElse(Success(0)), opt(WorkspaceArg)) >> App.apply
    case Container => (get(ImageArg), get(TimeoutArg).orElse(Success(0)), opt(WorkspaceArg)) >> Container.apply
    case Bench     => get(MainArg) >> Bench.apply
    case Compiler  => (get(SpecArg), get(ReplArg).orElse(~ClassRef("scala.tools.nsc.MainGenericRunner"))) >> Compiler.apply
    case Plugin    => (get(PluginArg), get(MainArg)) >> Plugin.apply
  })

  private[this] lazy val cliCompiler = opt(CompilerArg) >>= {
    case Some(ref) => resolveToCompiler(ref) >> (Some(_))
    case None => ~None
  }

  private[this] lazy val getCompilerRefs = (getLayer, getLayout) >> (_.compilerRefs(_))
  private[this] lazy val getTable = (getProject >> (_.id), getProject >> (_.main), universe) >> (Tables().modules(_, _, _))
  private[this] lazy val getModuleName = (getProject >> (_.modules), get(ModuleNameArg)) >>= (_.unique(_))
  private[this] lazy val getKindName: Try[Kind.Id] = get(KindArg) orElse getModule >> (_.kind.name)
  private[this] implicit lazy val moduleKindsHint: KindArg.Hinter = KindArg.hint(Kind.ids)
  private[this] implicit lazy val hiddenHint: HiddenArg.Hinter = HiddenArg.hint(true, false)
  private[this] implicit lazy val pluginHint: PluginArg.Hinter = PluginArg.hint()
  private[this] implicit lazy val moduleNamesHint: ModuleNameArg.Hinter = ModuleNameArg.hint()
  private[this] implicit lazy val specHint: SpecArg.Hinter = SpecArg.hint()

  private[this] implicit lazy val compilersHint: CompilerArg.Hinter = CompilerArg.hint {
    (getCompilerRefs >> (_.map(BspCompiler(_)) ++ Javac.Versions)).orElse(~Javac.Versions)
  }

  private[this] implicit lazy val mainClassHint: MainArg.Hinter = MainArg.hint {
    (getLayout, getModuleRef) >> (_.classesDir(_)) >> (Asm.executableClasses(_))
  }

  private[this] implicit lazy val replHint: ReplArg.Hinter = ReplArg.hint(
    (getLayout, getModuleRef) >> (_.classesDir(_)) >> (Asm.executableClasses(_))
  )

  private[this] implicit lazy val timeoutHint: TimeoutArg.Hinter = TimeoutArg.hint(0)

  private[this] def modulesLens: Try[Lens[Layer, SortedSet[Module], SortedSet[Module]]] = getProject >>
    { case p => Lens[Layer](_.projects(p.id).modules) }

  private[this] def mainModuleLens: Try[Lens[Layer, Option[ModuleId], Option[ModuleId]]] = getProject >>
    { case p => Lens[Layer](_.projects(p.id).main)}

  private[this] def defaultCompilerLens: Try[Lens[Layer, Option[CompilerRef], Option[CompilerRef]]] = getProject >>
    { case p => Lens[Layer](_.projects(p.id).compiler) }
}
