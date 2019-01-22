package fury.module

import fury.error._
import fury._

import scala.util.{Failure, Success}

object Verifier {

  def verifyUniverse(universe: Universe): Outcome[Set[ModuleId]] = {
    val definedModules = for {
      schema  <- universe.schemas.values
      project <- schema.projects
      module  <- project.modules
    } yield module

    definedModules.foldLeft(~Set[ModuleId]()) {
      case (Success(verifiedModules), module) =>
        verifyModule(universe, module, verifiedModules)
      case (f, _) => f
    }
  }

  def verifyModule(universe: Universe, module: Module): Outcome[Set[ModuleId]] =
    verifyModule(universe, module, Set())

  private def verifyModule(
      universe: Universe,
      module: Module,
      acc: Set[ModuleId]
    ): Outcome[Set[ModuleId]] =
    if (acc.contains(module.id)) Success(acc + module.id)
    else
      verifyDependencies(
          universe,
          (module.after + module.compiler).toList,
          acc + module.id
      )

  private def verifyDependencies(
      universe: Universe,
      modules: Seq[ModuleRef],
      acc: Set[ModuleId]
    ): Outcome[Set[ModuleId]] =
    if (modules.isEmpty) Success(acc)
    else
      modules.foldLeft(~acc) {
        case (Success(verified), ref) => verifyRef(universe, ref, verified)
        case (f, _)                   => f
      }

  private def verifyRef(
      universe: Universe,
      moduleRef: ModuleRef,
      acc: Set[ModuleId]
    ): Outcome[Set[ModuleId]] =
    if (moduleRef == ModuleRef.JavaRef) Success(acc)
    else
      for {
        project  <- universe.project(moduleRef.projectId)
        module   <- project.apply(moduleRef.moduleId)
        verified <- verifyModule(universe, module, acc)
      } yield verified
}
