package fury.module

import fury._
import fury.error._

import scala.util.Success

object Verifier {

  def verifyLayer(universe: Universe, layer: Layer): Outcome[Set[ModuleId]] = {
    val dependencies = for {
      schema     <- layer.schemas
      project    <- schema.projects
      module     <- project.modules
      dependency <- module.after + module.compiler
    } yield dependency

    verify(universe, dependencies.toList, Set())
  }

  def verifyModule(universe: Universe, module: Module): Outcome[Set[ModuleId]] = {
    val dependencies = module.after + module.compiler
    verify(universe, dependencies.toList, Set())
  }

  private def verify(
      universe: Universe,
      dependencies: List[ModuleRef],
      verified: Set[ModuleId]
    ): Outcome[Set[ModuleId]] =
    dependencies match {
      case Nil                       => Success(verified)
      case ModuleRef.JavaRef :: tail => verify(universe, tail, verified)

      case ref :: tail if verified.contains(ref.moduleId) => verify(universe, tail, verified)
      case ref :: tail =>
        for {
          project <- universe.project(ref.projectId)
          module  <- project.apply(ref.moduleId)
          result  <- verify(universe, tail, verified + module.id)
        } yield result
    }
}
