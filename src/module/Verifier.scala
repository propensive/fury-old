package fury.module

import fury.core._

import scala.util.{Success, Try}

object Verifier {

  def verifyLayer(universe: Universe, layer: Layer): Try[Set[ModuleId]] = {
    val dependencies = for {
      schema     <- layer.schemas
      project    <- schema.projects
      module     <- project.modules
      dependency <- module.after + module.compiler
    } yield dependency

    verify(universe, dependencies.toList, Set())
  }

  def verifyModule(universe: Universe, module: Module): Try[Set[ModuleId]] = {
    val dependencies = module.after + module.compiler
    verify(universe, dependencies.toList, Set())
  }

  private def verify(
      universe: Universe,
      dependencies: List[ModuleRef],
      verified: Set[ModuleId]
    ): Try[Set[ModuleId]] =
    dependencies match {
      case Nil                       => Success(verified)
      case ModuleRef.JavaRef :: tail => verify(universe, tail, verified)

      case ref :: tail if verified.contains(ref.moduleId) => verify(universe, tail, verified)
      case ref :: tail =>
        for {
          module <- universe.getMod(ref)
          deps    = module.after + module.compiler
          result  <- verify(universe, tail ++ deps, verified + module.id)
        } yield result
    }
}
