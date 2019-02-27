package fury.cli
import fury._

object DependencyMenu {
  import FuryCLIParameters._

  object add
      extends Action(Schema :+ Project :+ Module :+ Link :+ Intransitive)(bindings =>
        for {
          schema       <- bindings(Schema)
          project      <- bindings(Project)
          module       <- bindings(Module)
          link         <- bindings(Link)
          intransitive <- bindings(Intransitive)
        } yield AddDependency(schema, project, module, link, intransitive))

  object remove
      extends Action(Schema :+ Project :+ Module :+ Link :+ Force)(bindings =>
        for {
          schema  <- bindings(Schema)
          project <- bindings(Project)
          module  <- bindings(Module)
          link    <- bindings(Link)
          force   <- bindings(Force)
        } yield RemoveDependency(schema, project, module, link, force))

  object list
      extends Action(Schema :+ Project :+ Module :+ Link :+ Raw)(bindings =>
        for {
          schema  <- bindings(Schema)
          project <- bindings(Project)
          module  <- bindings(Module)
          raw     <- bindings(Raw)
        } yield ShowDependencies(schema, project, module, raw))

}
