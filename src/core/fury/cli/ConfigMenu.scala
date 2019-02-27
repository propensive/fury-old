package fury.cli

object ConfigMenu {
  import FuryCLIParameters._

  object set
      extends Action(Parameters.of(Theme))(bindings =>
        for {
          theme <- bindings(Theme)
        } yield ???)
}
