package fury.cli

import fury.FuryCommand
import fury.cli.CLI._

object FuryCLI {

  val interface: CLI[FuryCommand] = menu("fury", "about")(
      command("about", "about Fury", ShowAbout),
      menu("config", "change system configuration options", "set")(
          command("set", "", ConfigMenu.set)
      ),
      menu("dependency", "manage dependencies for the module", "list")(
          command("add", "", DependencyMenu.add),
          command("remove", "", DependencyMenu.remove),
          command("list", "", DependencyMenu.list)
      )
  )
}
