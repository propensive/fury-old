package fury.cli

import fury._
import fury.cli.Parameter._

object FuryCLIParameters {

  val Link = parameter("link", "l", "specify a dependency module")(Some.apply)

  val Intransitive =
    flag("intransitive", "I", "specify if this dependency should not be included transitively")
  val Schema  = optional("schema", "s", "specify a schema")(SchemaId.parse(_).toOption)
  val Project = optional("project", "p", "specify a project")(v => Some(ProjectId(v)))
  val Module  = optional("module", "m", "specify a module")(v => Some(ModuleId(v)))

  val Force = flag("force", "f", "force this operation")
  val Raw   = flag("raw", "r", "display raw output")

  val Theme = parameter("theme", "T", "specify a color theme")(fury.Theme.unapply)
}
