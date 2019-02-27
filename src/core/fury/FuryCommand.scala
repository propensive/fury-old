package fury

import scala.util.Try

trait FuryCommand

case class AddDependency(
    schema: Option[SchemaId],
    project: Option[ProjectId],
    module: Option[ModuleId],
    link: String,
    intransitive: Boolean)
    extends FuryCommand

case class RemoveDependency(
    schema: Option[SchemaId],
    project: Option[ProjectId],
    module: Option[ModuleId],
    link: String,
    force: Boolean)
    extends FuryCommand

case class ShowDependencies(
    schema: Option[SchemaId],
    project: Option[ProjectId],
    module: Option[ModuleId],
    raw: Boolean)
    extends FuryCommand
