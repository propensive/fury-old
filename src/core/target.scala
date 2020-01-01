package fury.core

import fury.model._, fury.io._

object Target {
    case class Graph(dependencies: Map[TargetId, Set[TargetId]], targets: Map[TargetId, Target]) {
      def links: Map[ModuleRef, Set[ModuleRef]] = dependencies.map { case (k, ds) =>
        (k.ref, ds.map { d => d.ref.copy(hidden = targets(d).kind == Compiler) })
      }.toMap
    }
  }
  
  case class Target(ref: ModuleRef,
                    schemaId: SchemaId,
                    kind: Kind,
                    main: Option[String],
                    plugin: Option[String],
                    repos: List[Repo],
                    checkouts: List[Checkout],
                    binaries: List[Path],
                    dependencies: List[TargetId],
                    compiler: Option[Target],
                    bloopSpec: Option[BloopSpec],
                    params: List[Parameter],
                    permissions: List[Permission],
                    intransitive: Boolean,
                    sourcePaths: List[Path],
                    environment: Map[String, String],
                    properties: Map[String, String]) {
  
    def id: TargetId = TargetId(schemaId, ref.projectId, ref.moduleId)
  }
  