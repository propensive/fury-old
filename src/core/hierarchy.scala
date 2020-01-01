package fury.core

import fury.model._

import scala.util._

case class Hierarchy(schema: Schema, inherited: Set[Hierarchy]) {
  lazy val universe: Try[Universe] = {
    val localProjectIds = schema.projects.map(_.id)

    def merge(universe: Try[Universe], hierarchy: Hierarchy) = for {
      projects             <- universe
      nextProjects         <- hierarchy.universe
      potentialConflictIds  = (projects.ids -- localProjectIds).intersect(nextProjects.ids)

      conflictIds           = potentialConflictIds.filter { id =>
                                projects.entity(id).map(_.spec) != nextProjects.entity(id).map(_.spec)
                              }

      allProjects          <- conflictIds match {
                                case x if x.isEmpty => Success(projects ++ nextProjects)
                                case _ => Failure(ProjectConflict(conflictIds/*, h1 = this, h2 = hierarchy*/))
                              }
    } yield allProjects

    val empty: Try[Universe] = Success(Universe())

    for(allInherited <- inherited.foldLeft(empty)(merge)) yield {
      val schemaEntities = schema.projects.map { project => project.id -> Entity(project, schema) }
      allInherited ++ Universe(schemaEntities.toMap)
    }
  }
}
