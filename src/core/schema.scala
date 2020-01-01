package fury.core

import fury.model._, fury.strings._

import mercator._

import scala.collection.immutable._
import scala.util._
import scala.annotation.tailrec

object Schema {
  implicit val msgShow: MsgShow[Schema] = v => UserMsg(_.schema(v.id.key))
  implicit val stringShow: StringShow[Schema] = _.id.key
  implicit def diff: Diff[Schema] = Diff.gen[Schema]
}

case class Schema(id: SchemaId,
                  projects: SortedSet[Project] = TreeSet(),
                  repos: SortedSet[SourceRepo] = TreeSet(),
                  imports: SortedSet[SchemaRef] = TreeSet(),
                  main: Option[ProjectId] = None) {

  def apply(id: ProjectId) = projects.findBy(id)
  def repo(repoId: RepoId, layout: Layout): Try[SourceRepo] = repos.findBy(repoId)
  def moduleRefs: SortedSet[ModuleRef] = projects.flatMap(_.moduleRefs)
  def mainProject: Try[Option[Project]] = main.map(projects.findBy(_)).to[List].sequence.map(_.headOption)
  def sourceRepoIds: SortedSet[RepoId] = repos.map(_.id)
  def duplicate(id: String) = copy(id = SchemaId(id))

  def compilerRefs(layout: Layout, https: Boolean)(implicit log: Log): List[ModuleRef] =
    allProjects(layout, https).toOption.to[List].flatMap(_.flatMap(_.compilerRefs))

  def importCandidates(layout: Layout, https: Boolean)(implicit log: Log): List[String] =
    repos.to[List].flatMap(_.importCandidates(this, layout, https).toOption.to[List].flatten)

  def hierarchy(layout: Layout)(implicit log: Log): Try[Hierarchy] = for {
    imps <- imports.map { ref => for {
      layer        <- Layer.read(ref.layerRef, layout)
      resolved     <- layer.schemas.findBy(ref.schema)
      tree         <- resolved.hierarchy(layout)
    } yield tree }.sequence
  } yield Hierarchy(this, imps)

  def resolvedImports(layout: Layout, https: Boolean)(implicit log: Log): Try[Map[ImportId, Schema]] =
    imports.to[List].map { sr => resolve(sr, layout, https).map(sr.id -> _) }.sequence.map(_.toMap)

  def importedSchemas(layout: Layout, https: Boolean)(implicit log: Log): Try[List[Schema]] =
    resolvedImports(layout, https).map(_.values.to[List])
  
  def importTree(layout: Layout, https: Boolean)(implicit log: Log): Try[List[ImportPath]] = for {
    imports    <- resolvedImports(layout, https)
    importList <- imports.to[List].map { case (id, schema) =>
                    schema.importTree(layout, https).map { is => is.map(_.prefix(id)) }
                  }.sequence.map(_.flatten)
  } yield (ImportPath.Root :: importList)

  def allProjects(layout: Layout, https: Boolean)(implicit log: Log): Try[List[Project]] = {
    @tailrec
    def flatten[T](treeNodes: List[T])(aggregated: List[T], getChildren: T => Try[List[T]]): Try[List[T]] = {
      treeNodes match {
        case Nil => ~aggregated
        case head :: tail =>
          getChildren(head) match {
            case Success(ch) => flatten(ch ::: tail)(head :: aggregated, getChildren)
            case fail => fail
          }
      }
    }

    for {
      allSchemas <- flatten(List(this))(Nil, _.importedSchemas(layout, https))
    } yield allSchemas.flatMap(_.projects)
  }

  def unused(projectId: ProjectId): Try[ProjectId] = projects.find(_.id == projectId) match {
    case None    => Success(projectId)
    case Some(m) => Failure(ProjectAlreadyExists(m.id))
  }
  
  def resolve(ref: SchemaRef, layout: Layout, https: Boolean)(implicit log: Log): Try[Schema] = for {
    layer    <- Layer.read(ref.layerRef, layout)
    resolved <- layer.schemas.findBy(ref.schema)
  } yield resolved

  def localRepo(layout: Layout): Try[SourceRepo] = for {
    repo   <- Repo.local(layout)
    commit <- Shell(layout.env).git.getCommit(layout.baseDir).map(Commit(_))
    branch <- Shell(layout.env).git.getBranch(layout.baseDir).map(RefSpec(_))
  } yield SourceRepo(RepoId("~"), repo, branch, commit, Some(layout.baseDir))

  def allRepos(layout: Layout): SortedSet[SourceRepo] =
    (localRepo(layout).toOption.to[SortedSet].filterNot { r =>
      repos.map(_.repo.simplified).contains(r.repo.simplified)
    }) ++ repos
}
