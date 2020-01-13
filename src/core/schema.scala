/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.8.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.                                         ║
   ║                                                                                                           ║
   ║ The primary distribution site is: https://propensive.com/                                                 ║
   ║                                                                                                           ║
   ║ Licensed under  the Apache License,  Version 2.0 (the  "License"); you  may not use  this file  except in ║
   ║ compliance with the License. You may obtain a copy of the License at                                      ║
   ║                                                                                                           ║
   ║     http://www.apache.org/licenses/LICENSE-2.0                                                            ║
   ║                                                                                                           ║
   ║ Unless required  by applicable law  or agreed to in  writing, software  distributed under the  License is ║
   ║ distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. ║
   ║ See the License for the specific language governing permissions and limitations under the License.        ║
   ╚═══════════════════════════════════════════════════════════════════════════════════════════════════════════╝
*/
package fury.core

import fury.model._, fury.strings._

import mercator._

import scala.collection.immutable._
import scala.util._
import scala.annotation.tailrec

import language.higherKinds

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
  def mainProject: Try[Option[Project]] = main.to[List].traverse(projects.findBy(_)).map(_.headOption)
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

  def resolve(ref: SchemaRef, layout: Layout, https: Boolean)(implicit log: Log): Try[Schema] = for {
    layer    <- Layer.read(ref.layerRef, layout)
    resolved <- layer.schemas.findBy(ref.schema)
  } yield resolved

  def localRepo(layout: Layout): Try[SourceRepo] = for {
    repo   <- Repo.local(layout)
    commit <- Shell(layout.env).git.getCommit(layout.baseDir)
    branch <- Shell(layout.env).git.getBranch(layout.baseDir).map(RefSpec(_))
  } yield SourceRepo(RepoId("~"), repo, branch, commit, Some(layout.baseDir))

  def allRepos(layout: Layout): SortedSet[SourceRepo] =
    (localRepo(layout).toOption.to[SortedSet].filterNot { r =>
      repos.map(_.repo.simplified).contains(r.repo.simplified)
    }) ++ repos
}
