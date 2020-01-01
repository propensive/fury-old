package fury.core

import fury.model._, fury.io._, fury.strings._, fury.ogdl._

import scala.util._

object SourceRepo {
    implicit val msgShow: MsgShow[SourceRepo] = r => UserMsg(_.repo(r.id.key))
    implicit val stringShow: StringShow[SourceRepo] = _.id.key
    implicit def diff: Diff[SourceRepo] = Diff.gen[SourceRepo]
  }
  
  case class SourceRepo(id: RepoId, repo: Repo, track: RefSpec, commit: Commit, local: Option[Path]) {
    def listFiles(layout: Layout, https: Boolean)(implicit log: Log): Try[List[Path]] = for {
      dir    <- localDir(layout).map(Success(_)).getOrElse(repo.get(layout, https))
      commit <- ~Shell(layout.env).git.getTag(dir, track.id).toOption.orElse(Shell(layout.env).git.
                    getBranchHead(dir, track.id).toOption).getOrElse(track.id)
      files  <- localDir(layout).map(Success(dir.children.map(Path(_))).waive).getOrElse(
                    Shell(layout.env).git.lsTree(dir, commit))
    } yield files
  
    def tracking(layout: Layout): Option[RefSpec] = localDir(layout).fold(Option(track)) { dir =>
      Shell(layout.env).git.getBranch(dir).toOption.map(RefSpec(_))
    }
  
    def fullCheckout(layout: Layout): Checkout = Checkout(id, repo, localDir(layout), commit, track, List())
  
    def localDir(layout: Layout): Option[Path] = local.orElse {
      Repo.local(layout).map(_.simplified == repo.simplified) match {
        case Success(true) => Some(layout.baseDir)
        case _             => None
      }
    }
  
    def changes(layout: Layout, https: Boolean)(implicit log: Log): Try[Option[String]] = for {
      repoDir <- localDir(layout).map(Success(_)).getOrElse(repo.fetch(layout, https))
      changes <- Shell(layout.env).git.diffShortStat(repoDir)
    } yield if(changes.isEmpty) None else Some(changes)
  
    def importCandidates(schema: Schema, layout: Layout, https: Boolean)(implicit log: Log): Try[List[String]] =
      for {
        repoDir     <- repo.get(layout, https)
  
        confString  <- Shell(layout.env).git.showFile(repoDir, ".fury.conf").orElse {
                         Shell(layout.env).git.showFile(repoDir, ".focus.fury")
                       }
  
        conf        <- ~Ogdl.read[FuryConf](confString, identity(_))
        layer       <- Layer.read(conf.layerRef, layout)
        schemas     <- ~layer.schemas.to[List]
      } yield schemas.map { schema => str"${id.key}:${schema.id.key}" }
  
    def pull(layout: Layout, https: Boolean)(implicit log: Log): Try[Commit] =
      repo.pull(commit, track, layout, https)
  
    def current(layout: Layout, https: Boolean)(implicit log: Log): Try[RefSpec] = for {
      dir    <- localDir(layout).map(Success(_)).getOrElse(repo.fetch(layout, https))
      commit <- Shell(layout.env).git.getCommit(dir)
    } yield RefSpec(commit)
  
    def sourceCandidates(layout: Layout, https: Boolean)(pred: String => Boolean)
                        (implicit log: Log)
                        : Try[Set[Source]] =
      listFiles(layout, https).map(_.filter { f => pred(f.filename) }.map { p =>
          ExternalSource(id, p.parent): Source }.to[Set])
  }