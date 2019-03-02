package fury

import scala.util._

object Coursier {

  def fetch(io: Io, org: String, artifact: String, version: String): Try[List[Path]] = {
    io.println(msg"Resolving $org:$artifact:$version")
    val resolution = coursier
      .Fetch()
      .addDependencies(
          coursier.Dependency(
              coursier.Module(coursier.Organization(org), coursier.ModuleName(artifact)),
              version))
      .run()
    Try(resolution._2.to[List].map { a =>
      Path(a._2.getAbsolutePath)
    })
  }
}
