package fury

import scala.util._
import scala.collection._
import scala.concurrent._

object Coursier {

  implicit val ec: ExecutionContext = ExecutionContext.global

  private val cache: mutable.HashMap[Binary, Future[List[Path]]] = mutable.HashMap()

  def fetch(io: Io, binary: Binary): Future[List[Path]] =
    cache.getOrElseUpdate(
        binary, {
          io.println(msg"Resolving $binary")
          coursier
            .Fetch()
            .addDependencies(
                coursier.Dependency(
                    coursier.Module(
                        coursier.Organization(binary.group),
                        coursier.ModuleName(binary.artifact)),
                    binary.version))
            .future
            .map(_._2.to[List].map { a =>
              Path(a._2.getAbsolutePath)
            })
        }
    )

}
