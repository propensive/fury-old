package fury

import coursier._

object Coursier {

  def fetch(io: Io, artifact: String): Try[List[Path]] =
    Resolve().addDependencies(artifact).run()
}
