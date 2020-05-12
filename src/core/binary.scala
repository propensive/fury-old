/*

    Fury, version 0.15.1. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

import fury.model._, fury.strings._, fury.io._, fury.ogdl._

import exoskeleton._
import kaleidoscope._

import scala.collection.immutable._
import scala.util._

object Binary {
  implicit val msgShow: MsgShow[Binary] = v => UserMsg(_.binary(v.spec))
  implicit val stringShow: StringShow[Binary] = b => b.id.key
  implicit def diff: Diff[Binary] = Diff.gen[Binary]

  def apply(id: Option[BinaryId], service: BinRepoId, binSpec: BinSpec)(implicit log: Log): Try[Binary] ={
    val bin = binSpec.string.only {
      case r"$group@([\.\-_a-zA-Z0-9]*)\:$artifact@([\.\-_a-zA-Z0-9]*)\:$version@([\.\-\+_a-zA-Z0-9]*)" => 
        Binary(id.getOrElse(BinaryId(artifact)), service, group, artifact, version)
    }
    bin.ascribe(InvalidValue(binSpec.string)).flatMap(_.paths).map(_ => bin.get)
  }


  def apply(id: Option[BinaryId], cwd: Path, jar: JarFile)(implicit log: Log): Try[Binary] = { 
    val absPath = cwd.resolve(jar)
    val bin = absPath.input.only { case r"^.*\/(?!.*\/)$filename@(.*).jar" =>
      Binary(id.getOrElse(BinaryId(filename)), BinRepoId.Local, path = Some(absPath))
    }
    bin.ascribe(InvalidValue(absPath.input)).flatMap(_.paths).map(_ => bin.get)
  }

  private val compilerVersionCache: HashMap[Binary, Try[String]] = HashMap()

  val Jmh = Binary(BinaryId("jmh-core"), BinRepoId.Central, group = "org.openjdk.jmh", artifact = "jmh-core", version = "1.21")
  
  private[core] val NA = "-"
}

case class Binary(
  id: BinaryId, 
  binRepo: BinRepoId, 
  group: String = Binary.NA, 
  artifact: String = Binary.NA,
  version: String = Binary.NA,
  path: Option[Path] = None
) {
  def spec = str"$group:$artifact:$version"

  def paths(implicit log: Log): Try[List[Path]] = (binRepo, path) match {
        case (BinRepoId.Central, _) => Coursier.fetch(this).recoverWith {
              case OfflineException() => ~List[Path]()
        }.orElse(Failure(DownloadFailure(spec)))
        case (BinRepoId.Local, Some(p)) => if(p.exists) Success(List(p)) else Failure(UnresolvedBinaryFile(p))
        case _ => Failure(UnknownBinaryRepository(binRepo))
    }

}

