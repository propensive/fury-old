/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.6.4. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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

import fury.io._, fury.strings._, fury.ogdl._, fury.model._

import kaleidoscope._

import scala.annotation.tailrec
import scala.util._

import language.higherKinds

final class LayerRevisions(directory: Path, retained: Int) {

  def store(layer: Layer): Try[Unit] = {
    val revision = previousRevision match {
      case None           => 0
      case Some(previous) => previous.revision + 1
    }

    val path = directory / s"$revision.bak"

    for {
      _ <- Ogdl.write(layer, path)
      _ <- discardStaleRevisions()
    } yield ()
  }

  private def discardStaleRevisions(): Try[Unit] = {
    @tailrec
    def discard(revisions: Seq[LayerRevision]): Try[Unit] = revisions match {
      case Nil => Success(())
      case revision :: remaining =>
        revision.discard match {
          case Success(_) => discard(remaining)
          case failure    => failure
        }
    }

    val staleRevisions = revisions.drop(retained)
    discard(staleRevisions)
  }

  def discardPrevious(): Try[Unit] = previousRevision match {
    case None           => Success(())
    case Some(previous) => previous.discard
  }

  def previous(io: Io, layout: Layout): Try[Layer] = previousRevision match {
    case None           => Failure(NoPreviousRevision)
    case Some(previous) => previous.layer(io, layout)
  }

  private def revisions: Seq[LayerRevision] = {
    def parseRevision(path: String) = path match {
      case r"""${rev: String}@(\d+).bak""" => Some(rev.toLong)
      case _                               => None
    }

    val revisions = for {
      file <- directory.children
      rev  <- parseRevision(file)
    } yield new LayerRevision(rev, directory / file)

    revisions.sortWith(_.revision > _.revision)
  }

  private def previousRevision = revisions.headOption

  private class LayerRevision(val revision: Long, path: Path) {
    def layer(io: Io, layout: Layout): Try[Layer] = Layer.read(io, path, layout)
    def discard: Try[Unit]                        = path.delete().map(_ => ())
  }
}

case object NoPreviousRevision extends FuryException
