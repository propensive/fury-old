/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.6.6. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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
package fury

import fury.io._
import java.nio.file.{Files, Path}
import fury.core._

import probably._

import fury.io._, fury.core._

/**

I have disabled these tests temporarily, because the use of the `version`
parameter in `Layer` is a bit confusing. Its purpose is to determine the
version of the file format, not the version of the build. When using the
`Layer.read` method to load these files, it should automatically upgrade
any version to the latest version, before parsing into case classes, and
using arbitrary version numbers in the Layer is likely to cause problems.

  */

object LayerRepositoryTest extends TestApp {
  private var currentLayer: Path               = _
  private var layerRepository: LayerRepository = _

  override def tests(): Unit = {
    /*
    test("modifies current layer on update") {
      init()

      layerRepository.update(Layer(version = 1))

      versionOf(currentLayer)
    }.assert(version => version == 1)

    test("does not change current layer if history is empty") {
      init()

      currentLayer.write(Layer(version = 1))
      layerRepository.restorePrevious()

      versionOf(currentLayer)
    }.assert(version => version == 1)

    test("restores previous layer on undo") {
      init()

      layerRepository.update(Layer(version = 1))
      layerRepository.update(Layer(version = 2))
      layerRepository.restorePrevious()

      versionOf(currentLayer)
    }.assert(version => version == 1)

    test("allows undoing more than one revision") {
      init()

      layerRepository.update(Layer(version = 1))
      layerRepository.update(Layer(version = 2))
      layerRepository.update(Layer(version = 3))
      layerRepository.restorePrevious()
      layerRepository.restorePrevious()

      versionOf(currentLayer)
    }.assert(version => version == 1)

    test("discard newer revisions when restoring") {
      init()

      layerRepository.update(Layer(version = 1))
      layerRepository.update(Layer(version = 2))
      layerRepository.restorePrevious()
      layerRepository.update(Layer(version = 3))
      layerRepository.restorePrevious()

      versionOf(currentLayer)
    }.assert(version => version == 1)

    test("cannot restore more revisions than are retained") {
      init(retainedRevisions = 1)

      layerRepository.update(Layer(version = 1))
      layerRepository.update(Layer(version = 2))
      layerRepository.update(Layer(version = 3))
      layerRepository.restorePrevious()
      layerRepository.restorePrevious()
      layerRepository.restorePrevious()

      versionOf(currentLayer)
    }.assert(version => version == 2)
   */
  }
  /*
  private def versionOf(currentLayer: Path) = currentLayer.read[Layer].map(_.version).get

  private def init(retainedRevisions: Int = Int.MaxValue) = {
    val revisionsDir = Path(Files.createTempDirectory("layer-repo").toString)
    val revisions    = new LayerRevisions(revisionsDir, retainedRevisions)

    currentLayer = Path(Files.createTempFile("layer", "fury").toString)
    layerRepository = new LayerRepository(revisions, currentLayer)
  }

 */
}
