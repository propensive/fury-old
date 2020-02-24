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

import fury.io._, fury.strings._, fury.model._

import mercator._
import euphemism._

import scala.util._

object NodeJs {
  def clean(layout: Layout): Try[Boolean] =
    layout.npmFile.ifExists().fold[Try[Boolean]](Success(false))(_.delete())

  def generateFiles(compilation: Compilation, layout: Layout)(implicit log: Log): Try[Iterable[Path]] =
    new CollOps(compilation.targets.values.map { target =>
      for {
        path       <- layout.npmFile.mkParents()
        jsonString <- makeConfig(target, compilation, layout)
        _          <- ~path.writeSync(jsonString)
      } yield List(path)
    }).sequence.map(_.flatten)

  private def makeConfig(target: Target, compilation: Compilation, layout: Layout)(implicit log: Log): Try[String] = {
    val classpath = compilation.classpath(target.ref, layout)
    val compiler = ModuleRef.NodeJsRef

    val result = Json.of(
      name = target.ref.moduleId.key,
      version = "0.0.1",
      description = "",
      main = target.main.toString,
      scripts = Json.of(
        test = """"echo \"Error: no test specified\" && exit 1""""
      ),
      files = target.sourcePaths.map(_.value),
      keywords = List.empty[String],
      author = "",
      license = "ISC"
    )
    Success(result.toString)
  }
}
