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
package fury.io

import java.io.{File, PrintStream}

import probably._

import scala.language.implicitConversions

object RecoveringFileOutputStreamTest extends TestApp {

  override def tests(): Unit = {
    test("File is restored after it has been deleted") {
      tmpFile{ file =>
        val out = new PrintStream(new RecoveringFileOutputStream(file))
        out.print("Hello file!")
        file.delete().get
        out.print("I'm back!")
        out.close()
        file.exists()
      }
    }.assert(_ == true)

    test("File is not restored if nothing is written to it") {
      tmpFile{ file =>
        val out = new PrintStream(new RecoveringFileOutputStream(file))
        out.print("Hello file!")
        file.delete().get
        out.close()
        file.exists()
      }
    }.assert(_ == false)

    test("File is restored after its parent directory has been deleted") {
      tmpFile{ dir =>
        dir.mkdir()
        val file = dir / "test.txt"
        val out = new PrintStream(new RecoveringFileOutputStream(file))
        out.print("Hello file!")
        dir.delete().get
        out.print("I'm back!")
        out.close()
        file.exists()
      }
    }.assert(_ == true)

    test("File contains only what was written after it has been deleted") {
      tmpFile{ file =>
        val out = new PrintStream(new RecoveringFileOutputStream(file))
        out.print("Hello file!")
        file.delete().get
        out.print("I'm back!")
        out.close()
        scala.io.Source.fromFile(file.javaFile).getLines().toList
      }
    }.assert(_ == List("I'm back!"))
  }

  private def tmpFile[T](fn: Path => T): T = {
    val file = Path(File.createTempFile("fury-test", "tmp"))
    file.mkParents()
    file.delete()
    val result = fn(file)
    file.delete()
    result
  }

}
