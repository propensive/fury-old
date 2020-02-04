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

import java.io._

class RecoveringFileOutputStream(path: Path, append: Boolean = false) extends OutputStream {

  private[this] var underlying: FileOutputStream = createStream()

  private[this] def recoverIfMissing(write: => Unit): Unit = {
    if(!path.exists()){
      path.extantParents()
      path.touch()
      synchronized{
        underlying.close()
        underlying = createStream()
      }
    }
    write
  }

  private[this] def createStream(): FileOutputStream = {
    new FileOutputStream(path.javaFile, append)
  }

  override def write(i: Int): Unit = recoverIfMissing(underlying.write(i))

  override def write(b: Array[Byte]): Unit = {
    recoverIfMissing(underlying.write(b))
  }

  override def write(b: Array[Byte], off: Int, len: Int): Unit = {
    recoverIfMissing(underlying.write(b, off, len))
  }

  override def close(): Unit = underlying.close()
}
