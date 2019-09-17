/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.6.7. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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
package fury.strings

import scala.reflect._
import scala.util._

trait FuryException extends Exception

object Outcome {

  class Rescue[E <: Exception](val error: E => FuryException) {

    def apply[T](fn: => T)(implicit classTag: ClassTag[E]): Try[T] =
      try Success(fn)
      catch { case e: E => Failure(error(e)) }
  }

  def rescue[E <: Exception](error: E => FuryException): Rescue[E] = new Rescue[E](error)

  def rescue[E <: Exception](error: FuryException): Rescue[E] =
    new Rescue[E]({ e: Exception =>
      error
    })
}
