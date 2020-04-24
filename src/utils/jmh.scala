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
package fury.utils

import fury.io._

import org.openjdk.jmh._
import generators.bytecode._

import scala.util._
import java.io._

object Jmh {
  def instrument(bytecode: Path, sources: Path, resources: Path): Try[Unit] = {
    val discard: OutputStream = _ => ()
    val out = System.out
    System.setOut(new PrintStream(discard))
    val args = Array(bytecode.value, sources.value, resources.value, "reflection")
    val result = Try(JmhBytecodeGenerator.main(args))
    System.setOut(out)
    
    result
  }
}
