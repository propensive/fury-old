/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.5.0. Copyright 2018-19 Jon Pretty, Propensive Ltd.                                        ║
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

import fury.io._, fury.strings._

object Reporter {
  val all: List[Reporter] = List(GraphReporter, LinearReporter, QuietReporter)
  final private val reporters: Map[String, Reporter] = all.map { r => r.name -> r }.toMap
  def unapply(string: String): Option[Reporter] = reporters.get(string)
  implicit val stringShow: StringShow[Reporter] = _.name
}

abstract class Reporter(val name: String) {
  def report(io: Io,
             compilation: Compilation,
             theme: Theme,
             multiplexer: Multiplexer[ModuleRef, CompileEvent],
             startTime: Long)
            : Unit
}

object GraphReporter extends Reporter("graph") {

  private def timeString(t: Long): String = if(t >= 10000) s"${t / 1000}s" else s"${t}ms"

  def report(io: Io,
             compilation: Compilation,
             theme: Theme,
             multiplexer: Multiplexer[ModuleRef, CompileEvent],
             startTime: Long)
            : Unit = {
    val modules = compilation.graph.map { case (k, v) => (k.ref, v.to[Set].map(_.ref)) }
    Graph.live(changed = true, io, modules, multiplexer.stream(50, Some(Tick)), Map(), false)(theme)
    val duration = System.currentTimeMillis - startTime
    io.println(msg"Total time: ${timeString(duration)}")
  }
}

object LinearReporter extends Reporter("linear") {
  def report(io: Io,
             compilation: Compilation,
             theme: Theme,
             multiplexer: Multiplexer[ModuleRef, CompileEvent],
             startTime: Long)
            : Unit =
    multiplexer.stream(50, None).foreach {
      case StartCompile(ref)                           => io.println(msg"Starting compilation of module $ref")
      case StopCompile(ref, true)                      => io.println(msg"Successfully compiled module $ref")
      case StopCompile(ref, false)                     => io.println(msg"Compilation of module $ref failed")
      case DiagnosticMsg(ref, Graph.OtherMessage(out)) => io.println(out)
      case other                                       => ()
    }
}

object QuietReporter extends Reporter("quiet") {

  def report(io: Io,
             compilation: Compilation,
             theme: Theme,
             multiplexer: Multiplexer[ModuleRef, CompileEvent],
             startTime: Long)
            : Unit =
    multiplexer.stream(50, None).foreach { event => () }
}
