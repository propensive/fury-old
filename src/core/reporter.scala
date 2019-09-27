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
package fury.core

import fury.io._, fury.strings._, fury.model._, fury.utils._

object Reporter {
  val all: List[Reporter] = List(GraphReporter, LinearReporter(false, "linear"), LinearReporter(true, "stable"), QuietReporter)
  final private val reporters: Map[String, Reporter] = all.map { r => r.name -> r }.toMap
  def unapply(string: String): Option[Reporter] = reporters.get(string)
  implicit val stringShow: StringShow[Reporter] = _.name
}

abstract class Reporter(val name: String) {
  def report(io: Io,
             compilation: Compilation,
             theme: Theme,
             multiplexer: Multiplexer[ModuleRef, CompileEvent])
            : Unit
}

object GraphReporter extends Reporter("graph") {

  private def timeString(t: Long): String = if(t >= 10000) s"${t / 1000}s" else s"${t}ms"

  def report(io: Io,
             compilation: Compilation,
             theme: Theme,
             multiplexer: Multiplexer[ModuleRef, CompileEvent])
            : Unit = {
    val modules = compilation.graph.map { case (k, v) => (k.ref, v.to[Set].map(_.ref)) }
    io.println(msg"Starting build")
    Graph.live(io, modules, multiplexer.stream(50, Some(Tick)))(theme)
    io.println(msg"Build completed")
  }
}

case class LinearReporter(noTime: Boolean = false, reporterName: String) extends Reporter(reporterName) {
  def report(io: Io,
             compilation: Compilation,
             theme: Theme,
             multiplexer: Multiplexer[ModuleRef, CompileEvent])
            : Unit = {
    val interleaver = new Interleaver(io, 3000L)
    multiplexer.stream(50, Some(Tick)).foreach {
      case StartCompile(ref)                           => interleaver.println(ref, msg"Starting compilation of module $ref", noTime)
      case StopCompile(ref, true)                      => interleaver.println(ref, msg"Successfully compiled module $ref", noTime)
      case StopRun(ref)                                => interleaver.terminate(ref)
      case StartRun(ref)                               => ()
      case StopCompile(ref, false)                     => interleaver.println(ref, msg"Compilation of module $ref failed", noTime)
      case DiagnosticMsg(ref, Graph.OtherMessage(out)) => interleaver.println(ref, out, noTime)
      case Print(ref, line)                            => interleaver.println(ref, UserMsg { theme =>
                                                            theme.gray(escritoire.Ansi.strip(line))
                                                          }, noTime)
      case Tick                                        => interleaver.tick()
      case other                                       => ()
    }
  }
}

object QuietReporter extends Reporter("quiet") {

  def report(io: Io,
             compilation: Compilation,
             theme: Theme,
             multiplexer: Multiplexer[ModuleRef, CompileEvent])
            : Unit =
    multiplexer.stream(50, None).foreach { event => () }
}
