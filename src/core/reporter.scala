/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.7.14. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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
  val all: List[Reporter] = List(GraphReporter, InterleavingReporter, LinearReporter, QuietReporter)
  final private val reporters: Map[String, Reporter] = all.map { r => r.name -> r }.toMap
  def unapply(string: String): Option[Reporter] = reporters.get(string)
  implicit val stringShow: StringShow[Reporter] = _.name
}

abstract class Reporter(val name: String) {
  def report(graph: Target.Graph,
             theme: Theme,
             multiplexer: Multiplexer[ModuleRef, CompileEvent])(implicit log: Log)
            : Unit
}

object GraphReporter extends Reporter("graph") {

  private def timeString(t: Long): String = if(t >= 10000) s"${t / 1000}s" else s"${t}ms"

  def report(graph: Target.Graph,
             theme: Theme,
             multiplexer: Multiplexer[ModuleRef, CompileEvent])(implicit log: Log)
            : Unit = {
    val modules = graph.map { case (k, v) => (k.ref, v.map(_.ref)) }
    log.info(msg"Starting build")
    Graph.live(modules, multiplexer.stream(50, Some(Tick)))(log, theme)
    log.info(msg"Build completed")
  }
}

object LinearReporter extends Reporter("linear") {
  def report(graph: Target.Graph,
             theme: Theme,
             multiplexer: Multiplexer[ModuleRef, CompileEvent])(implicit log: Log)
            : Unit = {
    val interleaver = new Interleaver(3000L)
    multiplexer.stream(50, Some(Tick)).foreach {
      case StartCompile(ref)                           => log.info(msg"Starting compilation of module $ref")
      case StopCompile(ref, true)                      => log.info(msg"Successfully compiled module $ref")
      case StopCompile(ref, false)                     => log.info(msg"Compilation of module $ref failed")
      case DiagnosticMsg(ref, message)                 => log.info(message.msg)
      case Print(ref, line)                            => log.info(line)
      case other                                       => ()
    }
  }
}
object InterleavingReporter extends Reporter("interleaving") {
  def report(graph: Target.Graph,
             theme: Theme,
             multiplexer: Multiplexer[ModuleRef, CompileEvent])(implicit log: Log)
            : Unit = {
    val interleaver = new Interleaver(3000L)
    multiplexer.stream(50, Some(Tick)).foreach {
      case StartCompile(ref)                           => interleaver.println(ref, msg"Starting compilation of module $ref", false)
      case StopCompile(ref, true)                      => interleaver.println(ref, msg"Successfully compiled module $ref", false)
      case StopRun(ref)                                => interleaver.terminate(ref)
      case StartRun(ref)                               => ()
      case StopCompile(ref, false)                     => interleaver.println(ref, msg"Compilation of module $ref failed", false)
      case DiagnosticMsg(ref, message)                 => interleaver.println(ref, message.msg, false)
      case Print(ref, line)                            => interleaver.println(ref, UserMsg { theme =>
                                                            theme.gray(escritoire.Ansi.strip(line))
                                                          }, false)
      case Tick                                        => interleaver.tick()
      case other                                       => ()
    }
  }
}

object QuietReporter extends Reporter("quiet") {

  def report(graph: Target.Graph,
             theme: Theme,
             multiplexer: Multiplexer[ModuleRef, CompileEvent])(implicit log: Log)
            : Unit =
    multiplexer.stream(50, None).foreach { event => () }
}

case class TargetGraph(graph: Map[TargetId, List[TargetId]])
