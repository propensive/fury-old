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
package fury.core

import fury.strings._, fury.io._, fury.model._

import annotation.tailrec
import optometry.Lens

object Graph {
  final private val North = 8
  final private val East  = 4
  final private val South = 2
  final private val West  = 1
  final private val chars = "   ┐ ─┌┬ ┘│┤└┴├┼".toCharArray

  sealed trait DiagnosticMessage { def msg: UserMsg }
  case class CompilerDiagnostic(msg: UserMsg, repo: RepoId, path: Path, line: LineNo, charNum: Int)
      extends DiagnosticMessage
  case class OtherMessage(msg: UserMsg) extends DiagnosticMessage

  case class CompilationInfo(state: CompileState, messages: List[DiagnosticMessage])

  sealed trait CompileState
  case class Compiling(progress: Double)         extends CompileState
  case object AlreadyCompiled                    extends CompileState
  case class Successful(content: Option[String]) extends CompileState
  case class Failed(output: String)              extends CompileState
  case object Skipped                            extends CompileState

  def updateValue[A, B](map: Map[A, B], ref: A, f: B => B, default: B): Map[A, B] =
    map.updated(ref, f(map.getOrElse(ref, default)))

  @tailrec
  def live(changed: Boolean,
           io: Io,
           graph: Map[ModuleRef, Set[ModuleRef]],
           stream: Stream[CompileEvent],
           state: Map[ModuleRef, CompilationInfo],
           streaming: Boolean)
          (implicit theme: Theme)
          : Unit = {

    def updateState(
        ref: ModuleRef,
        f: CompilationInfo => CompilationInfo
      ): Map[ModuleRef, CompilationInfo] =
      updateValue(state, ref, f, CompilationInfo(Compiling(0), List()))

    io.print(Ansi.hideCursor())

    stream match {
      case Tick #:: tail =>
        val next: String = draw(graph, false, state).mkString("\n")
        if(changed && !streaming) {
          io.println(next)
          io.println(Ansi.up(graph.size + 1)())
        }
        live(false, io, graph, tail, state, streaming)

      case CompilationProgress(ref, progress) #:: tail =>
        live(true, io, graph, tail, updateState(ref, _.copy(state = Compiling(progress))), streaming)
      case StartCompile(ref) #:: tail =>
        live(true, io, graph, tail, updateState(ref, _.copy(state = Compiling(0))), streaming)
      case DiagnosticMsg(ref, msg) #:: tail =>
        live(false, io, graph, tail, updateState(ref, Lens[CompilationInfo](_.messages).modify(_)(_ :+ msg)),
            streaming)
      case NoCompile(ref) #:: tail =>
        val newState = if(state.contains(ref)) state else updateState(ref, _.copy(state = AlreadyCompiled))
        live(true, io, graph, tail, newState, streaming)
      case StopCompile(ref, success) #:: tail =>
        val msgs = state(ref).messages
        val newState = state.updated(ref, CompilationInfo(if(success) Successful(None) else Failed(""),msgs))
        live(true, io, graph, tail, newState, streaming)
      case StartStreaming #:: tail =>
        io.println(Ansi.down(graph.size + 1)())
        live(true, io, graph, tail, state, true)
      case Print(line) #:: tail =>
        io.println(line)
        live(true, io, graph, tail, state, streaming)
      case StopStreaming #:: tail =>
        live(true, io, graph, tail, state, false)
      case SkipCompile(ref) #:: tail =>
        live(true, io, graph, tail, updateState(ref, _.copy(state = Skipped)), streaming)
      case Stream.Empty =>
        io.print(Ansi.showCursor())
        val output = state.collect {
          case (ref, CompilationInfo(Failed(_), out))     => out.map(_.msg)
          case (ref, CompilationInfo(Successful(_), out)) => out.map(_.msg)
        }.flatten
        io.println(Ansi.down(graph.size + 1)())
        output.foreach(io.println(_))
    }
  }

  def draw(graph: Map[ModuleRef, Set[ModuleRef]],
           describe: Boolean,
           state: Map[ModuleRef, CompilationInfo] = Map())
          (implicit theme: Theme)
          : Vector[String] = {
    def sort(todo: Map[ModuleRef, Set[ModuleRef]], done: List[ModuleRef]): List[ModuleRef] =
      if(todo.isEmpty) done else {
        val node = todo.find { case (k, v) => (v -- done).isEmpty }.get._1
        sort((todo - node).mapValues(_.filter(_ != node)), node :: done)
      }

    val nodes: List[(ModuleRef, Int)] = sort(graph, Nil).reverse.zipWithIndex

    val array: Array[Array[Char]] =
      Array.range(0, nodes.length).map { len =>
        Array.fill[Char](len*2 + 2)(' ')
      }

    val indexes = nodes.toMap

    def overwrite(x: Int, y: Int, ch: Char) =
      array(y)(x) =
        if(ch == (North | South) && array(y)(x) == chars(East | West)) chars(North | South)
        else chars(chars.indexOf(array(y)(x)) | ch)

    nodes.foreach {
      case (node, i) =>
        array(i)(2*i + 1) = '»'
        graph(node).filter(!_.hidden).foreach { dep =>
          overwrite(2*indexes(dep) + 1, i, East | North)
          for (j <- (2*indexes(dep) + 1) to (2*i - 1)) overwrite(j + 1, i, East | West)
          for (j <- (indexes(dep) + 1) to (i - 1)) overwrite(2*indexes(dep) + 1, j, North | South)
        }
    }

    val namedLines = array.zip(nodes).map {
      case (chs, (moduleRef, _)) =>
        val ModuleRef(ProjectId(p), ModuleId(m), _, hidden) = moduleRef
        val text =
          if(describe || state.get(moduleRef).exists(_.state.isInstanceOf[Compiling]))
            theme.project(p) + theme.gray("/") + theme.module(m)
          else theme.projectDark(p) + theme.gray("/") + theme.moduleDark(m)

        val errors = state.get(moduleRef) match {
          case Some(CompilationInfo(Failed(_), msgs)) =>
            val plural = if(msgs.length == 1) "" else "s"
            theme.failure(s"${msgs.size} error${plural}".padTo(10, ' '))
          case Some(CompilationInfo(Successful(_), msgs)) =>
            theme.success("■"*10)
          case Some(CompilationInfo(Compiling(progress), msgs)) =>
            val p = (progress*10).toInt
            theme.ongoing("■"*p + " "*(10 - p))
          case Some(CompilationInfo(AlreadyCompiled, msgs)) =>
            theme.gray("■"*10)
          case _ => theme.bold(theme.failure("          "))
        }
        val name = if(state.get(moduleRef) == Some(Skipped)) theme.strike(text) else text
        val errorsAnsi = if(describe) msg"   " else msg" ${theme.gray("[")}$errors${theme.gray("]")}"
        (msg"${chs.filter(_ != '.').mkString} $name ", errorsAnsi, p.length + m.length + 4)
    }

    val maxStrippedLength = namedLines.zipWithIndex.map { case ((_, _, len), idx) => len + idx*2 }.max + 4

    namedLines.zipWithIndex.map { case ((n, s, len), idx) =>
      val paddingLength = maxStrippedLength - len - idx*2
      n.string(theme) + (" "*(paddingLength%4)) + theme.gray(
          (if(idx%2 != 0 || describe) "    " else "  . ")*(paddingLength/4)) + s.string(theme)
    }.to[Vector]
  }
}

object Diamond {
  def draw(topLeft: UserMsg, topRight: UserMsg, left: UserMsg, right: UserMsg, bottom: UserMsg)
          : List[UserMsg] = {
    val width = left.length.max(topLeft.length - 2)
    val padding = " "*width

    List(
      msg"  ${" "*(width - topLeft.length + 2)}$topLeft ■ $topRight",
      msg"  ${" "*width} ┌─╨─┐",
      msg"  ${" "*(width - left.length)}$left ■   ■ $right",
      msg"  ${" "*width} └─┬─┘",
      msg"  ${" "*width}   ■ $bottom"
    )
  }
}
