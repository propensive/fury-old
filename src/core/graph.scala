/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.7.5. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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
  case object Executing                          extends CompileState

  private case class GraphState(changed: Boolean,
                        log: Log,
                        graph: Map[ModuleRef, Set[ModuleRef]],
                        stream: Iterator[CompileEvent],
                        compilationLogs: Map[ModuleRef, CompilationInfo]) {

    def updateCompilationLog(ref: ModuleRef, f: CompilationInfo => CompilationInfo): GraphState = {
      val previousState = compilationLogs.getOrElse(ref, CompilationInfo(state = Compiling(0), messages = List.empty))
      this.copy(compilationLogs = compilationLogs.updated(ref, f(previousState)))
    }
  }

  @tailrec
  private def live(graphState: GraphState)(implicit theme: Theme): Unit = {
    import graphState._

    log.print(Ansi.hideCursor())
    if (stream.hasNext) {
      val newState = stream.next match {
        case Tick =>
          val next: String = draw(graph, false, compilationLogs).mkString("\n")
          if(changed || compilationLogs.exists(_._2.state == Executing)) {
            log.println(next)
            log.println(Ansi.up(graph.size + 1)())
          }
          graphState.copy(changed = false)

        case CompilationProgress(ref, progress) =>
          graphState.updateCompilationLog(ref, _.copy(state = Compiling(progress))).copy(changed = true)
        case StartCompile(ref) =>
          graphState.updateCompilationLog(ref, _.copy(state = Compiling(0))).copy(changed = true)
        case DiagnosticMsg(ref, msg) =>
          graphState.updateCompilationLog(ref, Lens[CompilationInfo](_.messages).modify(_)(_ :+ msg)).copy(changed = false)
        case NoCompile(ref) =>
          val newState = compilationLogs.getOrElse(ref, CompilationInfo(state = AlreadyCompiled, messages = List.empty))
          graphState.updateCompilationLog(ref, _ => newState).copy(changed = true)
        case StopCompile(ref, success) =>
          val msgs = compilationLogs(ref).messages
          val newState = CompilationInfo(if (success) Successful(None) else Failed(""), msgs)
          graphState.updateCompilationLog(ref, _ => newState).copy(changed = true)
        case Print(ref, line) =>
          graphState.updateCompilationLog(ref, Lens[CompilationInfo](_.messages).modify(_)(_ :+ OtherMessage(line))).copy(changed = false)
        case StopRun(ref) =>
          graphState.updateCompilationLog(ref, _.copy(state = Successful(None))).copy(changed = true)
        case StartRun(ref) => graphState.updateCompilationLog(ref, _.copy(state = Executing)).copy(changed = true)
        case SkipCompile(ref) =>
          graphState.updateCompilationLog(ref, _.copy(state = Skipped)).copy(changed = true)
      }
      live(newState)
    } else {
      log.print(Ansi.showCursor())
      val output = compilationLogs.collect {
        case (_, CompilationInfo(Failed(_), out)) => out.map(_.msg)
        case (_, CompilationInfo(Successful(_), out)) => out.map(_.msg)
      }.flatten

      log.println(Ansi.down(graph.size + 1)())
      
      compilationLogs.foreach { case (ref, info) =>
        info match {
          case CompilationInfo(Failed(_) | Successful(_), out) if !out.isEmpty =>
            log.info(UserMsg { theme =>
              List(
                msg"Output from ",
                msg"${ref.projectId}",
                msg"${'/'}",
                msg"${ref.moduleId}"
              ).map { msg => theme.underline(theme.bold(msg.string(theme))) }.mkString
            })
            out.foreach { msg => log.info(msg.msg) }
          case _ => ()
        }
      }
    }
  }


  def live(log: Log,
           graph: Map[ModuleRef, Set[ModuleRef]],
           stream: Iterator[CompileEvent])
          (implicit theme: Theme)
          : Unit = {
    live(GraphState(changed = true, log, graph, stream, Map()))
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
          case Some(CompilationInfo(Executing, msgs)) =>
            val p = ((System.currentTimeMillis/50)%10).toInt
            theme.active((" "*p)+"■"+(" "*(9 - p)))
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
