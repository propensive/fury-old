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
package fury.core

import fury.strings._, fury.io._, fury.model._

import annotation.tailrec
import optometry.Lens

object UiGraph {
  final private val North = 8
  final private val East  = 4
  final private val South = 2
  final private val West  = 1
  final private val chars = "   ┐ ─┌┬ ┘│┤└┴├┼".toCharArray

  sealed trait DiagnosticMessage { def msg: UserMsg }
  case class CompilerDiagnostic(msg: UserMsg, repo: RepoId, path: Path, line: LineNo, charNum: Int)
      extends DiagnosticMessage
  case class OtherMessage(msg: UserMsg) extends DiagnosticMessage

  case class CompilationInfo(state: CompileState, msgs: List[DiagnosticMessage]) {
    def failed(): CompilationInfo = CompilationInfo(Failed, msgs)
    
    def progress(n: Double): CompilationInfo =
      if(state == Failed) this else CompilationInfo(Compiling(n), msgs)
    
    def skipped: CompilationInfo =
      if(state == Failed) this else CompilationInfo(Skipped, msgs)
    
    def successful: CompilationInfo =
      if(state == Failed) this else CompilationInfo(Successful(None), msgs)
  }

  object Key {
    implicit def apply[T: MsgShow: StringShow](v: T): Key = apply(v, false)
    def apply[T: MsgShow: StringShow](v: T, hide: Boolean = false): Key = new Key {
      protected type Type = T
      protected val value: Type = v
      protected val show: StringShow[Type] = implicitly
      protected val msgShow: MsgShow[Type] = implicitly
      val hidden = hide
    }
  }

  trait Key {
    protected type Type
    protected val value: Type
    protected val show: StringShow[Type]
    protected val msgShow: MsgShow[Type]
    def userMsg: UserMsg = msgShow.show(value)
    def string: String = show.show(value)
    def hidden: Boolean

    override def equals(that: Any): Boolean = that match {
      case that: Key => that.value == value
      case _              => false
    }
    
    override def hashCode: Int = value.hashCode
  }

  sealed abstract class CompileState(val completion: Double)
  case class Compiling(progress: Double)         extends CompileState(progress)
  case object AlreadyCompiled                    extends CompileState(1.0)
  case class Successful(content: Option[String]) extends CompileState(1.0)
  case object Failed                             extends CompileState(0.0)
  case object Skipped                            extends CompileState(1.0)
  case object Executing                          extends CompileState(1.0)

  private case class GraphState(changed: Boolean,
                        graph: Map[Key, Set[Key]],
                        stream: Iterator[CompileEvent],
                        compilationLogs: Map[Key, CompilationInfo])(implicit log: Log) {

    def updateCompilationLog(ref: Key, f: CompilationInfo => CompilationInfo): GraphState = {
      val previousState = compilationLogs.getOrElse(ref, CompilationInfo(state = Compiling(0), msgs = Nil))
      this.copy(compilationLogs = compilationLogs.updated(ref, f(previousState)), changed = true)
    }
  }

  @tailrec
  private def live(graphState: GraphState)(implicit log: Log, theme: Theme): Unit = {
    import graphState._

    log.raw(Ansi.hideCursor())
    if (stream.hasNext) {
      val newState = stream.next match {
        case Tick =>
          val next: String = draw(graph, false, compilationLogs).mkString("\n")
          if(changed || compilationLogs.exists(_._2.state == Executing)) {
            log.raw(next)
            log.raw("\n")
            log.raw(Ansi.up(graph.size + 1)())
            log.raw("\n")
            
            val completion =
              graphState.compilationLogs.values.map(_.state.completion).sum/graphState.compilationLogs.size
            
            val current = graphState.compilationLogs.collect { case (r, s) if s.state.isInstanceOf[Compiling] =>
                str"${r.string}" }.to[List].sorted.join(", ")

            log.raw { Ansi.title {
              if(!current.isEmpty) str"Fury: Building ${(completion*100).toInt}% ($current)" else str"Fury"
            } }

            log.flush()
          }
          graphState.copy(changed = false)

        case CompilationProgress(ref, progress) =>
          graphState.updateCompilationLog(ref, _.progress(progress))
        case StartCompile(ref) =>
          graphState.updateCompilationLog(ref, _.progress(0))
        case DiagnosticMsg(ref, msg) =>
          graphState.updateCompilationLog(ref, Lens[CompilationInfo](_.msgs).modify(_)(_ :+ msg)).copy(changed =
              false)
        case NoCompile(ref) =>
          val newState = compilationLogs.getOrElse(ref, CompilationInfo(state = AlreadyCompiled, msgs =
              List.empty))

          graphState.updateCompilationLog(ref, newState.waive)
        case StopCompile(ref, success) =>
          val msgs = compilationLogs(ref).msgs
          val newState = CompilationInfo(if(success) Successful(None) else Failed, msgs)
          graphState.updateCompilationLog(ref, newState.waive)
        case Print(ref, line) =>
          log.info(msg"$line")
          graphState
        case StopRun(ref) =>
          graphState.updateCompilationLog(ref, _.successful)
        case StartRun(ref) =>
          graphState.updateCompilationLog(ref, _.copy(state = Executing))
        case SkipCompile(ref) =>
          graphState.updateCompilationLog(ref, _.skipped).copy(changed = true)
      }
      live(newState)
    } else {
      log.raw(Ansi.showCursor())
      val output = compilationLogs.collect {
        case (_, CompilationInfo(Failed, out)) => out.map(_.msg)
        case (_, CompilationInfo(Successful(_), out)) => out.map(_.msg)
      }.flatten

      log.raw(Ansi.down(graph.size + 2)())
      
      compilationLogs.foreach { case (ref, info) =>
        info match {
          case CompilationInfo(Failed | Successful(_), out) if !out.isEmpty =>
            log.info(UserMsg { theme =>
              List(
                msg"Output from ${ref.userMsg}",
              ).map { msg => theme.underline(theme.bold(msg.string(theme))) }.mkString
            })
            out.foreach { msg => log.info(msg.msg) }
          case _ => ()
        }
      }
    }
  }


  def live(graph: Map[Key, Set[Key]],
           stream: Iterator[CompileEvent])
          (implicit log: Log, theme: Theme)
          : Unit = {
    live(GraphState(changed = true, graph, stream, Map()))
  }

  def draw(graph: Map[Key, Set[Key]],
           describe: Boolean,
           state: Map[Key, CompilationInfo] = Map())
          (implicit theme: Theme)
          : Vector[String] = {
    def sort(todo: Map[Key, Set[Key]], done: List[Key]): List[Key] =
      if(todo.isEmpty) done else {
        val node = todo.find { case (k, v) => (v -- done).isEmpty }.get._1
        sort((todo - node).mapValues(_.filter(_ != node)), node :: done)
      }

    val nodes: List[(Key, Int)] = sort(graph, Nil).reverse.zipWithIndex

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
      case (chs, (key, _)) =>
        val text: UserMsg = key.userMsg

        val errors = state.get(key) match {
          case Some(CompilationInfo(Failed, msgs)) =>
            val count = str"${msgs.size}"
            theme.failure(str"${"■"*((9 - count.length)/2)} ${count} ${"■"*((8 - count.length)/2)}")
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
        val name = if(state.get(key) == Some(Skipped)) theme.strike(text.string(theme)) else text.string(theme)
        val errorsAnsi = if(describe) msg"   " else msg" ${'['}$errors${']'}"
        (msg"${chs.filter(_ != '.').mkString} $name ", errorsAnsi, text.length + 3)
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
