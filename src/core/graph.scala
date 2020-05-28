/*

    Fury, version 0.16.1. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

import fury.text._, fury.io._, fury.model._

import annotation.tailrec
import optometry.Lens

object UiGraph {
  final private val North = 8
  final private val East  = 4
  final private val South = 2
  final private val West  = 1
  final private val chars = "   ┐ ─┌┬ ┘│┤└┴├┼".toCharArray

  sealed trait Issue { def msg: UserMsg }
  case class CompileIssue(msg: UserMsg, repo: RepoId, path: Path, line: LineNo, charNum: Int) extends Issue
  case class BuildIssue(msg: UserMsg) extends Issue

  case class BuildInfo(state: BuildState, msgs: List[Issue]) {
    def failed(): BuildInfo = BuildInfo(Failed, msgs)
    def progress(n: Double): BuildInfo = if(state == Failed) this else BuildInfo(Compiling(n), msgs)
    def skipped: BuildInfo = if(state == Failed) this else BuildInfo(Skipped, msgs)
    def successful: BuildInfo = if(state == Failed) this else BuildInfo(Successful(None), msgs)
  }

  sealed abstract class BuildState(val completion: Double)
  case class Compiling(progress: Double) extends BuildState(progress)
  case object Finished extends BuildState(1.0)
  case class Successful(content: Option[String]) extends BuildState(1.0)
  case object Failed extends BuildState(0.0)
  case object Skipped extends BuildState(1.0)
  case object Executing extends BuildState(1.0)

  private case class GraphState(changed: Boolean,
                                graph: Map[ModuleRef, Set[Dependency]],
                                stream: Iterator[CompileEvent],
                                buildLogs: Map[ModuleRef, BuildInfo])
                               (implicit log: Log) {

    def update(ref: ModuleRef, f: BuildInfo => BuildInfo): GraphState = {
      val previousState = buildLogs.getOrElse(ref, BuildInfo(state = Compiling(0), msgs = Nil))
      this.copy(buildLogs = buildLogs.updated(ref, f(previousState)), changed = true)
    }
  }

  @tailrec
  private def live(graphState: GraphState)(implicit log: Log, theme: Theme): Unit = {
    import graphState._

    log.raw(Ansi.hideCursor())
    if (stream.hasNext) {
      val newState = stream.next match {
        case Tick =>
          val next: String = draw(graph, false, buildLogs).mkString("\n")
          if(changed || buildLogs.exists(_._2.state == Executing)) {
            log.raw(next)
            log.raw("\n")
            log.raw(Ansi.up(graph.size + 1)())
            log.raw("\n")
            
            val completion =
              graphState.buildLogs.values.map(_.state.completion).sum/graphState.buildLogs.size
            
            val current = graphState.buildLogs.collect { case (r, s) if s.state.isInstanceOf[Compiling] =>
                str"${r}" }.to[List].sorted.join(", ")

            log.raw { Ansi.title {
              if(!current.isEmpty) str"Fury: Building ${(completion*100).toInt}% ($current)" else str"Fury"
            } }

            log.flush()
          }
          graphState.copy(changed = false)

        case Progress(ref, progress) =>
          graphState(ref) = _.progress(progress)
        case StartCompile(ref) =>
          graphState(ref) = _.progress(0)
        case DiagnosticMsg(ref, msg) =>
          (graphState(ref) = Lens[BuildInfo](_.msgs).modify(_)(_ :+ msg)).copy(changed = false)
        case NoCompile(ref) =>

          graphState(ref) = buildLogs.getOrElse(ref, BuildInfo(state = Finished, msgs = List.empty)).waive
        case StopCompile(ref, success) =>
          graphState(ref) = BuildInfo(if(success) Successful(None) else Failed, buildLogs(ref).msgs).waive
        case Print(ref, line) =>
          log.info(msg"$line")
          graphState
        case StopRun(ref) =>
          graphState(ref) = _.successful
        case StartRun(ref) =>
          graphState(ref) = _.copy(state = Executing)
        case SkipCompile(ref) =>
          (graphState(ref) = _.skipped).copy(changed = true)
      }
      live(newState)
    } else {
      log.raw(Ansi.showCursor())
      val output = buildLogs.collect {
        case (_, BuildInfo(Failed, out)) => out.map(_.msg)
        case (_, BuildInfo(Successful(_), out)) => out.map(_.msg)
      }.flatten

      log.raw(Ansi.down(graph.size + 2)())
      
      buildLogs.foreach { case (ref, info) =>
        info match {
          case BuildInfo(Failed | Successful(_), out) if !out.isEmpty =>
            log.info(UserMsg { theme =>
              List(msg"Output from $ref").map { msg => theme.underline(theme.bold(msg.string(theme))) }.mkString
            })
            out.foreach { msg => log.info(msg.msg) }
          case _ => ()
        }
      }
    }
  }

  def live(graph: Map[ModuleRef, Set[Dependency]], stream: Iterator[CompileEvent])
          (implicit log: Log, theme: Theme)
          : Unit =
    live(GraphState(changed = true, graph, stream, Map()))

  def draw(graph: Map[ModuleRef, Set[Dependency]], describe: Boolean, state: Map[ModuleRef, BuildInfo] = Map())
          (implicit theme: Theme)
          : Vector[String] = {

    def sort(todo: Map[ModuleRef, Set[Dependency]], done: List[ModuleRef]): List[ModuleRef] =
      if(todo.isEmpty) done else {
        val node = todo.find { case (k, v) => (v.map(_.ref) -- done).isEmpty }.get._1
        sort((todo - node).mapValues(_.filter(_.ref != node)), node :: done)
      }

    val nodes: List[(ModuleRef, Int)] = sort(graph, Nil).reverse.zipWithIndex

    val array: Array[Array[Char]] = Array.range(0, nodes.length).map { len => Array.fill[Char](len*2 + 2)(' ') }

    val indexes = nodes.toMap

    def overwrite(x: Int, y: Int, ch: Char) =
      array(y)(x) =
        if(ch == (North | South) && array(y)(x) == chars(East | West)) chars(North | South)
        else chars(chars.indexOf(array(y)(x)) | ch)

    nodes.foreach {
      case (node, i) =>
        array(i)(2*i + 1) = '»'
        graph(node).filter(!_.hidden).foreach { dep =>
          overwrite(2*indexes(dep.ref) + 1, i, East | North)
          for (j <- (2*indexes(dep.ref) + 1) to (2*i - 1)) overwrite(j + 1, i, East | West)
          for (j <- (indexes(dep.ref) + 1) to (i - 1)) overwrite(2*indexes(dep.ref) + 1, j, North | South)
        }
    }

    val namedLines = array.zip(nodes).map {
      case (chs, (key, _)) =>
        val text: UserMsg = msg"$key"

        val errors = state.get(key) match {
          case Some(BuildInfo(Failed, msgs)) =>
            val count = str"${msgs.size}"
            theme.failure(str"${"■"*((9 - count.length)/2)} ${count} ${"■"*((8 - count.length)/2)}")
          case Some(BuildInfo(Successful(_), msgs)) =>
            theme.success("■"*10)
          case Some(BuildInfo(Compiling(progress), msgs)) =>
            val p = (progress*10).toInt
            theme.ongoing("■"*p + " "*(10 - p))
          case Some(BuildInfo(Executing, msgs)) =>
            val p = ((System.currentTimeMillis/50)%10).toInt
            theme.active((" "*p)+"■"+(" "*(9 - p)))
          case Some(BuildInfo(Finished, msgs)) =>
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
