/*
  Fury, version 0.1.2. Copyright 2018 Jon Pretty, Propensive Ltd.

  The primary distribution site is: https://propensive.com/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required  by applicable  law or  agreed to  in writing,  software  distributed  under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
  express  or  implied.  See  the  License for  the specific  language  governing  permissions and
  limitations under the License.
                                                                                                  */
package fury

import annotation.tailrec

object Graph {
  private final val North = 8
  private final val East = 4
  private final val South = 2
  private final val West = 1
  private final val chars = "   ┐ ─┌┬ ┘│┤└┴├┼".toCharArray

  sealed trait CompileState
  case object Compiling extends CompileState
  case object Successful extends CompileState
  case class Failed(output: String) extends CompileState
  case object Skipped extends CompileState

  @tailrec
  def live(cli: Cli[_],
           changed: Boolean = false)
          (io: cli.Io,
           graph: Map[ModuleRef, Set[ModuleRef]],
           stream: Stream[CompileEvent],
           state: Map[ModuleRef, CompileState])
          (implicit theme: Theme)
          : Unit =
    stream match {
      case Tick #:: tail =>
        val next: String = draw(graph, false, state).mkString("\n")
        if(changed) {
          io.println(next)
          io.println(Ansi.up(graph.size + 1)())
        }
        live(cli, false)(io, graph, tail, state)
      case StartCompile(ref) #:: tail =>
        live(cli, true)(io, graph, tail, state.updated(ref, Compiling))
      case StopCompile(ref, out, success) #:: tail =>
        live(cli, true)(io, graph, tail, state.updated(ref, if(success) Successful else Failed(out)))
      case SkipCompile(ref) #:: tail =>
        live(cli, true)(io, graph, tail, state.updated(ref, Skipped))
      case Stream.Empty =>
        val output = state.collect { case (ref, Failed(out)) =>
          UserMsg { theme => (msg"Output from $ref:".string(theme)+"\n\n"+out) }
        }.foldLeft(msg"")(_ + _)
        io.println(Ansi.down(graph.size + 1)())
        io.println(output)
    }

  def draw(graph: Map[ModuleRef, Set[ModuleRef]],
           describe: Boolean,
           state: Map[ModuleRef, CompileState] = Map())
          (implicit theme: Theme): Vector[String] = {
    def sort(todo: Map[ModuleRef, Set[ModuleRef]], done: List[ModuleRef]): List[ModuleRef] =
      if(todo.isEmpty) done
      else {
        val node = todo.find { case (k, v) => (v -- done).isEmpty }.get._1
        sort((todo - node).mapValues(_.filter(_ != node)), node :: done )
      }

    val nodes: List[(ModuleRef, Int)] = sort(graph, Nil).reverse.zipWithIndex

    val array: Array[Array[Char]] =
      Array.range(0, nodes.length).map { len => Array.fill[Char](len*2 + 2)(' ') }

    val indexes = nodes.toMap

    def overwrite(x: Int, y: Int, ch: Char) = array(y)(x) =
      if(ch == (North | South) && array(y)(x) == chars(East | West)) chars(North | South)
      else chars(chars.indexOf(array(y)(x)) | ch)

    nodes.foreach { case (node, i) =>
      array(i)(2*i + 1) = '»'
      graph(node).foreach { dep =>
        overwrite(2*indexes(dep) + 1, i, East | North)
        for(j <- (2*indexes(dep) + 1) to (2*i - 1)) overwrite(j + 1, i, East | West)
        for(j <- (indexes(dep) + 1) to (i - 1)) overwrite(2*indexes(dep) + 1, j, North | South)
      }
    }

    // compact the tree
    // FIXME: This is not tail-recursive, and doesn't seem to do compaction in some cases
    def compact(): Unit = array.zipWithIndex.foreach { case (line, idx) =>
      val max = line.dropRight(1).tails.find { t =>
        t.forall { ch => (chars.indexOf(ch) & North) == 0 }
      }.get.length
      
      if(max > 1) {
        val i = line.length - max - 1
        for(ln <- idx until array.length) array(ln)(i) = '.'
        compact()
      }
    }

    //compact()

    val namedLines = array.zip(nodes).map { case (chs, (moduleRef, _)) =>
      val ModuleRef(ProjectId(p), ModuleId(m), _) = moduleRef
      val text =
        if(describe || state.get(moduleRef) == Some(Compiling)) theme.project(p)+theme.gray("/")+theme.module(m)
        else theme.projectDark(p)+theme.gray("/")+theme.moduleDark(m)
      
      val errors = state.get(moduleRef) match {
        case Some(Failed(_)) => theme.failure("!")
        case Some(Successful) => theme.success("*")
        case _ => theme.bold(theme.failure(" "))
      }

      (msg"${chs.filter(_ != '.').mkString} ${if(state.get(moduleRef) == Some(Skipped)) theme.strike(text) else text} ", if(describe) msg"   " else msg" ${theme.gray("[")}$errors${theme.gray("]")}", p.length+m.length + 4)
    }
    
    val maxStrippedLength = namedLines.zipWithIndex.map { case ((_, _, len), idx) => len + idx*2 }.max + 4

    namedLines.zipWithIndex.map { case ((n, s, len), idx) =>
      val paddingLength = maxStrippedLength - len - idx*2
      n.string(theme)+(" "*(paddingLength%4))+theme.gray((if(idx%2 != 0 || describe) "    " else "  . ")*(paddingLength/4))+s.string(theme)
    }.to[Vector]
  }
}

object Diamond {

  def draw(topLeft: UserMsg, topRight: UserMsg, left: UserMsg, right: UserMsg, bottom: UserMsg): List[UserMsg] = {
    def width = left.length max (topLeft.length - 2)
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
