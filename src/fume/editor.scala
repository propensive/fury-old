package editor

import org.scalajs.dom._, html._, ext.Ajax

import scala.scalajs.js
import scala.scalajs.js.annotation._

import scala.collection.immutable.SortedMap
import scala.annotation._

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

sealed trait Patch {
  def counter: Int
  def pos: Int
}
case class Insert(counter: Int, pos: Int, str: String) extends Patch
case class Delete(counter: Int, pos: Int, chars: Int) extends Patch

sealed trait Metadata

case class Style(name: String) extends Metadata
case class Tooltip(text: String) extends Metadata

object Editor {

  var counter = 0
  var current = ""
  var cursor = 0
  var events: Vector[Patch] = Vector()
  var lastSent: Long = 0L
  var lastMsg: String = ""

  def escape(str: String): String = str.replaceAll("\\\\", "\\\\\\\\").replaceAll("\"", "\\\\\"")

  def sendAjax(counter: Int, code: String): Unit = if(code != lastMsg) {
    if(System.currentTimeMillis - lastSent > 5000) {
      val msg = s"""{"id":$counter,"code":"${escape(code)}"}\n"""
      Ajax.post("http://localhost:8899/foo/bar", data = msg).map { result =>
        console.log(result)
      }
      lastSent = System.currentTimeMillis
      lastMsg = code
    }// else window.setTimeout(() => sendAjax(counter, code), 5100)
  }

  case class State(display: String, spans: List[(Int, Set[Change])])
  case class Context(nodes: List[Node], pos: Int, current: List[Metadata]) {
    def apply(change: Change): Context = Context(nodes, pos, change match {
      case Add(cls)     => Style(cls) :: current
      case Remove(cls)  => current.filterNot(_ == Style(cls))
      case Message(msg) => Tooltip(msg) :: current
    })
  }

  sealed trait Change
  case class Add(cls: String) extends Change
  case class Remove(cls: String) extends Change
  case class Message(text: String) extends Change

  def add(cls: String): Set[Change] = Set(Add(cls))
  def remove(cls: String): Set[Change] = Set(Remove(cls))
  def msg(text: String): Set[Change] = Set(Message(text))

  def handleResponse(msg: js.Dynamic): Unit = {
    val idx = msg.index.asInstanceOf[Int]
    
    events.dropWhile(_.counter <= idx).foldLeft(()) {
      case (cur, Insert(c, p, s)) => ()
      case (cur, Delete(c, p, s)) => ()
    }
  }

  def textNode(tagName: String, content: String, classes: List[Metadata]): Node = {
    val node = document.createElement(tagName)
    node.textContent = content
    classes.foreach {
      case Tooltip(msg) =>
        node.addEventListener("mouseover", showPanel(msg))
        node.addEventListener("mouseout", hidePanel(msg))
      case Style(cls) =>
        node.classList.add(cls)
    }
    node
  }

  @JSExportTopLevel("bind")
  def bind(e: js.Dynamic): Unit = {
    document.getElementById("area").addEventListener("keyup", change)
    document.getElementById("codeDiv").addEventListener("keyup", change)
  }

  def showPanel(msg: String)(e: Event): Unit = document.getElementById("panel").innerText = msg
  def hidePanel(msg: String)(e: Event): Unit = document.getElementById("panel").innerText = ""

  def change(e: js.Dynamic): Boolean = {
    val area = document.getElementById("area") match { case a: TextArea => a }
    val codeDiv = document.getElementById("codeDiv")

    val pos = e.target.selectionStart.asInstanceOf[Int]
    
    if(area.value.length > current.length) {
      val diff = area.value.length - current.length
      events :+= Insert(counter, cursor, area.value.substring(cursor, pos))
      counter += 1
    } else if(area.value.length < current.length) {
      events :+= Delete(counter, pos, current.length - area.value.length)
      counter += 1
    }

    if(current != area.value) {
      val parsed = parse(area.value)
      
      val context = parsed.spans.foldLeft(Context(Nil, 0, Nil)) {
        case (ctx, (end, classes)) =>
          val substring = parsed.display.substring(ctx.pos, end)
          val node = textNode("span", substring, ctx.current)
          val newClasses = classes.foldLeft(ctx) { (ctx, next) => ctx(next) }.current
          
          ctx.copy(nodes = ctx.nodes :+ node, pos = end, current = newClasses)
      }

      val lastNode = textNode("span", parsed.display.drop(context.pos), context.current)

      val ns = context.nodes
      
      (0 until codeDiv.childNodes.length).map(codeDiv.childNodes.item(_)).foreach(codeDiv.removeChild(_))

      ns.foreach(codeDiv.appendChild(_))
    }
    current = area.value
    cursor = pos
    sendAjax(counter, area.value)
    true
  }

  def parse(str: String): State = {
    def char(i: Int) = if(i >= str.length) '\u0000' else str(i)

    sealed trait Token
    case class Text(start: Int, end: Int) extends Token
    case class Comment(start: Int, start2: Int, end2: Int, end: Int) extends Token

    def parse() = parseText(0, 0, Nil).reverse

    @tailrec
    def parseText(i: Int, start: Int, xs: List[Token]): List[Token] = char(i) match {
      case '\u0000' => Text(start, i) :: xs
      case '/' => char(i + 1) match {
        case '*' => parseInlineComment(i + 2, i + 2, Text(start, i) :: xs)
        case '/' => parseLineComment(i + 2, i + 2, Text(start, i) :: xs)
        case ch  => parseText(i + 1, start, xs)
      }
      case ch  => parseText(i + 1, start, xs)
    }

    @tailrec
    def parseLineComment(i: Int, start: Int, xs: List[Token]): List[Token] = char(i) match {
      case '\u0000' => Comment(start - 2, start, i, i) :: xs
      case '\n' => parseText(i + 1, i + 1, Comment(start - 2, start, i, i + 1) :: xs)
      case ch   => parseLineComment(i + 1, start, xs)
    }

    @tailrec
    def parseInlineComment(i: Int, start: Int, xs: List[Token]): List[Token] = char(i) match {
      case '\u0000' => Comment(start - 2, start, i, i + 1) :: xs
      case '*' => char(i + 1) match {
        case '/' => parseText(i + 2, i + 2, Comment(start - 2, start, i, i + 2) :: xs)
        case ch  => parseInlineComment(i + 1, start, xs)
      }
      case ch  => parseInlineComment(i + 1, start, xs)
    }

    val Regex = """\?\[([^\]]*)\]\(""".r

    val map: List[(Int, Set[Change])] = parse().flatMap {
      case Text(s, e) =>
        List(s -> Set[Change](Add("text")), e -> Set[Change](Remove("text")))
      case Comment(s, s2, e2, e) => str.substring(s2, e2).trim match {
        case "("  => List((s, add("hidden")))
        case ")"  => List((e, remove("hidden")))
        case "!(" => List((s, add("hidden")),
                          (e, remove("hidden") ++ add("attention")))
        case ")!" => List((s, remove("attention") ++ add("hidden")),
                          (e, remove("hidden")))
        case Regex(text) =>
                     List((s, add("hidden")),
                          (e, remove("hidden") ++ add("info") ++ msg(text)))
        case ")?" => List((s, remove("info") ++ add("hidden")),
                          (e, remove("hidden")))
        case _    => List((s, add("gray")),
                                (s2, remove("gray") ++ add("comment")),
                                (e2, remove("comment") ++ add("gray")),
                          (e, remove("gray")))
      }
    }

    State(str, map)
  }
}