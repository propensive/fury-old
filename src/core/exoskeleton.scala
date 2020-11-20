/*
  
  Exoskeleton, version 0.1.0. Copyright 2018 Jon Pretty, Propensive Ltd.

  The primary distribution site is: https://propensive.com/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use
  this file except in compliance with the License. You may obtain a copy of the
  License at
  
      http://www.apache.org/licenses/LICENSE-2.0
 
  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
  WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
  License for the specific language governing permissions and limitations under
  the License.

*/
package exoskeleton

import scala.util._
import scala.annotation._

case class ParamUsage(map: ParamMap, used: Set[String]) {
  def -(key: String): ParamUsage = copy(used = used + key)
  def --(keys: Set[String]): ParamUsage = copy(used = used ++ keys)
  def unexpected = map.groups.filterNot { p =>
    used contains p.key()
  }
}

case class Arg(value: String)

object :~ {
  def unapply(paramMap: ParamMap): Option[(Arg, ParamMap)] =
    paramMap.prefix.headOption.map { h =>
      (h, paramMap.tail)
    }
}

case class ParamMap(args: String*) {

  def ++(pm2: ParamMap) = ParamMap(pm2.args ++ args: _*)
  def +(arg: String) = ParamMap(args :+ arg: _*)
  def tail: ParamMap = ParamMap(args.tail: _*)
  def headOption: Option[String] = args.headOption

  case class Part(no: Int, start: Int, end: Int) {
    def apply() = args(no).substring(start, end)
    private[exoskeleton] def throwIfBad(): Part = {
      if(no < 0 || no >= args.length)
        throw new IllegalStateException(s"Expected a $no-th argument, but it doesn't exist.")
      else if(start < 0 || start > end || end > args(no).length)
        throw new IllegalStateException(s"Substring ($start, $end) is invalid for ${args(no)}.")
      else this
    }
  }

  case class Parameter(key: Part, values: Vector[Part] = Vector()) {
    override def toString = {
      val prefix = if (key().length == 1) "-" else "--"
      s"$prefix${key()} ${values.map(_ ()) mkString " "}"
    }
  }

  val (prefix, groups, suffix) = parseArgs()

  @tailrec
  private def parseArgs(prefix: Vector[Arg] = Vector(), gs: List[Parameter] = Nil, n: Int = 0, off: Int = 0): (Vector[Arg], Set[Parameter], Vector[Arg]) = {
    if (n == args.length) (prefix.reverse, gs.to[Set], Vector())
    else if(args(n) == "--") {
      (prefix.reverse, gs.to[Set], args.drop(n + 1).map(Arg(_)).to[Vector])
    } else if (args(n) startsWith "--") {
      val idx = args(n).indexOf('=')
      if (idx < off) parseArgs(prefix, Parameter(Part(n, 2, args(n).length).throwIfBad()) :: gs, n + 1)
      else parseArgs(prefix, Parameter(Part(n, 2, idx).throwIfBad()) :: gs, n, idx + 1)
    } else if (args(n) startsWith "-") {
      if (off == 0) parseArgs(prefix, gs, n, 1)
      else if (args(n).length == off + 1) parseArgs(prefix, Parameter(Part(n, off, off + 1).throwIfBad()) :: gs, n + 1)
      else parseArgs(prefix, Parameter(Part(n, off, off + 1).throwIfBad()) :: gs, n, off + 1)
    } else {
      if (gs.isEmpty) parseArgs(Arg(args(n)) +: prefix, gs, n + 1)
      else parseArgs(prefix, gs.head.copy(values = gs.head.values :+ Part(n, 0, args(n).length).throwIfBad()) :: gs.tail, n + 1)
    }
  }

  def find(key: String): Option[Parameter] = groups.find(_.key() == key)

  def apply(names: Vector[String]): Option[Parameter] = names match {
    case Vector() => None
    case h +: t => find(h) orElse apply(t)
  }

  def get[T](param: SimpleParam[T]): Try[T] =
    groups.find(param.keys contains _.key()) match {
      case Some(p) => param.extractor.extract(p.values.map(_())) match {
        case Some(p) => Success(p)
        case None => Failure(InvalidArgValue(param.toString, p.values.map(_()).mkString(" ")))
      }
      case None => Failure(MissingArg(param.toString))
    }

  def apply[T: Default](param: SimpleParam[T]): Try[T] = {
    groups.find(param.keys contains _.key()) match {
      case Some(p) => param.extractor.extract(p.values.map(_())) match {
        case Some(p) => Success(p)
        case None => Failure(InvalidArgValue(param.toString, p.values.map(_()).mkString(" ")))
      }
      case None => Failure(MissingArg(param.toString))
    }
  }
  
  def isEmpty = args.isEmpty

  override def toString = groups.mkString
}

sealed class ParamException(msg: String) extends Exception(msg)

case class MissingArg(name: String) extends ParamException(s"the parameter --$name was missing")

case class InvalidArgValue(value: String, name: String)
    extends ParamException(s"the value '$value' is not valid for the parameter --$name")

case class UnexpectedParam(param: String) extends ParamException(s"found unexpected parameter '$param'")

@implicitNotFound("Can not combine elements of type ${A} and ${B}")
trait Construct[-A <: Params, -B <: Params] { construct =>
  type And <: Params
  type Or <: Params

  def and(a: A, b: B): ProductParams[And]
  def or(a: A, b: B): CoproductParams[Or]

  def swap: Construct[B, A] { type And = construct.And; type Or = construct.Or } =
    new Construct[B, A] {
      type And = construct.And
      type Or = construct.Or

      def and(a: B, b: A): ProductParams[And] = construct.and(b, a)
      def or(a: B, b: A): CoproductParams[Or] = construct.or(b, a)
    }
}

trait Construct_1 {

  implicit def general[A <: Params, B <: Params]: Construct[A, B] { type And = A with B; type Or = A with B } = {

    new Construct[A, B] {
      type And = A with B
      type Or = A with B

      def and(a: A, b: B) = ProductParams[A with B](Set(a, b))
      def or(a: A, b: B) = CoproductParams[A with B](Vector(a, b))
    }
  }
}

trait Default[T] { def default: T = getDefault(); def getDefault(): T }
object Default {
  implicit val int: Default[Int] = () => 0
  implicit val long: Default[Long] = () => 0L
  implicit val string: Default[String] = () => ""
  implicit val unit: Default[Unit] = () => ()
}

object Construct extends Construct_1 {

  implicit def leftProduct[A <: Params, B <: SimpleParam[_]]: Construct[ProductParams[A], B] {
    type And = A with B; type Or = ProductParams[A] with B
  } = {

    new Construct[ProductParams[A], B] {
      type And = A with B
      type Or = ProductParams[A] with B

      def and(a: ProductParams[A], b: B) = ProductParams[A with B](a.elements + b)

      def or(a: ProductParams[A], b: B) =
        CoproductParams[ProductParams[A] with B](Vector(a, b))
    }
  }

  implicit def rightProduct[A <: SimpleParam[_], B <: Params]: Construct[A, ProductParams[B]] {
    type And = B with A; type Or = ProductParams[B] with A
  } = leftProduct[B, A].swap

  implicit def leftCoproduct[A <: Params, B <: SimpleParam[_]]
    : Construct[CoproductParams[A], B] { type And = CoproductParams[A] with B; type Or = A with B } = {

    new Construct[CoproductParams[A], B] {
      type And = CoproductParams[A] with B
      type Or = A with B

      def and(a: CoproductParams[A], b: B) =
        ProductParams[CoproductParams[A] with B](Set(a, b))

      def or(a: CoproductParams[A], b: B) = CoproductParams[A with B](a.elements :+ b)
    }
  }

  implicit def rightCoproduct[A <: SimpleParam[_], B <: Params]
    : Construct[A, CoproductParams[B]] { type And = CoproductParams[B] with A; type Or = B with A } =
    leftCoproduct[B, A].swap
}

case class Suggestions(output: Option[Seq[Vector[String]]]) {
  def orElse(ss: Suggestions) = Suggestions(output orElse ss.output)
}

object SuggestionOutput {
  implicit val defaultOutput: SuggestionOutput = new SuggestionOutput {
    def output(ss: Suggestions) = ()
  }
}
trait SuggestionOutput { def output(ss: Suggestions): Unit }

trait Params { params =>
  type Result

  def parse(args: ParamMap, tabArg: Int = -1)(implicit suggestOutput: SuggestionOutput): Result = {

    val (result, lastArgs, ss) = check(ParamUsage(args, Set()), tabArg, Suggestions(None))

    suggestOutput.output(ss)

    lastArgs.unexpected foreach { p =>
      throw UnexpectedParam(p.key())
    }
    result
  }

  def check(args: ParamUsage, tabArg: Int, ss: Suggestions): (Result, ParamUsage, Suggestions)

  def &[B <: Params](b: B)(implicit con: Construct[params.type, b.type]): ProductParams[con.And] = {

    con.and(this, b)
  }

  def |[B <: Params](b: B)(implicit con: Construct[params.type, b.type]): CoproductParams[con.Or] = {

    con.or(this, b)
  }

  def unary_~ : OptionParams[this.type] = OptionParams(this)

  def by[R](fn: Result => R): Param.Handler[this.type, R] =
    new Param.Handler[this.type, R](this) {
      type From = Result
      def handle(v: From): R = fn(v)
    }
}

case class OptionParams[Ps <: Params](params: Ps) extends Params {
  type Result = Option[params.Result]

  def check(args: ParamUsage, tabArg: Int, ss: Suggestions): (Result, ParamUsage, Suggestions) =
    try {

      val (res, newArgs, newSs) = params.check(args, tabArg, ss)
      (Some(res), newArgs, newSs)
    } catch { case e: Exception => (None, args, ss) }

  override def toString = s"[$params]"
}

case class ProductParams[Ps <: Params](elements: Set[Params]) extends Params {
  type ProductTypes = Ps
  type Result = Product[ProductTypes]

  def check(args: ParamUsage, tabArg: Int, ss: Suggestions): (Result, ParamUsage, Suggestions) = {

    val (finalArgs, finalElems, newSs) = elements.foldLeft((args, Set[(Params, Any)](), ss)) {
      case ((args, es, ss), key) =>
        val (res, nextArgs, newSs) = key.check(args, tabArg, ss)
        (nextArgs, es + (key -> res), ss orElse newSs)
    }

    (new Product[Ps](finalElems.toMap), finalArgs, newSs)
  }

  override def toString = elements.mkString("( ", " & ", " )")
}

case class CoproductParams[Ps <: Params](elements: Vector[Params]) extends Params {
  type CoproductTypes = Ps
  type Result = Coproduct[CoproductTypes]

  def check(args: ParamUsage, tabArg: Int, ss: Suggestions): (Result, ParamUsage, Suggestions) = {
    val elems = elements.to[List].flatMap { k =>
      Try(Option(k.check(args, tabArg, ss)).get).toOption.map(k -> _)
    }

    elems match {
      case (key, (res, args, newSs)) :: Nil => (Coproduct[CoproductTypes](key -> res), args, newSs)
      case Nil => throw MissingArg(toString)
      case _ :: (key, _) :: _ => throw UnexpectedParam(key.toString)
    }
  }

  override def toString = elements.mkString("( ", " | ", " )")
}

object ToSuggestion {

  implicit val stringSuggestion: ToSuggestion[String] = new ToSuggestion[String] {
    def suggestion(value: String): Vector[String] = Vector(value)
  }
}
trait ToSuggestion[T] {
  def suggestion(value: T): Vector[String]
}

case class SimpleParam[T: Param.Extractor](keys: Vector[String]) extends Params { simpleParam =>

  type Result = T

  def toSuggestion(t: T): Vector[String] = Vector()
  def suggestions(s: String): Seq[T] = Seq()
  def checkValue: Option[T] = None

  def suggest(suggestions: T*)(implicit sug: ToSuggestion[T]): SimpleParam[T] =
    suggest { s =>
      suggestions
    }

  def suggest(suggestions: String => Seq[T])(implicit sug: ToSuggestion[T]): SimpleParam[T] = {
    val ss = suggestions
    new SimpleParam[T](keys) {
      override def toSuggestion(value: T): Vector[String] = sug.suggestion(value)
      override def suggestions(s: String) = ss(s)
      override def checkValue = simpleParam.checkValue
    }
  }

  def filter(fn: T => Boolean): SimpleParam[T] = new SimpleParam[T](keys) {
    override def toSuggestion(value: T): Vector[String] = simpleParam.toSuggestion(value)
    override def suggestions(str: String) = simpleParam.suggestions(str).filter(fn)
    override def checkValue = simpleParam.checkValue
  }

  val extractor = implicitly[Param.Extractor[T]]

  def check(args: ParamUsage, tabArg: Int, ss: Suggestions): (Result, ParamUsage, Suggestions) = {

    val parameter = args.map(keys) getOrElse { throw MissingArg(keys.head) }

    val res = extractor.extract(parameter.values.map(_ ())) getOrElse {
      throw InvalidArgValue(parameter.key(), parameter.values.mkString(" "))
    }

    checkValue.foreach { v =>
      if (v != res) throw InvalidArgValue(keys.head, "invalid")
    }

    val newSs = Suggestions(parameter.values.find(tabArg == _.no).map { p =>
      suggestions(p()).map(toSuggestion)
    })

    (res, args -- keys.to[Set], ss orElse newSs)
  }

  // Also consider `extractor` in `hashCode` and `equals`
  override def hashCode = keys.hashCode ^ extractor.hashCode

  override def equals(that: Any) = that match {
    case that: SimpleParam[_] =>
      keys.to[Set] == that.keys.to[Set] && that.extractor == extractor
    case _ =>
      false
  }

  override def toString =
    keys.map { k =>
      if (k.length == 1) s"-$k" else s"--$k"
    }.mkString("/")

  def of(v: T): SimpleParam[T] = new SimpleParam[T](keys) {
    override def toSuggestion(value: T): Vector[String] = simpleParam.toSuggestion(value)
    override def suggestions(str: String) = simpleParam.suggestions(str)
    override def checkValue = Some(v)
  }
}

object Param {

  object Extractor {
    implicit val stringExtractor: Extractor[String] = new Extractor[String] {
      // FIXME: Add mode parameter, and capture failure types
      def extract(values: Vector[String]): Option[String] = Some(values.mkString(" "))
    }

    implicit val intExtractor: Extractor[Int] = new Extractor[Int] {
      def extract(values: Vector[String]): Option[Int] = values match {
        case Vector(v) =>
          try Some(v.toInt)
          catch { case e: Exception => None }
        case _ => None
      }
    }

    implicit val unitExtractor: Extractor[Unit] = new Extractor[Unit] {
      def extract(values: Vector[String]): Option[Unit] = Some(())
    }
  }

  trait Extractor[T] { def extract(values: Vector[String]): Option[T] }

  abstract class Handler[-K, +H](val params: Params) {
    type From
    def handle(v: From): H
  }

  def apply[T: Extractor](shortName: Char, longName: Symbol): SimpleParam[T] =
    SimpleParam(Vector(shortName.toString, longName.name))

  def apply[T: Extractor](shortName: Char): SimpleParam[T] =
    SimpleParam(Vector(shortName.toString))

  def apply[T: Extractor](longName: Symbol): SimpleParam[T] = SimpleParam(Vector(longName.name))

  def flag(shortName: Char, longName: Symbol): SimpleParam[Unit] =
    SimpleParam(Vector(shortName.toString, longName.name))

  def flag(shortName: Char): SimpleParam[Unit] = SimpleParam(Vector(shortName.toString))
  def flag(longName: Symbol): SimpleParam[Unit] = SimpleParam(Vector(longName.name))
}

@implicitNotFound("product does not contain this value")
trait ProductContainsParam[V, T]

object ProductContainsParam extends ProductContainsParam_1 {
  implicit def optional[V <: OptionParams[_ <: Params], P <: Params]: ProductContainsParam[V, P] = null
}
trait ProductContainsParam_1 {
  implicit def generic[V, T <: V]: ProductContainsParam[V, T] = null
}

@implicitNotFound("coproduct cannot contain this value")
trait CoproductContainsParam[V, T]

object CoproductContainsParam {
  implicit def acceptable[V, T <: V]: CoproductContainsParam[V, T] = null
}

case class Product[T <: Params](tmap: Map[Params, Any]) {
  def apply[V <: Params](value: V)(implicit acc: ProductContainsParam[value.type, T]): value.Result =
    tmap(value).asInstanceOf[value.Result]

  override def toString = tmap.map { case (k, v) => s"$k: $v" }.mkString(", ")
}

case class Coproduct[T <: Params](value: (Params, Any)) {
  def handle[K, R](handlers: Param.Handler[K, R]*)(implicit ev: K <:< T): R = {
    val h = handlers.find(_.params == value._1).get
    h.handle(value._2.asInstanceOf[h.From])
  }

  override def toString = s"${value._1}: ${value._2}"
}
