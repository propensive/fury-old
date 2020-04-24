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
package fury.strings

import gastronomy._

import scala.util._

import java.text.DecimalFormat

import language.implicitConversions

object `package` {
  implicit def joinable(values: Traversable[String]): Joinable   = Joinable(values)
  implicit def stringContexts(sc: StringContext): StringContexts = StringContexts(sc)

  implicit def stringPart[T: StringShow](value: T): StringPart =
    StringPart(implicitly[StringShow[T]].show(value))

  implicit def userMsg[T: MsgShow](value: T): UserMsg =
    implicitly[MsgShow[T]].show(value)

  implicit class StringExtensions(string: String) {
    def urlEncode: String = java.net.URLEncoder.encode(string, "UTF-8")
    def bytes: Array[Byte] = string.getBytes("UTF-8")
    def as[T: Parser: KeyName]: Try[T] = implicitly[Parser[T]].parse(string).asTry
  }

  implicit class Only[T](value: T) {
    def only[S](pf: PartialFunction[T, S]): Option[S] = Some(value).collect(pf)
    def unit: Unit = ()
  }
  
  implicit class OptionExtensions[T](value: Option[T]) {
    def ascribe(e: Exception): Try[T] = value.map(Success(_)).getOrElse(Failure(e))
    def asTry(implicit keyName: KeyName[T]): Try[T] = value.ascribe(Unspecified[T]())
  }
}

trait KeyName[T] { def apply(): UserMsg }

trait FuryException extends Exception

case class Unspecified(kind: UserMsg) extends FuryException
object Unspecified { def apply[T: KeyName](): Unspecified = Unspecified(implicitly[KeyName[T]].apply()) }

object Parser {
  implicit val string: Parser[String] = Some(_)
  implicit val int: Parser[Int] = str => Try(str.toInt).toOption
  
  implicit val boolean: Parser[Boolean] = _.only {
    case "on" | "true" | "1" => true
    case "off" | "false" | "0" => false
  }
}

trait Parser[T] { def parse(str: String): Option[T] }

object Rnd extends java.util.Random {
  def token(size: Int = 20): String = {
    val array = new Array[Byte](size)
    nextBytes(array)
    Bytes(array).encoded[Hex]
  }
}

object UserMsg { implicit val msgShow: MsgShow[UserMsg] = identity }

object FuryVersion {
  lazy val versionInfo: (String, String) = try {
    val dateFormat = new java.text.SimpleDateFormat("HH:mm:ss d MMMM yyyy")
    val lines = scala.io.Source.fromResource(".version", getClass.getClassLoader).getLines
    (lines.next, dateFormat.format(new java.util.Date(lines.next.toLong*1000L)))
  } catch { case e: Exception => ("unknown", "unknown") }

  def current: String = versionInfo._1
  def built: String = versionInfo._2
}

case class UserMsg(string: Theme => String) {

  def *(n: Int): UserMsg = UserMsg { theme =>
    string(theme) * n
  }

  def +(that: UserMsg): UserMsg = UserMsg { theme =>
    string(theme) + that.string(theme)
  }

  def length: Int = string(Theme.NoColor).length
}

case class StringContexts(context: StringContext) extends AnyVal {

  def msg(parts: UserMsg*): UserMsg = {
    val msgParts: Seq[UserMsg] = context.parts.map { p =>
      UserMsg { theme =>
        p
      }
    }
    (msgParts.head +: parts.zip(msgParts.tail).map { case (l, r) => l + r }).reduce(_ + _)
  }

  def str(parts: StringPart*): String =
    context.parts.head + parts
      .map(_.string)
      .zip(context.parts.tail)
      .map { case (l, r) => s"$l$r" }
      .mkString
}

object Strings {
  def magnitude(value: Double,
                units: String,
                base: Int = 1024,
                scale: List[String] = List("", "ki", "Mi", "Gi", "Ti", "Pi"),
                format: DecimalFormat = new DecimalFormat("0.0")): String =
      if(value < base) s"${format.format(value)} ${scale.head}${units}"
      else magnitude(value/base, units, base, scale.tail, format)
}

case class Joinable(values: Traversable[String]) extends AnyVal {
  def join: String = values.mkString
  def join(separator: String): String = values.mkString(separator)
  def join(left: String, separator: String, right: String): String = values.mkString(left, separator, right)
}

case class StringPart(string: String) extends AnyVal

object StringShow {
  implicit val string: StringShow[String] = identity
  implicit val char: StringShow[Char] = _.toString
  implicit val long: StringShow[Long] = _.toString
  implicit val int: StringShow[Int] = _.toString
}

trait StringShow[-T] { def show(value: T): String }

object Compare {
  def editDistance(from: String, to: String) = {
    val m       = from.length
    val n       = to.length
    val oldDist = new Array[Int](n + 1)
    val dist    = new Array[Int](n + 1)

    for (j <- 1 to n) oldDist(j) = oldDist(j - 1) + 1
    for (i <- 1 to m) {
      dist(0) = oldDist(0) + 1

      for (j <- 1 to n) dist(j) = (oldDist(j - 1) + (if(from(i - 1) == to(j - 1)) 0 else 1)).min(oldDist(j) +
          1).min(dist(j - 1) + 1)

      for (j <- 0 to n) oldDist(j) = dist(j)
    }
    dist(n)
  }

  def uniquePrefixLength(xs: Set[String]): Int = {
    val array = xs.to[Array]
    scala.util.Sorting.quickSort(array)
    if(array.isEmpty) 0
    else array.indices.init.foldLeft(-1) { (base, idx) =>
      val a = array(idx)
      val b = array(idx + 1)
      val size = a.length.min(b.length)
      
      base.max(((0 until size).indexWhere { i => a(i) != b(i) }) match {
        case -1 => size + 1
        case n  => n + 1
      })
    }
  }
}
