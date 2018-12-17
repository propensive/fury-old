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

import scala.io._

object UserMsg { implicit val msgShow: MsgShow[UserMsg] = identity }

object Version {
  final val current: String =
    Source.fromResource(".version", getClass.getClassLoader).getLines.next
}

case class UserMsg(string: Theme => String) {
  def *(n: Int): UserMsg = UserMsg { theme => string(theme)*n }
  def +(that: UserMsg): UserMsg = UserMsg { theme => string(theme) + that.string(theme) }

  def length: Int = string(Theme.NoColor).length
}

case class StringContexts(context: StringContext) extends AnyVal {
  
  def msg(parts: UserMsg*): UserMsg = {
    val msgParts: Seq[UserMsg] = context.parts.map { p => UserMsg { theme => p } }
    (msgParts.head +: parts.zip(msgParts.tail).map { case (l, r) => l+r }).reduce(_ + _)
  }
  
  def str(parts: StringPart*): String =
    context.parts.head+parts.map(_.string).zip(context.parts.tail).map { case (l, r) => s"$l$r" }.mkString
}

case class Joinable(values: Traversable[String]) extends AnyVal {
  def join: String = values.mkString
  def join(separator: String): String = values.mkString(separator)
  
  def join(left: String, separator: String, right: String): String =
    values.mkString(left, separator, right)
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
    val m = from.length
    val n = to.length
    val oldDist = new Array[Int](n + 1)
    val dist = new Array[Int](n + 1)

    for(j <- 1 to n) oldDist(j) = oldDist(j - 1) + 1
    for(i <- 1 to m) {
      dist(0) = oldDist(0) + 1
      
      for(j <- 1 to n) dist(j) = (oldDist(j - 1) + (if(from(i - 1) == to(j - 1)) 0 else 1)) min
            (oldDist(j) + 1) min (dist(j - 1) + 1)
      
      for(j <- 0 to n) oldDist(j) = dist(j)
    }
    dist(n)
  }
}
