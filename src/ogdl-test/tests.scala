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
package fury.tests

import fury._
import probation._
import optometry._

case class Simple(first: String)
case class Simple2(nested: Simple)
case class Simple3(nested: Simple2)
case class Alpha(first: Int, second: String)
case class Beta(first: Int, second: String, third: Int)

object Objects {
  val alpha = Alpha(1, "two")
  val beta = Beta(1, "two", 3)
}
import Objects._

object Tests extends TestApp {

  /*def diskReadWrite[T: OgdlReader: OgdlWriter](t: T): T = {
    val sb = new StringBuilder()
    Ogdl.serialize(sb, Ogdl(t))
    val tmp = Path.temp()
    Ogdl.write(t, tmp)
    Ogdl.read[T](tmp, identity) match {
      case mitigation.Surprise(e) =>
        e.printStackTrace()
        null.asInstanceOf[T]
      case other =>
        tmp.delete()
        other.unsafeGet
    }
  }*/

  import optics._

  def tests(): Unit = {
    test("successful") {
       1
    }.assert(_ == 2)
    
    /*test("serialize string") {
      diskReadWrite(Simple("one"))
    }.assert(_ == Simple("one"))
    
    test("serialize nested string") {
      diskReadWrite(Simple2(Simple("one")))
    }.assert(_ == Simple2(Simple("one")))
    
    test("serialize empty string") {
      diskReadWrite(Simple2(Simple("")))
    }.assert(_ == Simple2(Simple("")))
    
    test("serialize deeper nested string") {
      diskReadWrite(Simple3(Simple2(Simple("one"))))
    }.assert(_ == Simple3(Simple2(Simple("one"))))
    
    test("serialize two strings") {
      diskReadWrite(List(Simple("zero"), Simple("one")))
    }.assert(_ == List(Simple("zero"), Simple("one")))*/
    
  }
}

