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

case class AnsiCode(private val code: String) {
  def apply(): String = if(code == "") "" else s"\u001b[$code"
  def apply(str: String): String = if(code == "") str else s"${apply()}$str${Ansi.reset()}"
  def +(that: AnsiCode): AnsiCode = AnsiCode(s"${code}\u001b${that.code}")
}

object Rgb { def apply(r: Int, g: Int, b: Int): AnsiCode = AnsiCode(s"38;2;$r;$g;${b}m") }

object MsgShow { implicit val string: MsgShow[String] = v => UserMsg(_ => v) }

trait MsgShow[T] { def show(value: T): UserMsg }

object Ansi {

  val reset:     AnsiCode = AnsiCode("0m")
  val bold:      AnsiCode = AnsiCode("1m")
  val underline: AnsiCode = AnsiCode("4m")
  val strike:    AnsiCode = AnsiCode("9m")
  val italic:    AnsiCode = AnsiCode("3m")
  val reverse:   AnsiCode = AnsiCode("7m")

  def up(n: Int):    AnsiCode = AnsiCode(s"${n}A")
  def down(n: Int):  AnsiCode = AnsiCode(s"${n}B")
  def right(n: Int): AnsiCode = AnsiCode(s"${n}C")
  def left(n: Int):  AnsiCode = AnsiCode(s"${n}D")
  
  val black:   AnsiCode = AnsiCode("30m")
  val red:     AnsiCode = AnsiCode("31m")
  val green:   AnsiCode = AnsiCode("32m")
  val yellow:  AnsiCode = AnsiCode("33m")
  val blue:    AnsiCode = AnsiCode("34m")
  val magenta: AnsiCode = AnsiCode("35m")
  val cyan:    AnsiCode = AnsiCode("36m")
  val white:   AnsiCode = AnsiCode("37m")
  
  val brightBlack:   AnsiCode = AnsiCode("30;1m")
  val brightRed:     AnsiCode = AnsiCode("31;1m")
  val brightGreen:   AnsiCode = AnsiCode("32;1m")
  val brightYellow:  AnsiCode = AnsiCode("33;1m")
  val brightBlue:    AnsiCode = AnsiCode("34;1m")
  val brightMagenta: AnsiCode = AnsiCode("35;1m")
  val brightCyan:    AnsiCode = AnsiCode("36;1m")
  val brightWhite:   AnsiCode = AnsiCode("37;1m")
}

object Theme {
  lazy val all = List(Full, Basic, NoColor)

  implicit def stringShow: StringShow[Theme] = _.name

  def unapply(string: String): Option[Theme] = all.find(_.name == string)

  object Full extends Theme("full",
                            project     = Rgb(60, 200, 180),
                            projectDark = Rgb(30, 100, 90),
                            module      = Rgb(40, 200, 255),
                            moduleDark  = Rgb(20, 100, 127),
                            path        = Rgb(200, 150, 100),
                            repo        = Rgb(225, 80, 0),
                            param       = Rgb(200, 0, 150),
                            url         = Rgb(255, 160, 0),
                            version     = Rgb(150, 150, 100),
                            schema      = Rgb(240, 240, 80),
                            license     = Rgb(160, 160, 170),
                            binary      = Rgb(100, 200, 200),
                            gray        = Rgb(80, 80, 80),
                            success     = Rgb(0, 200, 0),
                            failure     = Rgb(200, 0, 0))
  
  object Basic extends Theme("basic",
                             project     = Ansi.brightGreen,
                             projectDark = Ansi.green,
                             module      = Ansi.brightBlue,
                             moduleDark  = Ansi.blue,
                             path        = Ansi.red,
                             repo        = Ansi.brightRed,
                             param       = Ansi.magenta,
                             url         = Ansi.cyan,
                             version     = Ansi.brightYellow,
                             schema      = Ansi.yellow,
                             license     = Ansi.white,
                             binary      = Ansi.brightCyan,
                             gray        = Ansi.brightBlack,
                             success     = Ansi.green,
                             failure     = Ansi.red)

  object NoColor extends Theme("nocolor") {
    override val reset:     AnsiCode = AnsiCode("")
    override val bold:      AnsiCode = AnsiCode("")
    override val underline: AnsiCode = AnsiCode("")
    override val strike:    AnsiCode = AnsiCode("")
    override val italic:    AnsiCode = AnsiCode("")
    override val reverse:   AnsiCode = AnsiCode("")
    override val success:   AnsiCode = AnsiCode("")
    override val failure:   AnsiCode = AnsiCode("")
  }
}

case class Theme(name: String,
                 project:     AnsiCode = AnsiCode(""),
                 projectDark: AnsiCode = AnsiCode(""),
                 module:      AnsiCode = AnsiCode(""),
                 moduleDark:  AnsiCode = AnsiCode(""),
                 path:        AnsiCode = AnsiCode(""),
                 repo:        AnsiCode = AnsiCode(""),
                 param:       AnsiCode = AnsiCode(""),
                 url:         AnsiCode = AnsiCode(""),
                 version:     AnsiCode = AnsiCode(""),
                 schema:      AnsiCode = AnsiCode(""),
                 license:     AnsiCode = AnsiCode(""),
                 binary:      AnsiCode = AnsiCode(""),
                 gray:        AnsiCode = AnsiCode(""),
                 success:     AnsiCode = AnsiCode(""),
                 failure:     AnsiCode = AnsiCode("")) {
  val reset:     AnsiCode = AnsiCode("0m")
  val bold:      AnsiCode = AnsiCode("1m")
  val underline: AnsiCode = AnsiCode("4m")
  val strike:    AnsiCode = AnsiCode("9m")
  val italic:    AnsiCode = AnsiCode("3m")
  val reverse:   AnsiCode = AnsiCode("7m")
}
