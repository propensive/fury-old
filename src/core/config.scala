/*
  Fury, version 0.4.0. Copyright 2018-19 Jon Pretty, Propensive Ltd.

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

import exoskeleton._
import guillotine._

import scala.util._

object Config {

  def read()(implicit env: Environment, layout: Layout): Try[Config] =
    Success(Ogdl.read[Config](layout.userConfig, identity(_)).toOption.getOrElse(Config()))
}

case class Config(showContext: Boolean = true, theme: Theme = Theme.Basic, undoBuffer: Int = 5)
