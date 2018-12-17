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

object Install {
  private val zshrc = List(
    "autoload -Uz compinit",
    "fpath=($FURYHOME/completion/zsh $fpath)",
  )

  private val bashrc = List()
  private val fishrc = List()

  def alias(name: String, dir: Path, module: ModuleRef): String =
    str"alias $name='fury --layer=$dir '"

}

case class Installation(name: String, dir: Path, module: ModuleRef)
