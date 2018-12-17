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

import guillotine._

class MenuContext(val cli: Cli[CliParam[_]], val layout: Layout, val config: Config, val layer: Layer,
    val optSchemaId: Option[SchemaId] = None) {
  implicit def implicitLayout: Layout = layout
  implicit def implicitShell: Shell = cli.shell
  implicit def implicitEnv: Environment = cli.env
}
