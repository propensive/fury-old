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

object Prompt {
  
  private def escape(code: AnsiCode): String = s"%{${code()}%}"
  
  def zsh(layer: Layer,
          schema: Schema,
          optProject: Option[Project],
          optModule: Option[Module])
         (implicit theme: Theme): String = {
    val schemaId = schema.id.key
    val projectId = optProject.map(_.id.key).getOrElse("-")
    val moduleId = optModule.map(_.id.key).getOrElse("-")
    val schemaText = if(layer.schemas.size <= 1) "" else s"${escape(theme.schema)}$schemaId${escape(theme.gray)}/"
    
    msg" ${escape(theme.gray)}[$schemaText${escape(theme.project)}$projectId${escape(theme.gray)}/${escape(theme.module)}$moduleId${escape(theme.gray)}]${escape(Ansi.reset)}".string(theme)
  }

  def empty(config: Config)(implicit theme: Theme) = {
    msg" ${escape(config.theme.gray)}[${escape(config.theme.schema)}+${escape(config.theme.gray)}]${escape(Ansi.reset)}".string(theme)
  }
}
