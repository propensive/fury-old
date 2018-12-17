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

import mitigation._

object Manifest {

  def file(file: Path, classpath: Set[String], mainClass: Option[String])
          : Result[Path, ~ | FileWriteError] = {
    val classpathString = classpath.join(" ")
    
    val content: String = List(
      List("Manifest-Version" -> "1.0"),
      mainClass.to[List].map("Main-Class" -> _),
      List("Class-Path" -> classpathString),
      List("Created-By" -> str"Fury ${Version.current}"),
    ).flatten.map { case (k, v) => s"$k: $v" }.join("", "\n", "\n")
    
    file.writeSync(content).map { _ => file }
  }

}
