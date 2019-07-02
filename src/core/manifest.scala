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
package fury.core

import fury.strings._
import java.util.jar.{Attributes, Manifest => JManifest}

object Manifest {

  def apply(classpath: Set[String], mainClass: Option[String]): JManifest = {
    import Attributes.Name._

    val result = new JManifest
    val mainAttributes = result.getMainAttributes
    mainAttributes.put(MANIFEST_VERSION, "1.0")
    mainClass.foreach(mainAttributes.put(MAIN_CLASS, _))
    mainAttributes.put(CLASS_PATH, classpath.join(" "))
    mainAttributes.putValue("Created-By", str"Fury ${Version.current}")
    result
  }

}
