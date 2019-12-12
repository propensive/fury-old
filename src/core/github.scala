/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.7.14. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
   ║                                                                                                           ║
   ║ The primary distribution site is: https://propensive.com/                                                 ║
   ║                                                                                                           ║
   ║ Licensed under  the Apache License,  Version 2.0 (the  "License"); you  may not use  this file  except in ║
   ║ compliance with the License. You may obtain a copy of the License at                                      ║
   ║                                                                                                           ║
   ║     http://www.apache.org/licenses/LICENSE-2.0                                                            ║
   ║                                                                                                           ║
   ║ Unless required  by applicable law  or agreed to in  writing, software  distributed under the  License is ║
   ║ distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. ║
   ║ See the License for the specific language governing permissions and limitations under the License.        ║
   ╚═══════════════════════════════════════════════════════════════════════════════════════════════════════════╝
*/
package fury.core

import fury.strings._, fury.model._, fury.utils._

import euphemism._
import kaleidoscope._

import scala.util.Try

object GitHub {
  def repos(prefix: String)(implicit log: Log): List[String] = prefix match {
    case r"gh:$org@([a-z][a-z0-9]*)\/" => (for {
      _   <- ~log.info(s"Looking for completions for $org...")
      out <- Http.get(Uri("https", str"api.github.com/orgs/$org/repos"), Map(), Set(HttpHeader("Authorization", s"token ${ManagedConfig().token}")))
      _   <- ~log.info(new String(out, "UTF-8"))
      rs  <- Try(Json.parse(new String(out, "UTF-8")).get)
    } yield rs.as[List[Json]].get.flatMap(_.name.as[String].map { name => str"gh:$org/$name" }.to[List])).toOption.to[List].flatten
    case _ => Nil
  }
}
