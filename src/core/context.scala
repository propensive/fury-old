/*

    Fury, version 0.15.1. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

import fury.model._

import guillotine._
import scala.util.Try
import scala.collection.JavaConverters._

import java.util.Hashtable
import javax.naming.directory._

object Dns {
  def lookup(domain: DomainName)(implicit log: Log): Try[List[String]] = {
    val env = new Hashtable[String, String]()
    env.put("java.naming.factory.initial", "com.sun.jndi.dns.DnsContextFactory")
    val dirContext = new InitialDirContext(env)
    
    for {
      atts <- Try(dirContext.getAttributes(domain.value, Array("TXT")))
      txts <- Try(Option(atts.get("TXT")).get)
    } yield txts.getAll.asScala.to[List].map(_.toString)
  }
}

class MenuContext(val cli: Cli,
                  val layout: Layout,
                  val layer: Layer,
                  val conf: FuryConf) {
  implicit def implicitLayout: Layout   = layout
  implicit def implicitShell: Shell     = Shell(cli.env)
  implicit def implicitEnv: Environment = cli.env
}
