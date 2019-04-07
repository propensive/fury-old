package fury.core

import scala.collection.JavaConverters._
import java.util.Hashtable
import javax.naming.directory.InitialDirContext

import kaleidoscope._

object Dns {
  def txt(domain: String): Map[String, String] = {
    val env = new Hashtable[String, String]()
    env.put("java.naming.factory.initial", "com.sun.jndi.dns.DnsContextFactory")
    val dirContext = new InitialDirContext(env)
    val attrs = dirContext.getAttributes(domain, Array("TXT"))
    attrs.get("TXT").getAll().asScala.to[List].flatMap { case str: String =>
      str.split(" ").to[List]
    }.map { case r"$key@([^=]*)=$value@(.*)$$" => (key, value) }.toMap
  }

}
