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

import fury.strings._, fury.model._, fury.utils._, fury.io._

import euphemism._
import kaleidoscope._
import antiphony._
import mercator._

import scala.util._

object GitHub {
  def repos(prefix: Option[String])(implicit log: Log): Try[List[String]] = prefix match {
    case Some(r"gh:$user@([a-z0-9]*)") =>
      val results = for {
        _     <- ~log.info(s"Looking for completions for $user...")
        token <- ManagedConfig().token.ascribe(NotAuthenticated())
        
        out   <- ~Http.get((Https(DomainName("api.github.com")) / "search" / "users").query("q" ->
                    (if(user.isEmpty) "language:scala" else user)).key,
                    Set(HttpHeader("Authorization", str"token ${token}"))).to[Try]
        
        out   <- out
        _     <- ~log.info(new String(out, "UTF-8"))
        json  <- Json.parse(new String(out, "UTF-8")).to[Try]
        repos <- json.items.as[List[Json]].to[Try]
        names <- Try(repos.flatMap(_.login.as[String].to[Option].toList).map(_.toLowerCase).filter(
                     _.startsWith(user)).map { name => str"gh:$name/" })
        _     <- ~log.info(names.toString)
      } yield names
      
      results.flatMap {
        case List(one) => repos(Some(one))
        case values    => Success(values)
      }
    case Some(r"gh:$user@([a-z][a-z0-9]*)\/$repo@([a-z]*)") =>
      for {
        _     <- ~log.info(s"Looking for completions for $user...")
        token <- ManagedConfig().token.ascribe(NotAuthenticated())
        
        out   <- ~Http.get((Https(DomainName("api.github.com")) / "users" / user / "repos").key,
                    Set(HttpHeader("Authorization", str"token ${token}"))).to[Try]
        
        _     <- ~log.info(out.toString)
        out   <- out
        _     <- ~log.info(new String(out, "UTF-8"))
        json  <- Json.parse(new String(out, "UTF-8")).to[Try]
        repos <- json.as[List[Json]].to[Try]
        names <- Try(repos.flatMap(_.name.as[String].to[Option].toList).filter(_.startsWith(repo)).map {
                     name => str"gh:$user/$name" })
      } yield names
    case _ =>
      List("gh:", "bb:", "gl:").filter(_.startsWith(prefix.getOrElse(""))) match {
        case "gh:" :: Nil => repos(Some("gh:"))
        case values     => Success(values)
      }
  }
}
