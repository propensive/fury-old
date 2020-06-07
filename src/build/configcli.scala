/*

    Fury, version 0.16.1. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury

import fury.core._, fury.text._, fury.model._, fury.io._
import Args._

import antiphony._
import euphemism._

import scala.util._
import scala.concurrent._, ExecutionContext.Implicits.global, duration._

import java.{text => jt}

case class ConfigCli(cli: Cli)(implicit log: Log) {
  def set: Try[ExitStatus] = for {
    cli      <- cli.hint(ThemeArg, Theme.all)
    cli      <- cli.hint(TimestampsArg, List("on", "off"))
    cli      <- cli.hint(PipeliningArg, List("on", "off"))
    cli      <- cli.hint(TraceArg, List("on", "off"))
    cli      <- cli.hint(NoIpfsArg, List("on", "off"))
    cli      <- cli.hint(ServiceArg, List("furore.dev"))
    call     <- cli.call()
    newTheme <- ~call(ThemeArg).toOption
    timestamps <- ~call(TimestampsArg).toOption
    pipelining <- ~call(PipeliningArg).toOption
    trace    <- ~call(TraceArg).toOption
    service  <- ~call(ServiceArg).toOption
    noIpfs   <- ~call(NoIpfsArg).toOption
    config   <- ~ManagedConfig()
    config   <- ~newTheme.map { th => config.copy(theme = th) }.getOrElse(config)
    config   <- ~service.map { s => config.copy(service = s) }.getOrElse(config)
    config   <- ~timestamps.map { ts => config.copy(timestamps = ts) }.getOrElse(config)
    config   <- ~pipelining.map { p => config.copy(pipelining = p) }.getOrElse(config)
    config   <- ~trace.map { t => config.copy(trace = t) }.getOrElse(config)
    config   <- ~noIpfs.map { x => config.copy(skipIpfs = x) }.getOrElse(config)
    _        <- ManagedConfig.write(config)
  } yield log.await()

  def install: Try[ExitStatus] = for {
    cli   <- cli.hint(ForceArg)
    call  <- cli.call()
    force <- ~call(ForceArg).isSuccess
    _     <- Install(cli.env, force)
  } yield log.await()

  def auth: Try[ExitStatus] = for {
    call     <- cli.call()
    token    <- doAuth
    config   <- ~ManagedConfig().copy(token = Some(token))
    _        <- ~ManagedConfig.write(config)
    _        <- ~log.info("You are now authenticated")
  } yield log.await()

  def doAuth: Try[OauthToken] = for {
    code     <- ~Rnd.token(18)
    // These futures should be managed in the session
    uri      <- ~(Https(ManagedConfig().service) / "await").query("code" -> code)
    future   <- ~Future(blocking(Http.get(uri.key, Set()).to[Try]))
    uri      <- ~(Https(ManagedConfig().service) / "auth").query("code" -> code)
    _        <- ~log.info(msg"Please visit $uri to authenticate using GitHub.")
    _        <- ~Future(blocking(Shell(cli.env).tryXdgOpen(uri)))
    response <- Await.result(future, Duration.Inf)
    json     <- Json.parse(new String(response, "UTF-8")).to[Try]
    token    <- json.token.as[String].to[Try]
  } yield OauthToken(token)

  def software: Try[ExitStatus] = for {
    layout <- cli.layout
    conf   <- Layer.readFuryConf(layout)
    layer  <- Layer.retrieve(conf)
    cli    <- cli.hint(RawArg)
    table  <- ~Tables().software(cli.env)
    cli    <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
    call   <- cli.call()
    col    <- ~cli.peek(ColumnArg)
    raw    <- ~call(RawArg).isSuccess
    rows   <- ~Software.all
    table  <- ~Tables().show[Software, Boolean](table, cli.cols, rows, raw, col)
    _      <- ~log.infoWhen(!raw)(conf.focus())
    _      <- ~log.rawln(table)
  } yield log.await()

}