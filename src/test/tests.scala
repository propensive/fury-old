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
package fury.tests

import mitigation._
import probation._
import guillotine._, environments.enclosing
import impromptu._, scala.concurrent.ExecutionContext.Implicits.global

object Tests extends TestApp {
  def tests(): Unit = {

    val rebuild = Async {
      test("rebuild docker") {
        sh"docker build -t fury .".exec[Exit[String]]
      }.assert(_.status == 0)
    }

    val container = Async.after(rebuild) { implicit env =>
      test("start container") { Docker.start() }.assert(_.successful)
    }

    val initAlpha = Async.after(container) { implicit env =>
      test("initialize alpha") { for {
        container <- container().value
        out       <- ~container.alpha.run(sh"fury init -p pname")
        checksum  <- ~container.alpha.run(sh"md5sum layer.fury")
      } yield checksum.take(32) }.assert(_ == Answer("d41d8cd98f00b204e9800998ecf8427e"))
    }

    val initBeta = Async.after(container) { implicit env =>
      test("initialize beta") { for {
        container <- container().value
        out       <- ~container.beta.run(sh"fury init -p pname")
        checksum  <- ~container.beta.run(sh"md5sum layer.fury")
      } yield checksum.take(32) }.assert(_ == Answer("d41d8cd98f00b204e9800998ecf8427e"))
    }

    Async.after(initAlpha, initBeta, container) { implicit env =>
      test("stop container") { for {
        container <- container().value
        _         <- container.stop()
        _         <- Docker.prune()
      } yield () }.assert(_.successful)
    }.await().unit
  }
}

