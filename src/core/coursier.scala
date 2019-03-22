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

import fury.io._, fury.strings._

import scala.util._
import scala.collection._
import scala.concurrent._
import coursier.{Module => CModule, _}

object Coursier {

  implicit val ec: ExecutionContext = ExecutionContext.global

  private val cache: mutable.HashMap[Binary, Future[List[Path]]] =
    mutable.HashMap()

  def fetch(io: Io, binary: Binary): Future[List[Path]] = {
    def resolveRepository(repoId: String): Future[Repository] =
      coursier.cache.CacheParse
        .repositories(Seq(repoId))
        .either
        .map { repos =>
          Future.successful(repos.head)
        }
        .left
        .map { _ =>
          Future.failed[Repository](UnknownBinaryRepository(binary.binRepo))
        }
        .merge

    def resolve(repo: Repository): Future[List[Path]] = {
      io.println(msg"Resolving $binary")
      coursier
        .Fetch()
        .addRepositories(repo)
        .addDependencies(
            Dependency(
                CModule(Organization(binary.group), ModuleName(binary.artifact)),
                binary.version))
        .future
        .map(_._2.to[List].map { a =>
          Path(a._2.getAbsolutePath)
        })
    }

    cache.getOrElseUpdate(binary, resolveRepository(binary.binRepo.id).flatMap(resolve))
  }

}
