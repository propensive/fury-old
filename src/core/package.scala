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
package fury

import fury.strings._, fury.io._

import contextual._
import escritoire._
import gastronomy._
import guillotine._
import mercator._
import optometry._

import scala.collection.immutable.SortedSet
import scala.language.implicitConversions
import scala.util._

package object core extends GuillotineExtensions {
  implicit def resolverExt[T](items: Traversable[T]): ResolverExt[T] = new ResolverExt[T](items)

  implicit def sortedSetExt[T](set: SortedSet[T]): SortedSetExt[T] = new SortedSetExt[T](set)

  implicit def ansiShow[T: MsgShow](implicit theme: Theme): AnsiShow[T] =
    implicitly[MsgShow[T]].show(_).string(theme)

  implicit def msgShowTraversable[T: MsgShow]: MsgShow[SortedSet[T]] = xs =>
    UserMsg { theme => xs.map(implicitly[MsgShow[T]].show(_).string(theme)).join("\n") }

  implicit def stringShowOrdering[T: StringShow]: Ordering[T] =
    Ordering.String.on(implicitly[StringShow[T]].show(_))

  implicit val msgShowBoolean: MsgShow[Boolean] = if(_) msg">" else msg""
  implicit val msgShowPath: MsgShow[Path]       = path => UserMsg(_.path(path.value))
  implicit class Waive[T](t: T) { def waive[S]: S => T = { _ => t } }
  implicit class ShortTry[T](t: T) { def unary_~ : Try[T] = Try(t) }

  implicit class TryExtensions[T](t: Try[T]) {
    def pacify(alternative: => Option[T]): Try[T] = t match {
      case Success(v) => t
      case Failure(e) => alternative match {
        case Some(v) => Success(v)
        case None => t
      }
    }
  }

  implicit class FlatMap[F[_]: Monadic, T](monad: F[T]) {
    def >>=[S](that: T => F[S]): F[S] = monad.flatMap(that(_))
    def >>[S](that: T => S): F[S] = monad.map(that(_))
  }
  
  implicit class FlatMapPair[F[_]: Monadic, T1, T2](monadPair: (F[T1], F[T2])) {
    def >>=[S](that: (T1, T2) => F[S]): F[S] = for {
      t1 <- monadPair._1
      t2 <- monadPair._2
      s  <- that(t1, t2)
    } yield s
    
    def >>[S](that: (T1, T2) => S): F[S] = for {
      t1 <- monadPair._1
      t2 <- monadPair._2
    } yield that(t1, t2)
  }
  
  implicit class FlatMapTriple[F[_]: Monadic, T1, T2, T3](monadTriple: (F[T1], F[T2], F[T3])) {
    def >>=[S](that: (T1, T2, T3) => F[S]): F[S] = for {
      t1 <- monadTriple._1
      t2 <- monadTriple._2
      t3 <- monadTriple._3
      s  <- that(t1, t2, t3)
    } yield s
    
    def >>[S](that: (T1, T2, T3) => S): F[S] = for {
      t1 <- monadTriple._1
      t2 <- monadTriple._2
      t3 <- monadTriple._3
    } yield that(t1, t2, t3)
  }

  type Id[A] = A
  implicit def lensOptic[A, AId](id: AId)(implicit resolver: Resolver[A, AId]): Optic[SortedSet, Id, A] =
    new Optic[SortedSet, Id, A]("focus") {
      def map[B](v: SortedSet[A])(fn: A => B): B     = fn(v.find(resolver.matchOn(id, _)).get)
      def comap(f: SortedSet[A], g: A): SortedSet[A] = f.filterNot(resolver.matchOn(id, _)) + g
    }
}
