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

import escritoire._
import eucalyptus._
import scala.collection.immutable.SortedSet
import language.implicitConversions

object `package` {
  implicit def resolverExt[T](items: Traversable[T]): ResolverExt[T] = new ResolverExt[T](items)
  implicit def stringContexts(sc: StringContext): StringContexts = StringContexts(sc)
  
  implicit def stringPart[T: StringShow](value: T): StringPart =
    StringPart(implicitly[StringShow[T]].show(value))

  implicit def userMsg[T: StringShow](value: T): UserMsg =
    UserMsg { theme => implicitly[StringShow[T]].show(value) }

  implicit def ansiShow[T: MsgShow](implicit theme: Theme): AnsiShow[T] =
    value => implicitly[MsgShow[T]].show(value).string(theme)

  implicit def msgShowTraversable[T: MsgShow]: MsgShow[SortedSet[T]] =
    xs => UserMsg { theme => xs.map(implicitly[MsgShow[T]].show(_).string(theme)).join(" ") }

  implicit def stringShowOrdering[T: StringShow]: Ordering[T] =
    Ordering.String.on(implicitly[StringShow[T]].show(_))

  implicit def joinable(values: Traversable[String]): Joinable = Joinable(values)

  implicit val mainTag: Tag = Tag("fury")
  implicit val msgShowBoolean: MsgShow[Boolean] = if(_) msg">" else msg""
  implicit val msgShowPath: MsgShow[Path] = path => UserMsg(_.path(path.value))
  implicit val stringShow: StringShow[Path] = _.value
  implicit val diff: Diff[Path] = (l, r) => Diff.stringDiff.diff(l.value, r.value)
}
