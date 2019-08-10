/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.5.0. Copyright 2018-19 Jon Pretty, Propensive Ltd.                                        ║
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

package fury

import fury.strings._

import probably._

import scala.collection.JavaConverters._

import java.util.Random

import scala.language.implicitConversions

object StringsTests extends TestApp {

  val distinctFirstCharacters = Set("abc", "def", "ghi", "jkl")
  val duplicateFirstCharacter = distinctFirstCharacters + "afz"
  val randomDoubles = new Random(0).doubles.iterator.asScala.map(_.toString).take(1000000).to[Set]

  override def tests(): Unit = {
    test("distinct trigraphs need just a single character") {
      Compare.uniquePrefixes(distinctFirstCharacters)
    }.assert(_ == Set("a", "d", "g", "j"))

    test("one duplicate first character requires two characters") {
      Compare.uniquePrefixes(duplicateFirstCharacter)
    }.assert(_ == Set("ab", "af", "de", "gh", "jk"))

    test("different length strings") {
      Compare.uniquePrefixes(Set("one", "two", "three", "four"))
    }.assert(_.size == 4)

    test("large set all distinct") {
      Compare.uniquePrefixes(randomDoubles)
    }.assert(_.size == randomDoubles.size)
    
    test("large set not longer than necessary") {
      Compare.uniquePrefixes(randomDoubles).map(_.dropRight(1))
    }.assert(_.size < randomDoubles.size)
  }
}
