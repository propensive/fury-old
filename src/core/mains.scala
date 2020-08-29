/*

    Fury, version 0.18.27. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

import fury.io._, fury.model._

import mercator._

import org.objectweb.asm._

import scala.util._

object Asm {

  def executableClasses(dir: Path): Set[ClassRef] = Try {
    
    var found: Boolean = false
    
    val visitor = new ClassVisitor(Opcodes.ASM5) {
      override def visitMethod(flags: Int, name: String, desc: String, sig: String, exc: Array[String])
                              : MethodVisitor = {
        if(name == "main" && desc == "([Ljava/lang/String;)V" &&
            ((flags & (Opcodes.ACC_STATIC | Opcodes.ACC_PUBLIC)) > 0)) found = true

        super.visitMethod(flags, name, desc, sig, exc)
      }
    }

    dir.walkTree.filter(!_.directory).filter(_.name.endsWith(".class")).to[List].flatMap { file =>
      found = false
      file.bytes().foreach { bytes =>
        new ClassReader(bytes).accept(visitor, ClassReader.SKIP_DEBUG | ClassReader.SKIP_CODE)
      }
      
      if(found) List(ClassRef(file.relativizeTo(dir).value.dropRight(6).replaceAll("\\/", "."))) else Nil
    }.to[Set]
  }.getOrElse(Set())
}