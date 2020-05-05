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

import fury.io._, fury.strings._, fury.model._, fury.utils._

import guillotine._
import mercator._
import euphemism._

import scala.util._
import scala.collection.mutable.HashMap
import java.util.UUID
import java.util.jar.JarFile
import java.util.zip.ZipFile
import java.io._

import org.apache.commons.compress.archivers.zip.{ParallelScatterZipCreator, ZipArchiveEntry,
    ZipArchiveOutputStream}

import java.util.zip.ZipInputStream

case class Shell(environment: Environment) {
  implicit private[this] val defaultEnvironment: Environment = environment

  def runJava(classpath: List[String],
              main: String,
              securePolicy: Boolean,
              env: Map[String, String],
              properties: Map[String, String],
              policy: Policy,
              layout: Layout,
              args: List[String],
              noSecurity: Boolean)
             (output: String => Unit)
             : Running = {
    layout.sharedDir.mkdir()

    implicit val defaultEnvironment: Environment =
      Environment((environment.variables ++ env).updated("SHARED", layout.sharedDir.value), environment.workDir)

    val policyFile = Installation.policyDir.extant() / UUID.randomUUID().toString
    policy.save(policyFile).get

    val allProperties: Map[String, String] = {
      val withPolicy = if(noSecurity) properties else
          properties.updated("java.security.manager", "").updated("java.security.policy", policyFile.value)
      
      withPolicy.updated("fury.sharedDir", layout.sharedDir.value)
    }

    val propArgs = allProperties.map { case (k, v) => if(v.isEmpty) str"-D$k" else str"-D$k=$v" }.to[List]

    val classpathStr = classpath.mkString(":")
    
    val cmd =
      if(securePolicy) sh"java $propArgs -cp $classpathStr $main $args"
      else sh"java -Dfury.sharedDir=${layout.sharedDir.value} -cp ${classpath.mkString(":")} $main $args"

    cmd.async(output(_), output(_))
  }

  def javac(classpath: List[String], dest: String, sources: List[String]) =
    sh"javac -cp ${classpath.mkString(":")} -d $dest $sources".exec[Try[String]]

  def tryXdgOpen(url: Uri): Try[Unit] = {
    Try(sh"xdg-open ${url.key}".exec[String])
    Success(())
  }

  object java {
    def ensureIsGraalVM(): Try[Unit] =
      sh"sh -c 'java -version 2>&1'".exec[Try[String]].map(_.contains("GraalVM")).transform(
        if(_) Success(()) else Failure(GraalVMError("non-GraalVM java")),
        _ => Failure(GraalVMError("Could not check Java version"))
      )

    def ensureNativeImageInPath(): Try[Unit] =
      Try(sh"native-image --help".exec[Try[String]]).fold(
          _ => Failure(GraalVMError("This requires the native-image command to be on the PATH")),
          _.map(_ => ())
      )
  }

  private def shadowDuplicates(jarInputs: List[Path]): Try[Map[Path, Set[String]]] = {
    jarInputs.traverse { path => Try {
      val in = new FileInputStream(path.javaFile)
      val zis = new ZipInputStream(in)
      
      val entries = Stream.continually(zis.getNextEntry).takeWhile(_ != null).map(_.getName).to[Set]

      (path, entries)
    } }.map(_.foldLeft((Set[String](), Map[Path, Set[String]]())) { case ((all, map), (path, entries)) =>
      (all ++ entries, map.updated(path, entries -- all))
    }._2)
  }

  def jar(dest: Path, jarInputs: Set[Path], pathInputs: Set[Path], manifest: JarManifest): Try[Unit] = {
    val zos = new ZipArchiveOutputStream(dest.javaPath.toFile)
    zos.setEncoding("UTF-8")
    zos.putArchiveEntry(new ZipArchiveEntry(JarFile.MANIFEST_NAME))
    zos.write(manifest.content.getBytes("UTF-8"))
    zos.closeArchiveEntry()

    val creator = new ParallelScatterZipCreator()
    val result = for {
      dups <- shadowDuplicates(jarInputs.to[List])
      _    <- jarInputs.traverse { input => Zipper.pack(new ZipFile(input.javaFile), creator) { x =>
                !x.getName.contains("META-INF") && !x.isDirectory && dups(input).contains(x.getName)
              } }
      _    <- pathInputs.traverse(Zipper.pack(_, creator))
    } yield creator.writeTo(zos)
    
    zos.close()
    
    result
  }

  def native(dest: Path, classpath: List[String], main: String): Try[Unit] = {
    implicit val defaultEnvironment: Environment = environment.copy(workDir = Some(dest.value))

    for {
      _  <- java.ensureNativeImageInPath
      //_  <- java.ensureIsGraalVM()
      cp  = classpath.mkString(":")
      _  <- sh"native-image -cp $cp $main".exec[Try[String]].map(main.toLowerCase.waive)
    } yield ()
  }
}