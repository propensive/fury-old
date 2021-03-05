/*

    Fury, version 0.33.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

import fury._, io._, text._, model._, utils._

import guillotine._
import mercator._
import euphemism._
import jovian._

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
              main: ClassRef,
              securePolicy: Boolean,
              env: Map[String, String],
              properties: Map[String, String],
              policy: Policy,
              args: List[String],
              noSecurity: Boolean,
              workDir: Path,
              javaVersion: Int)
             (output: String => Unit)
             (implicit log: Log)
             : Running = {

    implicit val defaultEnvironment: Environment =
      Environment((environment.variables ++ env), environment.workDir)

    val policyFile = Installation.policyDir.extant() / UUID.randomUUID().toString
    policy.save(policyFile, workDir).get

    val allProperties: Map[String, String] =
      if(noSecurity) properties
      else properties.updated("java.security.manager", "").updated("java.security.policy", policyFile.value)

    val propArgs = allProperties.map { case (k, v) => if(v.isEmpty) str"-D$k" else str"-D$k=$v" }.to[List]

    val classpathStr = classpath.mkString(":")
    
    val cmd =
      if(securePolicy) sh"${Jdk.javaExec(javaVersion)} $propArgs -cp $classpathStr ${main.key} $args"
      else sh"${Jdk.javaExec(javaVersion)} -cp ${classpath.mkString(":")} ${main.key} $args"

    cmd.async(output(_), output(_))
  }

  def javac(classpath: List[String], dest: String, sources: List[String], javaVersion: Int)
           (implicit log: Log) =
    sh"${Jdk.javacExec(javaVersion)} -cp ${classpath.mkString(":")} -d $dest $sources".exec[Try[String]]

  def tryXdgOpen(url: Uri): Try[Unit] = {
    Try(sh"xdg-open ${url.key}".exec[String])
    Success(())
  }

  object java {
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