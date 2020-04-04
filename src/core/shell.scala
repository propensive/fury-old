/*

    Fury, version 0.12.3. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

import fury.io._, fury.strings._, fury.model._

import guillotine._
import mercator._
import euphemism._

import scala.util._
import scala.collection.mutable.HashMap
import java.util.UUID
import java.util.jar.{JarFile, Manifest => JManifest}
import java.util.zip.ZipFile
import java.io._
import org.apache.commons.compress.archivers.zip.{ParallelScatterZipCreator, ZipArchiveEntry, ZipArchiveOutputStream}
import java.util.zip.ZipInputStream

case class Shell(environment: Environment) {
  private val furyHome   = Path(System.getProperty("fury.home"))

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

  def duplicateFiles(jarInputs: List[Path]): Try[Map[Path, Set[String]]] = {
    jarInputs.traverse { path => Try {
      val in = new FileInputStream(path.javaFile)
      val zis = new ZipInputStream(in)
      
      val entries = Stream.continually(zis.getNextEntry).takeWhile(_ != null).map(_.getName).to[Set]

      (path, entries)
    } }.map(_.foldLeft((Set[String](), Map[Path, Set[String]]())) { case ((all, map), (path, entries)) =>
      (all ++ entries, map.updated(path, entries -- all))
    }._2)
  }

  def jar(dest: Path, jarInputs: Set[Path], pathInputs: Set[Path], manifest: JManifest): Try[Unit] = {
    val zos = new ZipArchiveOutputStream(dest.javaPath.toFile)
    zos.setEncoding("UTF-8")
    zos.putArchiveEntry(new ZipArchiveEntry(JarFile.MANIFEST_NAME))
    manifest.write(zos)
    zos.closeArchiveEntry()

    val creator = new ParallelScatterZipCreator
    val ok = for {
      dups <- duplicateFiles(jarInputs.to[List])
      _    <- jarInputs.traverse { input => Zipper.pack(new ZipFile(input.javaFile), creator) { x =>
                !x.getName.contains("META-INF") && !x.isDirectory && dups(input).contains(x.getName)
              } }
      _    <- pathInputs.traverse(Zipper.pack(_, creator))
    } yield creator.writeTo(zos)
    
    zos.close()
    ok
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

object Cached {
  val lsRemote: HashMap[String, Try[List[String]]] = new HashMap()
  val lsRemoteRefSpec: HashMap[(String, String), Try[String]] = new HashMap()
}

object GitDir {

  def supplementEnv(env: Environment): Environment = env.append("GIT_SSH_COMMAND", "ssh -o BatchMode=yes")

  def lsRemote(url: String)(implicit env: Environment): Try[List[String]] =
    Cached.lsRemote.getOrElseUpdate(url, sh"git ls-remote --tags --heads $url".exec[Try[String]]()(
        implicitly, supplementEnv(env)).map(_.split("\n").to[List].map(_.split("/").last)))

  def lsRemoteRefSpec(url: String, refSpec: String)(implicit env: Environment): Try[String] =
    Cached.lsRemoteRefSpec.getOrElseUpdate((url, refSpec), sh"git ls-remote $url $refSpec".exec[Try[String]]()(
        implicitly, supplementEnv(env)).map(_.take(40)))

  def apply(dir: Path)(implicit env: Environment): GitDir =
    GitDir(supplementEnv(env), dir)
}

case class GitDir(env: Environment, dir: Path) {

  private implicit val environment: Environment = env
  private def git = List("git", "-c", dir.value)
  
  def cloneBare(url: String): Try[String] =
    sh"git clone --mirror $url ${dir.value}".exec[Try[String]].map { out => (dir / ".done").touch(); out }

  def getOrigin(): Try[String] =
    sh"$git config --get remote.origin.url".exec[Try[String]]

  def diffShortStat(other: Option[Commit] = None): Try[String] = { other match {
    case None =>
      sh"git --work-tree ${dir.value} -C ${(dir / ".git").value} diff --shortstat"
    case Some(commit) =>
      sh"git --work-tree ${dir.value} -C ${(dir / ".git").value} diff --shortstat ${commit.id}"
  } }.exec[Try[String]]

  def sparseCheckout(from: Path, sources: List[Path], refSpec: RefSpec, commit: Commit, remote: Option[String])
                    : Try[String] = for {
    _   <- sh"$git init".exec[Try[String]]
    _   <- if(!sources.isEmpty) sh"$git config core.sparseCheckout true".exec[Try[String]]
            else Success(())
    _   <- ~(dir / ".git" / "info" / "sparse-checkout").writeSync(sources.map(_.value + "/*\n").mkString)
    _   <- sh"$git remote add origin ${from.value}".exec[Try[String]]
    str <- sh"$git fetch --all".exec[Try[String]]
    _   <- sh"$git checkout ${commit.id}".exec[Try[String]]
    _   <- ~remote.foreach { url => for {
              _ <- sh"$git remote remove origin".exec[Try[String]]
              _ <- sh"$git remote add origin $url".exec[Try[String]]
              _ <- sh"$git checkout -b ${refSpec.id}".exec[Try[String]]
              _ <- sh"$git fetch".exec[Try[String]]
              _ <- sh"$git branch -u origin/${refSpec.id}".exec[Try[String]]
            } yield () }
    _   <- sources.map(_.in(dir)).traverse(_.setReadOnly())
    _   <- ~(dir / ".done").touch()
  } yield str

  def lsTree(commit: Commit): Try[List[Path]] = for {
    string <- sh"$git ls-tree -r --name-only ${commit.id}".exec[Try[String]]
    files  <- ~string.split("\n").to[List].map(Path(_))
  } yield files

  def lsRoot(commit: Commit): Try[List[String]] = for {
    string <- sh"$git ls-tree --name-only ${commit.id}".exec[Try[String]]
    files  <- ~string.split("\n").to[List]
  } yield files

  def showRefs(): Try[List[String]] = for {
    refs  <- sh"$git show-ref --heads --tags".exec[Try[String]]
    lines <- ~refs.split("\n").to[List]
  } yield lines.map(_.split("/").last)

  def remoteHasCommit(commit: Commit, track: RefSpec): Try[Boolean] =
    sh"$git rev-list origin/${track.id}".exec[Try[String]].map(_.split("\n").contains(commit.id))
  
  def currentBranch(): Try[RefSpec] =
    sh"$git rev-parse --abbrev-ref HEAD".exec[Try[String]].map(RefSpec(_))

  def fetch(refspec: Option[RefSpec]): Try[String] =
    sh"$git fetch origin ${refspec.to[List].map(_.id)}".exec[Try[String]]

  def showFile(file: String): Try[String] =
    sh"$git show HEAD:$file".exec[Try[String]]

  def getCommitFromTag(tag: String): Try[String] =
    sh"$git rev-parse $tag".exec[Try[String]]

  def getCommit(): Try[Commit] =
    sh"$git rev-parse HEAD".exec[Try[String]].map(Commit(_))

  def getTrackedFiles(): Try[List[String]] =
    sh"$git ls-tree --name-only HEAD".exec[Try[String]].map(_.split("\n").to[List])

  def getAllTrackedFiles(): Try[List[String]] =
    sh"$git ls-tree -r --name-only HEAD".exec[Try[String]].map(_.split("\n").to[List])

  def getBranchHead(branch: String): Try[Commit] =
    sh"$git show-ref -s heads/$branch".exec[Try[String]].map(Commit(_))

  def getTag(tag: String): Try[Commit] =
    sh"$git show-ref -s tags/$tag".exec[Try[String]].map(Commit(_))
  
  def getBranch(): Try[String] =
    sh"git -C ${(dir / ".git").value} rev-parse --abbrev-ref HEAD".exec[Try[String]]
}
