/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.8.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.                                         ║
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
import org.apache.commons.compress.archivers.zip.{ParallelScatterZipCreator, ZipArchiveEntry, ZipArchiveOutputStream}

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

  object git {
    def cloneBare(url: String, dir: Path): Try[String] = {
      implicit val defaultEnvironment: Environment = sshBatchEnv

      sh"git clone --mirror $url ${dir.value}".exec[Try[String]].map { out => (dir / ".done").touch(); out }
    }

    def getOrigin(dir: Path): Try[String] =
      sh"git -C ${dir.value} config --get remote.origin.url".exec[Try[String]]

    def diffShortStat(dir: Path): Try[String] =
      sh"git --work-tree ${dir.value} -C ${(dir / ".git").value} diff --shortstat".exec[Try[String]]

    def sparseCheckout(from: Path,
                       dir: Path,
                       sources: List[Path],
                       refSpec: RefSpec,
                       commit: Commit,
                       remote: Option[String])
                      : Try[String] = for {
      _   <- sh"git -C ${dir.value} init".exec[Try[String]]
      _   <- if(!sources.isEmpty) sh"git -C ${dir.value} config core.sparseCheckout true".exec[Try[String]]
             else Success(())
      _   <- ~(dir / ".git" / "info" / "sparse-checkout").writeSync(sources.map(_.value + "/*\n").mkString)
      _   <- sh"git -C ${dir.value} remote add origin ${from.value}".exec[Try[String]]
      str <- sh"git -C ${dir.value} fetch --all".exec[Try[String]]
      _   <- sh"git -C ${dir.value} checkout ${commit.id}".exec[Try[String]]
      _   <- ~remote.foreach { url => for {
               _ <- sh"git -C ${dir.value} remote remove origin".exec[Try[String]]
               _ <- sh"git -C ${dir.value} remote add origin $url".exec[Try[String]]
               _ <- sh"git -C ${dir.value} checkout -b ${refSpec.id}".exec[Try[String]]
               _ <- sh"git -C ${dir.value} fetch".exec[Try[String]]
               _ <- sh"git -C ${dir.value} branch -u origin/${refSpec.id}".exec[Try[String]]
             } yield () }
      _   <- sources.map(_.in(dir)).traverse(_.setReadOnly())
      _   <- ~(dir / ".done").touch()
    } yield str

    def lsTree(dir: Path, commit: Commit): Try[List[Path]] = for {
      string <- sh"git -C ${dir.value} ls-tree -r --name-only ${commit.id}".exec[Try[String]]
      files  <- ~string.split("\n").to[List].map(Path(_))
    } yield files

    def lsRoot(dir: Path, commit: Commit): Try[List[String]] = for {
      string <- sh"git -C ${dir.value} ls-tree --name-only ${commit.id}".exec[Try[String]]
      files  <- ~string.split("\n").to[List]
    } yield files

    def showRefs(dir: Path): Try[List[String]] = for {
      refs  <- sh"git -C ${dir.value} show-ref --heads --tags".exec[Try[String]]
      lines <- ~refs.split("\n").to[List]
    } yield lines.map(_.split("/").last)

    private def sshBatchEnv: Environment = environment.append("GIT_SSH_COMMAND", "ssh -o BatchMode=yes")

    def lsRemote(url: String): Try[List[String]] = {
      implicit val defaultEnvironment: Environment = sshBatchEnv
      
      Cached.lsRemote.getOrElseUpdate(url, sh"git ls-remote --tags --heads $url".exec[Try[String]].map(_.split(
          "\n").to[List].map(_.split("/").last)))
    }

    def lsRemoteRefSpec(url: String, refSpec: String): Try[String] = {
      implicit val defaultEnvironment: Environment = sshBatchEnv
      
      Cached.lsRemoteRefSpec.getOrElseUpdate((url, refSpec), sh"git ls-remote $url $refSpec".exec[
          Try[String]].map(_.take(40)))
    }

    def remoteHasCommit(dir: Path, commit: Commit, track: RefSpec): Try[Boolean] =
      sh"git -C ${dir.value} rev-list origin/${track.id}".exec[Try[String]].map(_.split("\n").contains(commit.id))
    
    def currentBranch(dir: Path): Try[RefSpec] =
      sh"git -C ${dir.value} rev-parse --abbrev-ref HEAD".exec[Try[String]].map(RefSpec(_))

    def fetch(dir: Path, refspec: Option[RefSpec]): Try[String] =
      sh"git -C ${dir.value} fetch origin ${refspec.to[List].map(_.id)}".exec[Try[String]]

    def showFile(dir: Path, file: String): Try[String] =
      sh"git -C ${dir.value} show HEAD:$file".exec[Try[String]]

    def getCommitFromTag(dir: Path, tag: String): Try[String] =
      sh"git -C ${dir.value} rev-parse $tag".exec[Try[String]]

    def getCommit(dir: Path): Try[Commit] =
      sh"git -C ${dir.value} rev-parse HEAD".exec[Try[String]].map(Commit(_))

    def getTrackedFiles(dir: Path): Try[List[String]] =
      sh"git -C ${dir.value} ls-tree --name-only HEAD".exec[Try[String]].map(_.split("\n").to[List])

    def getAllTrackedFiles(dir: Path): Try[List[String]] =
      sh"git -C ${dir.value} ls-tree -r --name-only HEAD".exec[Try[String]].map(_.split("\n").to[List])

    def getBranchHead(dir: Path, branch: String): Try[Commit] =
      sh"git -C ${dir.value} show-ref -s heads/$branch".exec[Try[String]].map(Commit(_))

    def getTag(dir: Path, tag: String): Try[Commit] =
      sh"git -C ${dir.value} show-ref -s tags/$tag".exec[Try[String]].map(Commit(_))
    
    def getBranch(dir: Path): Try[String] =
      sh"git -C ${(dir / ".git").value} rev-parse --abbrev-ref HEAD".exec[Try[String]]
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

  def jar(dest: Path, jarInputs: Set[Path], pathInputs: Set[Path], manifest: JManifest): Try[Unit] = {
    val zos = new ZipArchiveOutputStream(dest.javaPath.toFile)
    zos.setEncoding("UTF-8")
    zos.putArchiveEntry(new ZipArchiveEntry(JarFile.MANIFEST_NAME))
    manifest.write(zos)
    zos.closeArchiveEntry()

    val creator = new ParallelScatterZipCreator
    val ok = for {
      _ <- jarInputs.traverse { input => Zipper.pack(new ZipFile(input.javaFile), creator)(x => !x.getName.contains("META-INF") && !x.isDirectory) }
      _ <- pathInputs.traverse(Zipper.pack(_, creator))
    } yield {
      creator.writeTo(zos)
    }
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
