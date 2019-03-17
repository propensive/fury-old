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

import fury.io._

import guillotine._
import kaleidoscope._

import scala.util._
import scala.collection.mutable.{HashMap, ListBuffer}

case class Shell(environment: Environment) {
  private val furyHome   = Path(System.getProperty("fury.home"))
  private val policyFile = furyHome / "etc" / "security" / "fury_application_module.policy"

  implicit private[this] val defaultEnvironment: Environment = environment

  def runJava(
      classpath: List[String],
      main: String,
      securePolicy: Boolean,
      layout: Layout
    )(output: String => Unit
    ): Running = {
    layout.sharedDir.mkdir()
    implicit val defaultEnvironment: Environment =
      environment.append("SHARED", layout.sharedDir.value)
    val cmd =
      if (securePolicy)
        sh"java -Djava.security.manager -Djava.security.policy=${policyFile.value} -Dfury.sharedDir=${layout.sharedDir.value} -cp ${classpath
          .mkString(":")} $main"
      else sh"java -Dfury.sharedDir=${layout.sharedDir.value} -cp ${classpath.mkString(":")} $main"

    cmd.async(output(_), output(_))
  }

  def javac(classpath: List[String], dest: String, sources: List[String]) =
    sh"javac -cp ${classpath.mkString(":")} -d $dest $sources".exec[Try[String]]

  object ipfs {

    def add(path: Path): Try[IpfsRef] =
      sh"ipfs add ${path.value}".exec[Try[String]].flatMap { out =>
        Try(IpfsRef(out.split(" ")(1)))
      }

    def get(ref: IpfsRef, path: Path): Try[Path] =
      sh"ipfs get ${ref.value} -o ${path.value}".exec[Try[String]].map(_ => path)
  }

  object git {

    def cloneBare(url: String, dir: Path): Try[String] = {
      implicit val defaultEnvironment: Environment =
        environment.append("GIT_SSH_COMMAND", "ssh -o BatchMode=yes")

      sh"git clone --mirror $url ${dir.value}".exec[Try[String]].map { out =>
        (dir / ".done").touch()
        out
      }
    }

    def sparseCheckout(
        from: Path,
        dir: Path,
        sources: List[Path],
        refSpec: String,
        commit: String
      ): Try[String] =
      for {
        _ <- sh"git -C ${dir.value} init".exec[Try[String]]
        _ <- if (!sources.isEmpty)
              sh"git -C ${dir.value} config core.sparseCheckout true".exec[Try[String]]
            else Success(())
        _ <- Success {
              (dir / ".git" / "info" / "sparse-checkout")
                .writeSync(sources.map(_.value + "/*\n").mkString)
            }
        _   <- sh"git -C ${dir.value} remote add origin ${from.value}".exec[Try[String]]
        str <- sh"git -C ${dir.value} fetch origin $refSpec".exec[Try[String]]
        _   <- sh"git -C ${dir.value} checkout $commit".exec[Try[String]]
        _   <- ~(dir / ".done").touch()
      } yield str

    def lsTree(dir: Path, commit: String): Try[List[Path]] =
      for {
        string <- sh"git -C ${dir.value} ls-tree -r --name-only $commit".exec[Try[String]]
        files  <- ~string.split("\n").to[List].map(Path(_))
      } yield files

    def showRefs(dir: Path): Try[List[String]] =
      for {
        refs  <- sh"git -C ${dir.value} show-ref --heads --tags".exec[Try[String]]
        lines <- ~refs.split("\n").to[List]
      } yield lines.map(_.split("/").last)

    def lsRemote(url: String): Try[List[String]] = {
      implicit val defaultEnvironment: Environment =
        environment.append("GIT_SSH_COMMAND", "ssh -o BatchMode=yes")
      Cached.lsRemote.getOrElseUpdate(
          url,
          sh"git ls-remote --tags --heads $url"
            .exec[Try[String]]
            .map(_.split("\n").to[List].map(_.split("/").last)))
    }

    def lsRemoteRefSpec(url: String, refSpec: String): Try[String] = {
      implicit val defaultEnvironment: Environment =
        environment.append("GIT_SSH_COMMAND", "ssh -o BatchMode=yes")
      Cached.lsRemoteRefSpec.getOrElseUpdate(
          (url, refSpec),
          sh"git ls-remote $url $refSpec".exec[Try[String]].map(_.take(40)))
    }

    def fetch(dir: Path, refspec: Option[RefSpec]): Try[String] =
      sh"git -C ${dir.value} fetch origin ${refspec.to[List].map(_.id)}".exec[Try[String]]

    def showFile(dir: Path, file: String): Try[String] =
      sh"git -C ${dir.value} show HEAD:$file".exec[Try[String]]

    def getCommitFromTag(dir: Path, tag: String): Try[String] =
      sh"git -C ${dir.value} rev-parse $tag".exec[Try[String]]

    def getCommit(dir: Path): Try[String] =
      sh"git -C ${dir.value} rev-parse --short HEAD".exec[Try[String]]

    def getBranchHead(dir: Path, branch: String): Try[String] =
      sh"git -C ${dir.value} show-ref -s heads/$branch".exec[Try[String]]

    def getTag(dir: Path, tag: String): Try[String] =
      sh"git -C ${dir.value} show-ref -s tags/$tag".exec[Try[String]]
  }

  /*object bloop {
    private val defaultConfigDir = Path(".fury") / "bloop"

    //def start(version: String): Running =
      //sh"sh -c 'launcher --skip-bsp-connection ${version} > /dev/null'".async(_ => (), _ => ())

    def running(): Boolean =
      sh"bloop help".exec[Exit[String]].status == 0

    def clean(name: String, configDir: Path = defaultConfigDir)(output: String => Unit): Running =
      sh"bloop clean --config-dir ${configDir.value} $name".async(output(_), output(_))

    def compile(name: String, configDir: Path = defaultConfigDir)(output: String => Unit): Running =
      sh"bloop compile $name --config-dir ${configDir.value}".async(output(_), output(_))

    def startBsp(socket: String): Running =
      sh"bloop bsp --socket $socket".async(println(_), _ => ())
  }*/

  object java {

    def ensureIsGraalVM(): Try[Unit] =
      sh"sh -c 'java -version 2>&1'"
        .exec[Try[String]]
        .map(_.contains("GraalVM"))
        .fold(
            _ => Failure(GraalVMError("Could not check Java version")),
            isGraal => if (isGraal) Success(()) else Failure(GraalVMError("non-GraalVM java")))

    def ensureNativeImageInPath(): Try[Unit] =
      Try(sh"native-image --help".exec[Try[String]]).fold(
          _ => Failure(GraalVMError("native-image could not be executed")),
          _.map(_ => ())
      )
  }

  private def jar(dest: Path, files: Set[(Path, List[String])], manifestFile: Path): Try[Unit] =
    sh"jar cmf ${manifestFile.value} ${dest.value}".exec[Try[String]].map { str =>
      val params = files.to[List].flatMap {
        case (path, files) =>
          files.flatMap { file =>
            List("-C", path.value, file)
          }
      }
      sh"jar uf ${dest.value} $params".exec[Try[String]]
      ()
    }

  def native(dest: Path, classpath: List[String], main: String): Try[Unit] = {
    implicit val defaultEnvironment: Environment =
      environment.copy(workDir = Some(dest.value))
    for {
      _ <- java.ensureNativeImageInPath
      _ <- java.ensureIsGraalVM()
      _ <- sh"native-image -cp ${classpath.mkString(":")} ${main}".exec[Try[String]].map { _ =>
            main.toLowerCase()
          }
    } yield ()
  }

  def copyTo(src: Path, dest: Path): Try[Path] =
    sh"cp -r ${src.value} ${dest.parent.value}/".exec[Try[String]].map { _ =>
      dest
    }

  def aggregatedJar(dest: Path, files: Set[(Path, List[String])], manifestFile: Path): Try[Unit] =
    for {
      dir <- ~(dest.parent / "staging")
      _   <- ~dir.mkdir()
      _   <- ~(for ((p, fs) <- files; f <- fs) copyTo(p / f, dir / f))
      _   <- ~jar(dest, Set((dir, dir.children)), manifestFile)
      _   <- dir.delete()
    } yield ()
}

object Cached {
  val lsRemote: HashMap[String, Try[List[String]]]            = new HashMap()
  val lsRemoteRefSpec: HashMap[(String, String), Try[String]] = new HashMap()
}
