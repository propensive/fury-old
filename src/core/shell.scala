/*
  Fury, version 0.2.2. Copyright 2019 Jon Pretty, Propensive Ltd.

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

import fury.io._
import fury.error._

import guillotine._
import kaleidoscope._

import scala.util._
import scala.collection.mutable.{HashMap, ListBuffer}

case class Shell()(implicit env: Environment) {

  val environment: Environment = env

  def runJava(
      classpath: List[String],
      main: String
    )(output: String => Unit
    )(implicit layout: Layout
    ): Running = {
    layout.sharedDir.mkdir()
    implicit val env = environment.append("SHARED", layout.sharedDir.value)
    sh"java -cp ${classpath.mkString(":")} $main".async(output(_), output(_))
  }

  object git {

    def setRemote(repo: Repo): Outcome[String] =
      sh"git remote add origin ${repo.url}".exec[Outcome[String]]

    def clone(repo: Repo, dir: Path): Outcome[String] = {
      implicit val env = environment.append("GIT_SSH_COMMAND", "ssh -o BatchMode=yes")

      sh"git clone ${repo.url} ${dir.value}".exec[Outcome[String]].map { out =>
        (dir / ".done").touch()
        out
      }
    }

    def cloneBare(url: String, dir: Path): Outcome[String] = {
      implicit val env = environment.append("GIT_SSH_COMMAND", "ssh -o BatchMode=yes")

      sh"git clone --bare $url ${dir.value}".exec[Outcome[String]].map { out =>
        (dir / ".done").touch()
        out
      }
    }

    def sparseCheckout(
        from: Path,
        dir: Path,
        sources: List[Path],
        commit: String
      ): Outcome[String] =
      for {
        _ <- sh"git -C ${dir.value} init".exec[Outcome[String]]
        _ <- if (!sources.isEmpty)
              sh"git -C ${dir.value} config core.sparseCheckout true".exec[Outcome[String]]
            else Success(())
        _ <- Success {
              (dir / ".git" / "info" / "sparse-checkout")
                .writeSync(sources.map(_.value + "/*\n").mkString)
            }
        _   <- sh"git -C ${dir.value} remote add origin ${from.value}".exec[Outcome[String]]
        str <- sh"git -C ${dir.value} pull origin $commit".exec[Outcome[String]]
        _   <- ~(dir / ".done").touch()
      } yield str

    def lsTree(dir: Path, commit: String): Outcome[List[Path]] =
      for {
        string <- sh"git -C ${dir.value} ls-tree -r --name-only $commit".exec[Outcome[String]]
        files  <- ~string.split("\n").to[List].map(Path(_))
      } yield files

    def lsRemote(url: String): Outcome[List[String]] =
      Cached.lsRemote.getOrElseUpdate(
          url,
          sh"git ls-remote --tags --heads $url"
            .exec[Outcome[String]]
            .map(_.split("\n").to[List].map(_.split("/").last)))

    def checkout(dir: Path, commit: String): Outcome[String] =
      sh"git -C ${dir.value} checkout $commit".exec[Outcome[String]].map { out =>
        (dir / ".done").touch()
        out
      }

    def init(dir: Path): Outcome[String] =
      sh"git -C ${dir.value} init".exec[Outcome[String]]

    def commit(dir: Path, message: String): Outcome[String] =
      sh"git -C ${dir.value} commit -m $message".exec[Outcome[String]]

    def add(dir: Path, paths: List[Path]): Outcome[String] =
      sh"git -C ${dir.value} add ${paths.map(_.value)}".exec[Outcome[String]]

    def status(dir: Path): Outcome[String] =
      sh"git -C ${dir.value} status --porcelain".exec[Outcome[String]]

    def pull(dir: Path, refspec: Option[RefSpec]): Outcome[String] =
      sh"git -C ${dir.value} pull origin ${refspec.to[List].map(_.id)}".exec[Outcome[String]]

    def push(dir: Path, all: Boolean): Outcome[String] =
      (if (all) sh"git -C ${dir.value} push --all" else sh"git push").exec[Outcome[String]]

    def showFile(dir: Path, file: String): Outcome[String] =
      sh"git -C ${dir.value} show HEAD:$file".exec[Outcome[String]]

    def tag(dir: Path, tag: String): Outcome[String] =
      sh"git -C ${dir.value} tag $tag".exec[Outcome[String]]

    def getCommitFromTag(dir: Path, tag: String): Outcome[String] =
      sh"git -C ${dir.value} rev-parse $tag".exec[Outcome[String]]

    def getCommit(dir: Path): Outcome[String] =
      sh"git -C ${dir.value} rev-parse --short HEAD".exec[Outcome[String]]

    def getBranchHead(dir: Path, branch: String): Outcome[String] =
      sh"git -C ${dir.value} show-ref -s heads/$branch".exec[Outcome[String]]

    def getTag(dir: Path, tag: String): Outcome[String] =
      sh"git -C ${dir.value} show-ref -s tags/$tag".exec[Outcome[String]]

    def getRemote(dir: Path): Outcome[String] =
      sh"git -C ${dir.value} remote get-url origin".exec[Outcome[String]]

    def tags(dir: Path): Outcome[List[String]] =
      for {
        output <- sh"git -C ${dir.value} tag -l".exec[Outcome[String]]
      } yield output.split("\n").to[List]
  }

  object bloop {

    def start(): Running =
      sh"sh -c 'launcher 1.2.1 > /dev/null'".async(_ => (), _ => ())

    def running(): Boolean =
      sh"bloop help".exec[Exit[String]].status == 0

    def clean(name: String)(output: String => Unit): Running =
      sh"bloop clean --config-dir .fury/bloop $name".async(output(_), output(_))

    def compile(name: String)(output: String => Unit): Running =
      sh"bloop compile $name --config-dir .fury/bloop".async(output(_), output(_))

    def startServer(): Running =
      sh"bloop server".async(_ => (), _ => ())
  }

  object coursier {

    def fetch(artifact: String): Outcome[List[Path]] = {
      val items = new ListBuffer[String]()
      val running = sh"coursier fetch --progress --repository central $artifact".async(items.append(_), println(_))
      val result = running.await()
      if(result == 0) Success(items.to[List].map(Path(_))) else Failure(ItemNotFound(msg"$artifact", msg"binary"))
    }
  }

  private def jar(dest: Path, files: Set[(Path, List[String])], manifestFile: Path): Outcome[Unit] =
    sh"jar cmf ${manifestFile.value} ${dest.value}".exec[Outcome[String]].map { str =>
      val params = files.to[List].flatMap {
        case (path, files) =>
          files.flatMap { file =>
            List("-C", path.value, file)
          }
      }
      sh"jar uf ${dest.value} $params".exec[Outcome[String]]
      ()
    }

  def copyTo(src: Path, dest: Path): Outcome[Path] =
    sh"cp -r ${src.value} ${dest.parent.value}/".exec[Outcome[String]].map { _ =>
      dest
    }

  def aggregatedJar(
      dest: Path,
      files: Set[(Path, List[String])],
      manifestFile: Path
    ): Outcome[Unit] =
    for {
      dir <- ~(dest.parent / "staging")
      _   <- ~dir.mkdir()
      _   <- ~(for ((p, fs) <- files; f <- fs) copyTo(p / f, dir / f))
      _   <- ~jar(dest, Set((dir, dir.children)), manifestFile)
      _   <- dir.delete()
    } yield ()

  object gpg {

    def sign(file: Path, signed: Path, key: String): Outcome[String] =
      sh"gpg -a --output ${signed.value} --detach-sign ${file.value}".exec[Outcome[String]]

    def keys(): Outcome[List[String]] =
      for {
        output <- sh"gpg --list-secret-keys".exec[Outcome[String]]
      } yield output.split("\n").to[List].collect { case r"uid.*<$key@([^>]+)>.*" => key }
  }
}

object Cached {
  val lsRemote: HashMap[String, Outcome[List[String]]] = new HashMap()
}
