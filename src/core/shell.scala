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

import guillotine._
import mitigation._
import kaleidoscope._

case class Shell()(implicit env: Environment) {
  type Out = Result[String, ~ | ShellFailure]
 
  object git {

    def setRemote(repo: Repo): Out =
      sh"git remote add origin ${repo.url}".exec[Out]

    def clone(repo: Repo, dir: Path): Out =
      sh"git clone ${repo.url} ${dir.value}".exec[Out]

    def cloneBare(url: String, dir: Path): Out =
      sh"git clone --bare $url ${dir.value}".exec[Out]

    def sparseCheckout(from: Path, dir: Path, sources: List[Path], commit: String): Result[String, ~ | ShellFailure | FileWriteError] = for {
      _   <- sh"git -C ${dir.value} init".exec[Out]
      _   <- if(!sources.isEmpty) sh"git -C ${dir.value} config core.sparseCheckout true".exec[Out] else Answer(())
      _   <- Answer { sources.foreach { src => (dir / ".git" / "info" / "sparse-checkout").appendSync(src.value+"/*\n") } }
      _   <- sh"git -C ${dir.value} remote add origin ${from.value}".exec[Out]
      str <- sh"git -C ${dir.value} pull origin $commit".exec[Out]
    } yield str

    def lsTree(dir: Path, commit: String): Result[List[Path], ~ | ShellFailure] = for {
      string <- sh"git -C ${dir.value} ls-tree -r --name-only $commit".exec[Out]
      files  <- ~string.split("\n").to[List].map(Path(_))
    } yield files

    def checkout(dir: Path, commit: String): Out =
      sh"git -C ${dir.value} checkout $commit".exec[Out]

    def init(dir: Path): Out =
      sh"git -C ${dir.value} init".exec[Out]

    def commit(dir: Path, message: String): Out =
      sh"git -C ${dir.value} commit -m $message".exec[Out]

    def add(dir: Path, paths: List[Path]): Out =
      sh"git -C ${dir.value} add ${paths.map(_.value)}".exec[Out]

    def status(dir: Path): Out =
      sh"git -C ${dir.value} status --porcelain".exec[Out]

    def pull(dir: Path, refspec: Option[RefSpec]): Out =
      sh"git -C ${dir.value} pull origin ${refspec.to[List].map(_.id)}".exec[Out]

    def push(dir: Path, all: Boolean): Out =
      (if(all) sh"git -C ${dir.value} push --all" else sh"git push").exec[Out]

    def tag(dir: Path, tag: String): Out =
      sh"git -C ${dir.value} tag $tag".exec[Out]

    def getCommitFromTag(dir: Path, tag: String): Out =
      sh"git -C ${dir.value} rev-parse $tag".exec[Out]

    def getCommit(dir: Path): Out =
      sh"git -C ${dir.value} rev-parse --short HEAD".exec[Out]

    def getRemote(dir: Path): Out =
      sh"git -C ${dir.value} remote get-url origin".exec[Out]

    def tags(dir: Path): Result[List[String], ~ | ShellFailure] = for {
      output <- sh"git -C ${dir.value} tag -l".exec[Out]
    } yield output.split("\n").to[List]
  }

  object bloop {
    def start(): Running = {
      sh"sh -c 'bloop server > /dev/null'".async(_ => (), _ => ())
    }
      
    def running(): Boolean = {
      sh"ng --nailgun-port 8212 help".exec[Exit[String]].status == 0
    }

    def clean(name: String)(output: String => Unit): Running =
      sh"ng --nailgun-port 8212 clean -c .fury/bloop $name".async(output(_), output(_))

    def compile(name: String, run: Boolean)(output: String => Unit): Running = {
      val action = if(run) "run" else "compile"
      sh"ng --nailgun-port 8212 $action -c .fury/bloop $name".async(output(_), output(_))
    }

    def startServer(): Running =
      sh"bloop server".async(_ => (), _ => ())
  }

  object coursier {
    def fetch(artifact: String): Result[List[Path], ~ | ShellFailure] = for {
      string <- sh"coursier fetch --repository central $artifact".exec[Out]
    } yield string.split("\n").to[List].map(Path(_))
  }

  private def jar(dest: Path, files: Set[(Path, List[String])], manifestFile: Path): Result[Unit, ~ | ShellFailure] =
    sh"jar cmf ${manifestFile.value} ${dest.value}".exec[Out].map { str =>
      val params = files.to[List].flatMap { case (path, files) => files.flatMap { file => List("-C", path.value, file) } }
      sh"jar uf ${dest.value} $params".exec[Out]
      ()
    }
  
  def copyTo(src: Path, dest: Path): Result[Path, ~ | ShellFailure | FileWriteError] =
    sh"cp -r ${src.value} ${dest.parent.value}/".exec[Out].map { _ => dest }

  def aggregatedJar(dest: Path, files: Set[(Path, List[String])], manifestFile: Path)
                   : Result[Unit, ~ | ShellFailure | FileWriteError] = for {
    dir <- ~(dest.parent / "staging")
    _   <- ~dir.mkdir()
    _   <- ~(for((p, fs) <- files; f <- fs) copyTo(p / f, dir / f))
    _   <- ~jar(dest, Set((dir, dir.children)), manifestFile)
    _   <- dir.delete()
  } yield ()

  object gpg {
    def sign(file: Path, signed: Path, key: String): Out =
      sh"gpg -a --output ${signed.value} --detach-sign ${file.value}".exec[Out]

    def keys(): Result[List[String], ~ | ShellFailure] = for {
      output <- sh"gpg --list-secret-keys".exec[Out]
    } yield output.split("\n").to[List].collect { case r"uid.*<$key@([^>]+)>.*" => key }
  }
}
