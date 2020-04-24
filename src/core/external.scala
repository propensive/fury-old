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

import java.io.ByteArrayInputStream

import fury.strings._, fury.io._, fury.model._

import guillotine._, environments.enclosing
import kaleidoscope._
import antiphony._

import scala.util._
import scala.collection.JavaConverters._
import scala.concurrent._, duration._
import scala.concurrent.ExecutionContext.Implicits.global

object Ipfs {
  import io.ipfs.api._
  import io.ipfs.multihash.Multihash

  private var lastIpfsTimeout: Long = 0L
  // Attempt to use IPFS again if last failure was more than ten minutes ago
  def attemptToUseIpfs = System.currentTimeMillis - lastIpfsTimeout > 10*60*1000L

  private val knownGateways: List[DomainName] = List(DomainName("ipfs.io"), DomainName("gateway.pinata.cloud"))

  case class IpfsApi(api: IPFS){

    def add(path: Path): Try[IpfsRef] = for {
      file <- Try(new NamedStreamable.FileWrapper(path.javaFile))
      ref  <- add(file, wrap = false, onlyHash = false)
    } yield ref

    def add(string: String): Try[IpfsRef] = for {
      data <- Try(new ByteArrayInputStream(string.getBytes("UTF-8")))
      data <- Try(new NamedStreamable.InputStreamWrapper(data))
      ref  <- add(data, wrap = false, onlyHash = false)
    } yield ref

    def hash(path: Path): Try[IpfsRef] = for {
      file <- Try(new NamedStreamable.FileWrapper(path.javaFile))
      ref  <- add(file, wrap = false, onlyHash = true)
    } yield ref

    def get(ref: IpfsRef)(implicit log: Log): Try[String] = {
      val config = ManagedConfig()
      
      val getFromIpfs: Try[String] =
        if(!attemptToUseIpfs) Failure(IpfsTimeout())
        else if(config.skipIpfs) Failure(new IllegalStateException(
            "Using IPFS is disabled by the configuration"))
        else getRef(ref)

      getFromIpfs.failed.foreach { e => Ipfs.synchronized { lastIpfsTimeout = System.currentTimeMillis } }
      
      getFromIpfs.recoverWith { case e =>
        knownGateways.foldLeft[Try[String]](Failure(e)) {
          case (res@Success(_), _)   =>
            res
          case (Failure(_), gateway) =>
            val result = getFileFromGateway(ref, gateway)

            result.failed.foreach { e =>
              log.warn(msg"The layer could not be resolved using IPFS or $gateway.")
            }

            result
        }
      }.recoverWith {
        case e: RuntimeException if e.getMessage matches "timeout \\(.+\\) has been exceeded" =>
          Failure(IpfsTimeout())
        case _: java.net.SocketTimeoutException =>
          Failure(IpfsTimeout())
      }
    }

    def getRef(hash: IpfsRef): Try[String] = for {
      hash   <- ~Multihash.fromBase58(hash.key)
      bytes  <- Try(Await.result(Future(api.cat(hash)), 10.seconds))
      string <- ~(new String(bytes, "UTF-8"))
    } yield string
    
    def id(): Try[IpfsId] = Try {
      val idInfo = api.id()

      IpfsId(
        id = idInfo.get("ID").toString,
        publicKey = idInfo.get("PublicKey").toString,
        addresses = idInfo.get("Addresses") match {
          case xs: java.util.List[_] => xs.asScala.to[List].map(_.toString)
        },
        agentVersion = idInfo.get("AgentVersion").toString,
        protocolVersion = idInfo.get("ProtocolVersion").toString
      )
    }

    private[this] def add(data: NamedStreamable, wrap: Boolean, onlyHash: Boolean): Try[IpfsRef] = Try {
      val nodes = api.add(data, wrap, onlyHash).asScala
      val top = nodes.last
      IpfsRef(top.hash.toBase58)
    }

    private[this] def getFile(hash: Multihash, path: Path): Try[Path] = for {
      _    <- path.mkParents()
      data =  api.get(hash)
      in   =  new ByteArrayInputStream(data)
      _    =  TarGz.untar(in, path)
    } yield path.childPaths.head

    private[this] def getFileFromGateway(ref: IpfsRef, gateway: DomainName)
                                        (implicit log: Log)
                                        : Try[String] = {

      log.note(msg"Accessing $gateway to retrieve ${ref}")
      val params = List("arg" -> ref.key, "archive" -> "true")
      for {
        data   <- Http.get((Https(gateway) / "api" / "v0" / s"get").query(params: _*).key, Set.empty).to[Try]
        stream <- TarGz.untar(new ByteArrayInputStream(data))
      } yield new String(stream.head, "UTF-8")
    }

  }

  case class IpfsId(id: String, publicKey: String, addresses: List[String], agentVersion: String,
      protocolVersion: String)

  def daemon(quiet: Boolean)(implicit log: Log): Try[IpfsApi] = {
    log.note("Checking for IPFS daemon")

    def getHandle(): Try[IPFS] = Try(new IPFS("localhost", 5001).timeout(10000))

    def init(ipfs: Path): Try[Unit] = Xdg.ipfsRepo.ifExists().map(Success(()).waive).getOrElse {
      sh"${ipfs.value} init".exec[Try[String]]().map(_ => ())
    }

    def handleAsync(ipfs: Path): Future[Unit] = {
      val ready = Promise[Unit]
      Future(blocking { sh"${ipfs.value} daemon".async(
        stdout = {
          case r".*Daemon is ready.*" =>
            log.infoWhen(!quiet)(msg"IPFS daemon has started")
            ready.success(())
          case r".*Initializing daemon.*" =>
            log.infoWhen(!quiet)(msg"Initializing IPFS daemon")
          case other =>
            log.note(str"[ipfs] $other")
        },
        stderr = {
          case r".*ipfs daemon is running.*" =>
            log.note("IPFS daemon is already running")
          case other =>
            log.note(str"[ipfs] $other")
        }
      ).await() })
      log.infoWhen(!quiet)(msg"Waiting for the IPFS daemon to start...")
      ready.future
    }

    getHandle().recoverWith {
      //TODO think of a better way to match the exact exception
      case e: RuntimeException if e.getMessage.contains("Couldn't connect to IPFS daemon") =>
        log.infoWhen(!quiet)(msg"Couldn't connect to IPFS daemon")
        for {
          _    <- IpfsSoftware.installedPath(environments.enclosing, quiet)
          ipfs =  IpfsSoftware.activePath(environments.enclosing)
          _    <- init(ipfs)
          _    <- Await.ready(handleAsync(ipfs), 120.seconds).value.get
          api  <- getHandle()
        } yield api
    }.map(IpfsApi(_))
  }

}

object Software {
  def all: List[Software] = List(FurySoftware, IpfsSoftware, JavaSoftware, VsCodeSoftware, GitSoftware,
      CcSoftware, GraalVmSoftware)
}

abstract class Software(val name: ExecName) {
  def description: String
  def path(env: Environment): Option[Path] = Installation.findExecutable(name, env).toOption
  def base: Path = Installation.usrDir / name.key
  def version(env: Environment): Option[String]
  def website: Uri
}

abstract class Installable(name: ExecName) extends Software(name) {
  def activePath(env: Environment): Path = path(env).getOrElse(managedPath)
  def installVersion: String
  def managedPath: Path
  def tarGz: Try[Uri]
  
  def install(env: Environment, quiet: Boolean)(implicit log: Log): Try[Unit] = {
    Installation.system.foreach { sys =>
      log.infoWhen(!quiet)(msg"Attempting to install ${name} for $sys to ${base}")
    }
    for {
      bin <- tarGz
      _   <- ~log.infoWhen(!quiet)(msg"Downloading $bin...")
      in  <- Http.request(bin.key, Map[String, String](), "GET", Set()).to[Try]
      _   <- TarGz.extract(new ByteArrayInputStream(in), base)
      _   <- managedPath.setExecutable(true)
    } yield {
      log.infoWhen(!quiet)(msg"Installed ${name} to ${managedPath}")
    }
  }

  def installedPath(env: Environment, quiet: Boolean)(implicit log: Log): Try[Path] =
    if(!activePath(env).exists()) install(env, quiet).map { _ => activePath(env) }
    else Success(activePath(env))

}

object IpfsSoftware extends Installable(ExecName("ipfs")) {
  def installVersion = "0.4.22"
  def description: String = "InterPlanetary File System"
  def website = Https(Path("ipfs.io"))
  def managedPath: Path = base / "go-ipfs" / "ipfs"
  
  def tarGz: Try[Uri] = {
    def url(sys: String) = Https(Path("dist.ipfs.io") / "go-ipfs" / str"v$installVersion" /
        str"go-ipfs_v${installVersion}_$sys.tar.gz")
    
    Installation.system.flatMap {
      case Linux(X86) => Success(url("linux-386"))
      case Linux(X64) => Success(url("linux-amd64"))
      case MacOs(X86) => Success(url("darwin-386"))
      case MacOs(X64) => Success(url("darwin-amd64"))
      case other      => Failure(UnknownOs(other.toString))
    }
  }

  def version(env: Environment): Option[String] =
    sh"${activePath(env).value} --version".exec[Try[String]].map(_.split("\n").head.split(" ").last).toOption
}

object JavaSoftware extends Installable(ExecName("java")) {
  def installVersion = "8u242b08"
  def description = "Java™ SE Runtime Environment"
  def website = Https(Path("openjdk.java.net"))
  def managedPath: Path = Installation.usrDir / "java" / "bin" / "java"
  def version(env: Environment): Option[String] = Option(System.getProperty("java.version"))
  
  def tarGz: Try[Uri] = {
    def url(sys: String) = Https(Path("github.com") / "AdoptOpenJDK" / "openjdk8-binaries" / "releases" /
        "download" / str"OpenJDK8U-jdk_x64_${sys}_hotspot_${installVersion}.tar.gz")
    
    Installation.system.flatMap {
      case Linux(X64) => Success(url("linux"))
      case MacOs(X64) => Success(url("mac"))
      case other      => Failure(UnknownOs(other.toString))
    }
  }
}

object VsCodeSoftware extends Installable(ExecName("code")) {
  def installVersion = "1.42.1"
  def description = "Visual Studio Code"
  def website = Https(Path("code.visualstudio.com"))

  private def osDir(base: Path): Try[Path] = Installation.system.flatMap {
    case Linux(_) =>
      Success(base / "VSCode-linux-x64" / "bin" / "code")
    case MacOs(_) =>
      Success(base / "Visual Studio Code.app" / "Contents" / "Resources" / "app" / "bin" / "code")
    case other =>
      Failure(UnknownOs(other.toString))
  }

  def managedPath: Path = osDir(Installation.usrDir / "code").get

  def version(env: Environment): Option[String] =
    sh"${activePath(env).value} --version".exec[Try[String]].map(_.split("\n").head.split(" ").last).toOption
  
  def tarGz: Try[Uri] = {
    Installation.system.flatMap {
      case Linux(X64) => Success(Https(Path("go.microsoft.com") / "fwlink" / "?LinkID=620884"))
      case MacOs(X64) => Success(Https(Path("go.microsoft.com") / "fwlink" / "?LinkID=620882"))
      case other      => Failure(UnknownOs(other.toString))
    }
  }

  override def install(env: Environment, quiet: Boolean)(implicit log: Log): Try[Unit] = for {
    _ <- super.install(env, quiet)
    _ <- sh"${managedPath.value} --install-extension scalameta.metals".exec[Try[String]]: Try[String]
  } yield ()
}

object GitSoftware extends Software(ExecName("git")) {
  def website = Https(Path("git-scm.com"))
  def description = "Git"

  def version(env: Environment): Option[String] = path(env).flatMap { execPath =>
    sh"${execPath.value} --version".exec[Try[String]].map(_.split("\n").head.split(" ").last).toOption
  }
}

object CcSoftware extends Software(ExecName("cc")) {
  def website = Https(Path("gcc.gnu.org"))
  def description = "C Compiler"

  def version(env: Environment): Option[String] = path(env).flatMap { execPath =>
    sh"${execPath.value} --version".exec[Try[String]].map(_.split("\n").head.split(" ").last).toOption
  }
}

object GraalVmSoftware extends Installable(ExecName("native-image")) {
  def website = Https(Path("www.graalvm.org"))
  def description = "GraalVM Native Image"
  def installVersion = "20.0.0"

  def version(env: Environment): Option[String] = path(env).flatMap { execPath =>
    sh"${execPath.value} --version".exec[Try[String]].map(_.drop(16)).toOption
  }

  def tarGz: Try[Uri] = {
    def url(sys: String) = Https(Path("github.com") / "graalvm" / "graalvm-ce-builds" / "releases" /
        "download" / str"vm-$installVersion" / str"graalvm-ce-java8-${sys}-amd64-$installVersion.tar.gz")
    
    Installation.system.flatMap {
      case Linux(X64) => Success(url("linux"))
      case MacOs(X64) => Success(url("darwin"))
      case other => Failure(UnknownOs(other.toString))
    }
  }
  
  def managedPath = Installation.usrDir / "graalvm" / "graalvm-ce-java8-$installedVersion" / "bin" /
      "native-image"
}

object FurySoftware extends Software(ExecName("fury")) {
  def website = Https(Path("propensive.com") / "fury")
  def description = "Fury"
  def version(env: Environment): Option[String] = Some(FuryVersion.current)
  override def path(env: Environment): Option[Path] = Some(Installation.binDir / "fury")
}
