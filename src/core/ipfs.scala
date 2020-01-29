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

import java.io.ByteArrayInputStream

import fury.strings._, fury.io._, fury.model._

import guillotine._, environments.enclosing
import kaleidoscope._

import scala.util._
import scala.collection.JavaConverters._
import scala.concurrent._, duration._
import scala.concurrent.ExecutionContext.Implicits.global

object Ipfs {
  import io.ipfs.api._
  import io.ipfs.multihash.Multihash

  case class IpfsApi(api: IPFS){
    def add(path: Path): Try[IpfsRef] = Try {
      val file = new NamedStreamable.FileWrapper(path.javaFile)
      val nodes = api.add(file).asScala
      val top = nodes.last
      IpfsRef(top.hash.toBase58)
    }

    def get(ref: IpfsRef, path: Path): Try[Path] = getFile(Multihash.fromBase58(ref.key), path)

    def id(): Try[IpfsId] = Try {
      val idInfo = api.id()
      IpfsId(
        ID = idInfo.get("ID").toString,
        PublicKey = idInfo.get("PublicKey").toString,
        Addresses = idInfo.get("Addresses").asInstanceOf[java.util.List[String]].asScala.toList,
        AgentVersion = idInfo.get("AgentVersion").toString,
        ProtocolVersion = idInfo.get("ProtocolVersion").toString
      )
    }

    private def getFile(hash: Multihash, path: Path): Try[Path] = for {
      _    <- path.mkParents()
      data =  api.get(hash)
      in   =  new ByteArrayInputStream(data)
      _    =  TarGz.untar(in, path)
    } yield path.childPaths.head

  }

  case class IpfsId(ID: String, PublicKey: String, Addresses: List[String], AgentVersion: String,
      ProtocolVersion: String)

  def daemon(quiet: Boolean)(implicit log: Log): Try[IpfsApi] = {
    log.note("Checking for IPFS daemon")

    def getHandle(): Try[IPFS] = Try{ new IPFS("localhost", 5001) }

    def handleAsync(path: String): Future[Unit] = {
      val ready = Promise[Unit]
      Future(blocking{ sh"$path daemon".async(
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
      ).await()})
      log.infoWhen(!quiet)(msg"Waiting for the IPFS daemon...")
      ready.future
    }

    def find(): Try[String] = {
      val embedded = Installation.ipfsBin.javaFile
      if(embedded.isFile && embedded.canExecute) Success(Installation.ipfsBin.value)
      else {
        sh"which ipfs".exec[Try[String]]()
      }
    }

    def install(): Try[Unit] = {
      Installation.system.foreach { sys =>
        log.infoWhen(!quiet)(msg"Attempting to install IPFS for $sys")
      }
      val bin = distBinary.get
      log.infoWhen(!quiet)(msg"Downloading $bin...")
      for {
        in <- Http.requestStream(bin, Map[String, String](), "GET", Set())
        _  <- TarGz.extract(in, Installation.ipfsInstallDir)
        _  <- ~Installation.ipfsBin.setExecutable(true)
      } yield {
        log.infoWhen(!quiet)(msg"Installed embedded IFPS to ${Installation.ipfsInstallDir}")
      }
    }

    getHandle().recoverWith {
      //TODO think of a better way to match the exact exception
      case e: RuntimeException if e.getMessage.contains("Couldn't connect to IPFS daemon") =>
        log.infoWhen(!quiet)(msg"Couldn't connect to IPFS daemon")
        for {
          ipfsPath <- find().orElse(install().map(_ => Installation.ipfsBin.value))
          _        <- {
            val task = handleAsync(ipfsPath)
            Await.ready(task, 120.seconds).value.get
          }
          api      <- getHandle()
        } yield api
    }.map(IpfsApi(_))
  }

  private def distBinary: Try[Uri] = {
    def url(sys: String) = Https(Path("dist.ipfs.io") / "go-ipfs" / "v0.4.22" /
        str"go-ipfs_v0.4.22_$sys.tar.gz")
    
    Installation.system.flatMap {
      case Linux(X86)   => Success(url("linux-386"))
      case Linux(X64)   => Success(url("linux-amd64"))
      case MacOs(X86)   => Success(url("darwin-386"))
      case MacOs(X64)   => Success(url("darwin-amd64"))
      case other => Failure(UnknownOs(other.toString))
    }
  }
}


