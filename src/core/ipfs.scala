/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.7.14. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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

import fury.strings._, fury.io._, fury.model._

import guillotine._, environments.enclosing
import kaleidoscope._
import euphemism._

import scala.util._
import scala.concurrent._, duration._
import scala.concurrent.ExecutionContext.Implicits.global

object Ipfs {

  class IpfsServer(ipfsPath: String) {
    def add(path: Path): Try[IpfsRef] =
      sh"$ipfsPath add -r -Q -H ${path.value}".exec[Try[String]].flatMap { out =>
        Try(IpfsRef(out))
      }

    def get(ref: IpfsRef, path: Path): Try[Path] =
      sh"$ipfsPath get /ipfs/${ref.key} -o ${path.value}".exec[Try[String]].map(_ => path)
    
    def id(): Option[IpfsId] = for {
      out  <- sh"$ipfsPath id".exec[Try[String]].toOption
      json <- Json.parse(out)
      id   <- json.as[IpfsId]
    } yield id
  }

  case class IpfsId(ID: String, PublicKey: String, Addresses: List[String], AgentVersion: String,
      ProtocolVersion: String)

  def daemon()(implicit log: Log): Try[IpfsServer] = {
    log.info("Checking for IPFS daemon")
    val awaiting = Promise[Try[IpfsServer]]()
   
    def handleAsync(path: String): Int = sh"$path daemon".async(
      stdout = {
        case r".*Daemon is ready.*" =>
          log.info("IPFS daemon has started")
          awaiting.success(Success(new IpfsServer(path)))
        case r".*Initializing daemon.*" =>
          log.info("Initializing IPFS daemon")
        case other =>
          log.note(str"[ipfs] $other")
      },
      stderr = {
        case r".*ipfs daemon is running.*" =>
          log.info("IPFS daemon is already running")
          awaiting.success(Success(new IpfsServer(path)))
        case other =>
          log.note(str"[ipfs] $other")
      }
    ).await()

    Future { blocking {
      handleAsync(Installation.ipfsBin.value) match {
        case 127 =>
          log.note("Could not find IPFS installation in Fury install directory; trying PATH")
          handleAsync("ipfs") match {
            case 127 =>
              log.info("IPFS is not installed; attempting to install IPFS")
              distBinary.foreach { bin =>
                log.info(msg"Downloading $bin...")
                (for {
                  in <- Http.requestStream(bin, Map[String, String](), "GET", Set())
                  _  <- TarGz.extract(in, Installation.ipfsInstallDir)
                  _  <- ~Installation.ipfsBin.setExecutable(true)
                  _  <- ~log.info(msg"Installed embedded IFPS to ${Installation.ipfsInstallDir}")
                } yield handleAsync(Installation.ipfsBin.value)) match {
                  case Success(_) =>
                    awaiting.success(Success(new IpfsServer(Installation.ipfsBin.value)))
                  case Failure(_) =>
                    log.fail(msg"Failed to run or install IPFS")
                    awaiting.success(Failure(IpfsNotOnPath()))
                }
              }
          }
      }
    } }

    Await.result(awaiting.future, 120.seconds)
  }

  private def distBinary: Option[Uri] = {
    def url(sys: String) = Https(Path("dist.ipfs.io") / "go-ipfs" / "v0.4.22" / str"go-ipfs_v0.4.22_$sys.tar.gz")
    Installation.system.flatMap {
      case Windows(_)   => None
      case Linux(X86)   => Some(url("linux-386"))
      case Linux(X64)   => Some(url("linux-amd64"))
      case MacOs(X86)   => Some(url("darwin-386"))
      case MacOs(X64)   => Some(url("darwin-amd64"))
    }
  }
}


