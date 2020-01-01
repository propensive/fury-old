package fury.core

import fury.io._, fury.model._, fury.strings._

import euphemism._

import scala.util._

object Service {
  def catalog(service: String): Try[List[Artifact]] = {
    val url = Https(Path(service) / "catalog")
    
    for {
      bytes <- Http.get(url, Map(), Set())
      catalog <- Try(Json.parse(new String(bytes, "UTF-8")).get)
      artifacts <- Try(catalog.entries.as[List[Artifact]].get)
    } yield artifacts
  }

  def publish(hash: String, path: String, quiet: Boolean, breaking: Boolean)
             (implicit log: Log)
             : Try[PublishedLayer] = {

    val url = Https(Path(ManagedConfig().service) / "publish")
    case class Request(path: String, token: String, hash: String, breaking: Boolean)
    case class Response(output: String)
    for {
      ipfs <- Ipfs.daemon(quiet)
      id   <- Try(ipfs.id().get)
      out  <- Http.post(url, Json(Request(path, ManagedConfig().token, hash, breaking)), headers = Set())
      str  <- Success(new String(out, "UTF-8"))
      json <- Try(Json.parse(str).get)
      res  <- Try(json.as[Response].get)
      // FIXME: Get major and minor version numbers
    } yield PublishedLayer(Uri("fury", Path(str"${ManagedConfig().service}/${path}")), 0, 0)
  }
}
  