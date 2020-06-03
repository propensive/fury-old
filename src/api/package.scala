package fury

import fury.text._
import euphemism._
import fury.model._

import quarantine._
import antiphony._

import scala.concurrent._

package object api {
  implicit val ec: ExecutionContext = ExecutionContext.global
  implicit object domain extends Domain[FuryException] {
    implicit val access: Access.Mitigator[this.type] = Access.mitigate(this) { case e => BadRequest() }
    implicit val parseException: ParseException.Mitigator[this.type] = ParseException.mitigate(this) { case e => BadRequest() }
  }
  
  val httpServer = if(ManagedConfig().httpServer) Future(HttpServer) else Future(())
}