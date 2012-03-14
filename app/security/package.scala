import models._
import play.api.mvc._
import play.api.mvc.Results._
import play.api.mvc.BodyParsers._

import security._

import scalaz._
import Scalaz._

package object security {

  trait Can {
    def canCreate = false
    def canRead = false
    def canUpdate = false
    def canDelete = false
    def isFiltered = false
  }

  val noRights = new Rights(c = false, r = false, u = false, d = false, f = false)
  val readOnly = new Rights(c = false, r = true, u = false, d = false, f = false)

  val tgNone = Target("")
  val tgReports = Target("reports")
  val tgAdminPortal = Target("adminPortal")
  val tgUserMaint = Target("userMaint")

  // (Ignore the IntelliJ syntax error on request, below.)
  case class ContextualizedRequest[A](context: Context, request: Request[A]) extends WrappedRequest(request) {
    def canCreate = context.rights.canCreate
    def canRead = context.rights.canRead
    def canUpdate = context.rights.canUpdate
    def canDelete = context.rights.canDelete
    def isFiltered = context.rights.isFiltered
  }
  object ContextualizedRequest {
    def apply[A](target: Target, request: Request[A]): ContextualizedRequest[A] = {
      request.session.get("id").flatMap(uid => models.User.findById(uid.toLong)).map { user =>
        ContextualizedRequest(Context(user, target), request)
      }.getOrElse(ContextualizedRequest(Context(None, noRights), request))
    }
  }

  def Createable[A](p: BodyParser[A])(target: Target)(f: ContextualizedRequest[A] => Result) = {
    Action(p) { request =>
      val ctxReq = ContextualizedRequest(target, request)
      if (ctxReq.canCreate) f(ctxReq) else Unauthorized
    }
  }

  def Createable(target: Target)(f: ContextualizedRequest[AnyContent] => Result) = {
    Action(parse.anyContent) { request =>
      val ctxReq = ContextualizedRequest(target, request)
      if (ctxReq.canCreate) f(ctxReq) else Unauthorized
    }
  }

  def Readable[A](p: BodyParser[A])(target: Target)(f: ContextualizedRequest[A] => Result) = {
    Action(p) { request =>
      val ctxReq = ContextualizedRequest(target, request)
      if (ctxReq.canRead) f(ctxReq) else Unauthorized
    }
  }

  def Readable(target: Target)(f: ContextualizedRequest[AnyContent] => Result) = {
    Action(parse.anyContent) { request =>
      val ctxReq = ContextualizedRequest(target, request)
      if (ctxReq.canRead) f(ctxReq) else Unauthorized
    }
  }

  def Updateable[A](p: BodyParser[A])(target: Target)(f: ContextualizedRequest[A] => Result) = {
    Action(p) { request =>
      val ctxReq = ContextualizedRequest(target, request)
      if (ctxReq.canUpdate) f(ctxReq) else Unauthorized
    }
  }

  def Updateable(target: Target)(f: ContextualizedRequest[AnyContent] => Result) = {
    Action(parse.anyContent) { request =>
      val ctxReq = ContextualizedRequest(target, request)
      if (ctxReq.canUpdate) f(ctxReq) else Unauthorized
    }
  }

  def Deleteable[A](p: BodyParser[A])(target: Target)(f: ContextualizedRequest[A] => Result) = {
    Action(p) { request =>
      val ctxReq = ContextualizedRequest(target, request)
      if (ctxReq.canDelete) f(ctxReq) else Unauthorized
    }
  }

  def Deleteable(target: Target)(f: ContextualizedRequest[AnyContent] => Result) = {
    Action(parse.anyContent) { request =>
      val ctxReq = ContextualizedRequest(target, request)
      if (ctxReq.canDelete) f(ctxReq) else Unauthorized
    }
  }

  def Unrestricted[A](p: BodyParser[A])(target: Target)(f: ContextualizedRequest[A] => Result) = {
    Action(p) { request =>
      val ctxReq = ContextualizedRequest(target, request)
      f(ctxReq)
    }
  }

  def Unrestricted(target: Target)(f: ContextualizedRequest[AnyContent] => Result) = {
    Action(parse.anyContent) { request =>
      val ctxReq = ContextualizedRequest(target, request)
      f(ctxReq)
    }
  }

  /**
   * Usages:
   *  def index =Contextualized(Target("reports")) { implicit ctxReq =>
   *    Ok("Hello " + ctxReq.context.user.name)
   *  }
   *  def index =Contextualized(tgReports) { implicit ctxReq =>
   *    Ok("Hello " + ctxReq.context.user.name)
   *  }
   */
}
