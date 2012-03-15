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
  case class CtxRqst[A](context: Context, request: Request[A]) extends WrappedRequest(request) {
    def canCreate = context.rights.canCreate

    def canRead = context.rights.canRead

    def canUpdate = context.rights.canUpdate

    def canDelete = context.rights.canDelete

    def isFiltered = context.rights.isFiltered

  }

  object CtxRqst {
    def apply[A](target: Target, request: Request[A]): CtxRqst[A] = {
      request.session.get("id").flatMap(uid => models.User.findById(uid.toLong)).map {
        user =>
          CtxRqst(Context(user, target), request)
      }.getOrElse(CtxRqst(Context(None, noRights), request))
    }
  }

  def withSession(r: PlainResult): PlainResult = {
    r.withSession("x" -> "session data added")
  }

  def withoutSession(r: PlainResult): PlainResult = {
    r.withNewSession
  }

  def create[A](ctx: CtxRqst[A]) = ctx.canCreate

  def read[A](ctx: CtxRqst[A]) = ctx.canRead

  def update[A](ctx: CtxRqst[A]) = ctx.canUpdate

  def delete[A](ctx: CtxRqst[A]) = ctx.canDelete

  def IfCan[A](ok: (CtxRqst[A]) => Boolean, p: BodyParser[A], target: Target,
               f: CtxRqst[A] => PlainResult): Action[A] = {
    Action(p) {
      request =>
        val ctxReq = CtxRqst(target, request)
        if (ok(ctxReq)) withSession(f(ctxReq)) else withSession(Unauthorized)
    }
  }

  def IfCanCreate[A](p: BodyParser[A])(target: Target)(f: CtxRqst[A] => PlainResult): Action[A] =
    IfCan(create[A] _, p, target, f)

  //  {
  //    Action(p) { request =>
  //      val ctxReq = CtxRqst(target, request)
  //      if (ctxReq.canCreate) withSession(f(ctxReq)) else withSession(Unauthorized)
  //    }
  //  }

  def IfCanCreate(target: Target)(f: CtxRqst[AnyContent] => PlainResult): Action[AnyContent] =
    IfCan(create[AnyContent] _, parse.anyContent, target, f)

  def IfCanRead[A](p: BodyParser[A])(target: Target)(f: CtxRqst[A] => PlainResult): Action[A] =
    IfCan(read[A] _, p, target, f)

  //
  //  def IfCanRead[A](p: BodyParser[A])(target: Target)(f: ContextualizedRequest[A] => PlainResult) = {
  //    Action(p) { request =>
  //      val ctxReq = ContextualizedRequest(target, request)
  //      if (ctxReq.canRead) withSession(f(ctxReq)) else withSession(Unauthorized)
  //    }
  //  }

  def IfCanRead(target: Target)(f: CtxRqst[AnyContent] => PlainResult): Action[AnyContent] =
    IfCan(read[AnyContent] _, parse.anyContent, target, f)

  def IfCanUpdate[A](p: BodyParser[A])(target: Target)(f: CtxRqst[A] => PlainResult): Action[A] =
    IfCan(update[A] _, p, target, f)


  def IfCanUpdate(target: Target)(f: CtxRqst[AnyContent] => PlainResult): Action[AnyContent] =
    IfCan(update[AnyContent] _, parse.anyContent, target, f)

  def IfCanDelete[A](p: BodyParser[A])(target: Target)(f: CtxRqst[A] => PlainResult): Action[A] =
    IfCan(delete[A] _, p, target, f)


  def IfCanDelete(target: Target)(f: CtxRqst[AnyContent] => PlainResult): Action[AnyContent] =
    IfCan(delete[AnyContent] _, parse.anyContent, target, f)

  def Unrestricted[A](p: BodyParser[A])(target: Target)(f: CtxRqst[A] => PlainResult): Action[A] = {
    Action(p) {
      request =>
        val ctxReq = CtxRqst(target, request)
        withSession(f(ctxReq))
    }
  }

  def Unrestricted(target: Target)(f: CtxRqst[AnyContent] => PlainResult): Action[AnyContent] =
    Unrestricted(parse.anyContent)(target)(f)
}
