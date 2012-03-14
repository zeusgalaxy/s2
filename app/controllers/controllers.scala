import models._
import play.api.mvc._
import play.api.mvc.BodyParsers._

package object controllers {

  val noRights = Rights(c = false, r = false, u = false, d = false, f = false)
  val readOnly = Rights(c = false, r = true, u = false, d = false, f = false)

//  case class ContextualizedRequest[A](user: models.User, override val request: Request[A]) extends WrappedRequest(request)
//
//  def Contextualized[A](p: BodyParser[A])(f: ContextualizedRequest[A] => Result) = {
//    Action(p) { request =>
//      request.session.get("user").flatMap(u => User.findByEmail(u)).map { user =>
//        f(ContextualizedRequest(user, request))
//      }.getOrElse(Results.Unauthorized)
//    }
//  }
//
//  // Overloaded method to use the default body parser
//  def Contextualized(f: ContextualizedRequest[AnyContent] => Result): Action[AnyContent]  = {
//    Contextualized(parse.anyContent)(f)
//  }
}
