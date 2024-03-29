import utils._
import controllers._
import models._
import views._
import play.api.Logger
import play.api.mvc._
import play.api.mvc.Results._
import play.api.mvc.BodyParsers._

import scalaz.{Logger => _, _}

/**The security package object provides the common facilities for creating and executing
 * rights-managed requests. This is done by wrapping a normal Request in a specialized
 * subclass -- CtxRqst -- which has knowledge of the user who is logged in (if any)
 * as well as the CRUD rights associated with that user for the target currently being
 * operated on.
 *
 * Targets are encapsulated in the [[security.Target]] class, so we can use them as implicits
 * if desired, and we can alter their representation in the future, if needed. For now, they
 * are simply strings, which represent areas of functionality that need to be controlled
 * from a security perspective.
 *
 */
package object security {

  /**Defines a template for the basic rights operations -- CRUD -- as well as the notion
   * of "filtering," which indicates whether or not the application needs to constrain the
   * data that this user sees. If so, the meaning of that constraint (i.e, of that filtering)
   * is entirely up to the application.
   *
   */
  trait Can {
    def canCreate = false

    def canRead = false

    def canUpdate = false

    def canDelete = false

    def isFiltered = false
  }

  val noRights = new Rights(c = 0, r = 0, u = 0, d = 0, f = 0)
  val readOnly = new Rights(c = 0, r = 1, u = 0, d = 0, f = 0)

  // Some example targets have been predefined. These can be fleshed out as we define the application.
  val tgNone = Target("")
  val tgReports = Target("mySpecificReport")
  val tgAdminPortal = Target("adminPortal")
  val tgUser = Target("user")
  val tgTest = Target("test")
  val tgTestA = Target("test-a")
  val tgTestB = Target("test-b")
  val tgReportWorkoutLocations = Target("reportWorkoutLocations")

  /**Subclasses and wraps a normal request so we can associate some "context" with the request.
   * [[security.Context]] at a minimum will contain the current user (if any) as well as the
   * rights that apply to this request. This context is queried by the helper functions that
   * control access on actions, and it can also be queried by views, controllers, etc., to access
   * the user's name, etc., since this request is passed down the chain.
   *
   */
  // (Ignore the IntelliJ syntax error on request, below.)
  case class CtxRqst[A](context: Context, request: Request[A]) extends WrappedRequest(request) with RightsDao {

    def canCreate = context.rights.canCreate

    def canCreate(t: Target) = rtGet(context.user, t).canCreate

    def canRead = context.rights.canRead

    def canRead(t: Target) = rtGet(context.user, t).canRead

    def canUpdate = context.rights.canUpdate

    def canUpdate(t: Target) = rtGet(context.user, t).canUpdate

    def canDelete = context.rights.canDelete

    def canDelete(t: Target) = rtGet(context.user, t).canDelete

    def isFiltered = context.rights.isFiltered

    def isFiltered(t: Target) = rtGet(context.user, t).isFiltered

  }

  /**Companion object for creating a [[security.CtxRqst]] from a normal request. The
   * application-defined [[security.Target]] must be passed in, so we know what realm of
   * functionality defines the rights that should be granted. The current user will be
   * extracted from the session cookie, if any.
   *
   */
  object CtxRqst extends models.PersonDao {
    def apply[A](target: Target, request: Request[A]): CtxRqst[A] = {
      vld {
        request.session.get("id").flatMap(pid => prFindById(pid.toInt)).map {
          user => CtxRqst(Context(user, target), request)
        }.getOrElse(CtxRqst(Context(None, noRights), request))
      }.fold(e => CtxRqst(Context(None, noRights), request), s => s)
    }
  }

  /**Helper function for attaching context-related data to the session, before returning
   * the final result.
   *
   * @param ctx The contextualized request which embeds necessary user data, if any
   * @param r The result, before attaching the session data
   * @return The result with session data attached
   */
  def withSession[A](ctx: CtxRqst[A], r: PlainResult): PlainResult = {
    val u = ctx.context.user
    val ss = ("id" -> u.map(_.id.toString).getOrElse(""))
    Logger.debug("in withSession, on page " + ctx.request.path + ", the user is: " + ctx.context.user.toString)
    r.withSession(ss) // TODO -- Figure out what really should be attached to session here
  }

  /**Helper function for clearing all session data, before returning
   * the final result.
   *
   * @param r The result, before clearing the session data
   * @return The result with session data cleared
   */
  def withoutSession(r: PlainResult): PlainResult = {
    r.withNewSession
  }

  /**Helper function for genericizing the inspection of a context to check for certain
   * rights. Intended to be called as partially-applied function by the various versions of
   * Action creation functions.
   *
   * @param ctx The context to be inspected for "create" right
   * @return Whether the "create" right is being granted
   */
  def create[A](ctx: CtxRqst[A]) = ctx.canCreate

  /**Helper function for genericizing the inspection of a context to check for certain
   * rights. Intended to be called as partially-applied function by the various versions of
   * Action creation functions.
   *
   * @param ctx The context to be inspected for "read" right
   * @return Whether the "read" right is being granted
   */
  def read[A](ctx: CtxRqst[A]) = ctx.canRead

  /**Helper function for genericizing the inspection of a context to check for certain
   * rights. Intended to be called as partially-applied function by the various versions of
   * Action creation functions.
   *
   * @param ctx The context to be inspected for "update" right
   * @return Whether the "update" right is being granted
   */
  def update[A](ctx: CtxRqst[A]) = ctx.canUpdate

  /**Helper function for genericizing the inspection of a context to check for certain
   * rights. Intended to be called as partially-applied function by the various versions of
   * Action creation functions.
   *
   * @param ctx The context to be inspected for "delete" right
   * @return Whether the "delete" right is being granted
   */
  def delete[A](ctx: CtxRqst[A]) = ctx.canDelete

  /**Generalized helper function that will create a [[play.api.mvc.Action]] which will
   * inspect the request context (using the supplied partially-applied inspection function)
   * to see whether this user has the rights to access the given target.
   * If so, the action performs the given function; if not, it returns an Unauthorized result.
   *
   * @param ok The partially-applied rights inspection method to be invoked
   * @param p The body parser to be used in processing the request
   * @param target The security target for which rights are being inspected
   * @param f The function that yields a [[play.api.mvc.SimpleResult]]
   * @return The [[play.api.mvc.SimpleResult]]
   */
  def IfCan[A](ok: (CtxRqst[A]) => Boolean, p: BodyParser[A], target: Target,
               f: CtxRqst[A] => PlainResult): Action[A] = {
    Action(p) {
      request =>
        println("Session in IfCan is: " + request.session.toString)
        val ctxReq = CtxRqst(target, request)
        ctxReq.context.user match {
          case Some(u) => if (ok(ctxReq)) withSession(ctxReq, f(ctxReq))
          else
            Results.Redirect(routes.AuthController.unauthorized())
          case _ => Results.Redirect(routes.AuthController.promptLogin(request.path))
        }
    }
  }

  /**Convenience function for wrapping secured actions in controllers. This version
   * will work with an explictly-provided BodyParser.
   *
   * Example usage:
   *
   * def restrictedHello = IfCanCreate(Target("hello")) { implicit request =>
   * Ok(html.kenner("Hello " + request.context.user.get.firstName.get))
   * }
   *
   * Refer to [[security.IfCan]] for explanation of parameters.
   */
  def IfCanCreate[A](p: BodyParser[A])(target: Target)(f: CtxRqst[A] => PlainResult): Action[A] =
    IfCan(create[A] _, p, target, f)

  /**Convenience function for wrapping secured actions in controllers. This version uses
   * the default AnyContent body parser.
   *
   * Example usage:
   *
   * def restrictedHello = IfCanCreate(parse.xml)(Target("hello")) { implicit request =>
   * Ok(html.kenner("Hello " + request.context.user.get.firstName.get))
   * }
   *
   * Refer to [[security.IfCan]] for explanation of parameters.
   */
  def IfCanCreate(target: Target)(f: CtxRqst[AnyContent] => PlainResult): Action[AnyContent] =
    IfCan(create[AnyContent] _, parse.anyContent, target, f)

  /**Convenience function for wrapping secured actions in controllers. This version
   * will work with an explictly-provided BodyParser.
   *
   * Example usage:
   *
   * def restrictedHello = IfCanRead(Target("hello")) { implicit request =>
   * Ok(html.kenner("Hello " + request.context.user.get.firstName.get))
   * }
   *
   * Refer to [[security.IfCan]] for explanation of parameters.
   */
  def IfCanRead[A](p: BodyParser[A])(target: Target)(f: CtxRqst[A] => PlainResult): Action[A] =
    IfCan(read[A] _, p, target, f)

  /**Convenience function for wrapping secured actions in controllers. This version uses
   * the default AnyContent body parser.
   *
   * Example usage:
   *
   * def restrictedHello = IfCanRead(parse.xml)(Target("hello")) { implicit request =>
   * Ok(html.kenner("Hello " + request.context.user.get.firstName.get))
   * }
   *
   * Refer to [[security.IfCan]] for explanation of parameters.
   */
  def IfCanRead(target: Target)(f: CtxRqst[AnyContent] => PlainResult): Action[AnyContent] =
    IfCan(read[AnyContent] _, parse.anyContent, target, f)

  /**Convenience function for wrapping secured actions in controllers. This version
   * will work with an explictly-provided BodyParser.
   *
   * Example usage:
   *
   * def restrictedHello = IfCanUpdate(Target("hello")) { implicit request =>
   * Ok(html.kenner("Hello " + request.context.user.get.firstName.get))
   * }
   *
   * Refer to [[security.IfCan]] for explanation of parameters.
   */
  def IfCanUpdate[A](p: BodyParser[A])(target: Target)(f: CtxRqst[A] => PlainResult): Action[A] =
    IfCan(update[A] _, p, target, f)

  /**Convenience function for wrapping secured actions in controllers. This version uses
   * the default AnyContent body parser.
   *
   * Example usage:
   *
   * def restrictedHello = IfCanUpdate(parse.xml)(Target("hello")) { implicit request =>
   * Ok(html.kenner("Hello " + request.context.user.get.firstName.get))
   * }
   *
   * Refer to [[security.IfCan]] for explanation of parameters.
   */
  def IfCanUpdate(target: Target)(f: CtxRqst[AnyContent] => PlainResult): Action[AnyContent] =
    IfCan(update[AnyContent] _, parse.anyContent, target, f)

  /**Convenience function for wrapping secured actions in controllers. This version
   * will work with an explictly-provided BodyParser.
   *
   * Example usage:
   *
   * def restrictedHello = IfCanDelete(Target("hello")) { implicit request =>
   * Ok(html.kenner("Hello " + request.context.user.get.firstName.get))
   * }
   *
   * Refer to [[security.IfCan]] for explanation of parameters.
   */
  def IfCanDelete[A](p: BodyParser[A])(target: Target)(f: CtxRqst[A] => PlainResult): Action[A] =
    IfCan(delete[A] _, p, target, f)

  /**Convenience function for wrapping secured actions in controllers. This version uses
   * the default AnyContent body parser.
   *
   * Example usage:
   *
   * def restrictedHello = IfCanDelete(parse.xml)(Target("hello")) { implicit request =>
   * Ok(html.kenner("Hello " + request.context.user.get.firstName.get))
   * }
   *
   * Refer to [[security.IfCan]] for explanation of parameters.
   */
  def IfCanDelete(target: Target)(f: CtxRqst[AnyContent] => PlainResult): Action[AnyContent] =
    IfCan(delete[AnyContent] _, parse.anyContent, target, f)


  /**Convenience function for wrapping an unsecured action in a controller. The reason that an action
   * might want this capability -- even though it wasn't secured -- is that it gets a request
   * that includes context such as the logged in user.
   *
   * This version will work with an explictly-provided body parser.
   *
   * Example usage:
   *
   * def restrictedHello = Unrestricted(parse.xml)(Target("hello")) { implicit request =>
   * Ok(html.kenner("Hello " + request.context.user.get.firstName.get))
   * }
   *
   * Refer to [[security.IfCan]] for explanation of parameters.
   */
  def Unrestricted[A](p: BodyParser[A])(f: CtxRqst[A] => PlainResult): Action[A] = {
    Action(p) {
      request =>
        val ctxReq = CtxRqst(tgNone, request)
        withSession(ctxReq, f(ctxReq))
    }
  }

  /**Convenience function for wrapping an unsecured action in a controller. The reason that an action
   * might want this capability -- even though it wasn't secured -- is that it gets a request
   * that includes context such as the logged in user.
   *
   * This version uses the default AnyContent body parser.
   *
   * Example usage:
   *
   * def restrictedHello = Unrestricted(Target("hello")) { implicit request =>
   * Ok(html.kenner("Hello " + request.context.user.get.firstName.get))
   * }
   *
   * Refer to [[security.IfCan]] for explanation of parameters.
   */
  def Unrestricted(f: CtxRqst[AnyContent] => PlainResult): Action[AnyContent] =
    Unrestricted(parse.anyContent)(f)
}
