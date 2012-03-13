package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

import views._
import models._

/** Login form and actions
 *
 * Routed here whenever an action in a controller that extends secured is called and the user
 * is not yet logged in. When the user goes through login and is authorized, a session and context
 * are created for them.
  */
object Auth extends Controller {

  /** Authenticate against the DB the user's entry for email and password.
   *
   * email, User's email address
   * password, User's existing password in unencrypted text form
   * @return form verified boolean
   */
  val loginForm = Form(
    tuple(
      "email" -> text,
      "password" -> text
    ) verifying("Invalid email or password", result => result match {
      case (email, password) =>
        User.authenticate(email, password).isDefined
    })
  )
  
  /** Present the Login page to the user
   *
   * @param destPage The page the user requested. They were routed here because they weren't logged in.
   * This allows us to redirect to that desired page after they log in.
   */
  def login(destPage: String = "") = Action {
    implicit request =>
      Ok(html.login(loginForm)).withSession(session + ("page" -> destPage))
  }

  /** Handle login form submission.
   *
   * Authenticate the user by accepting their login form data. If it checks out add auth to their session. Remove
   * the auth items on login failure. Redirect them to their originally desired page after auth success.
   * @return side effect - route the user either to their requested page or back to index()
   */
  def authenticate() = Action {
    implicit request =>
      loginForm.bindFromRequest.fold(
        formWithErrors => {
          // Remove User & Auth items from session
          val sessionKeys = Seq("id", "fname", "lname", "email", "cmpId", "oemId")
          val ss = sessionKeys.foldLeft(session) { (s, p) => s - p }
          BadRequest(html.login(formWithErrors)).withSession(ss)
        }, 
        user => {
          // Prepare to go to user's desired page after login
          var targetPage = "/login"
          session.get("page") match {
            case Some(page) => targetPage = page
            case _ =>

          }
          // Add User & Auth items to session
          User.findByEmail(user._1) match {
            case Some(u) => {
              val ss = session + ("id" -> u.id.toString) + ("fname" -> u.firstName.getOrElse("") ) + ("lname" -> u.lastName.getOrElse("")) + ("email" -> u.email) + ("cmpId" -> u.compId.toString) +("oemId" -> u.oemId.toString)
              Redirect(targetPage).withSession(ss)              
            }
            case _ => Redirect(routes.Auth.login("/index")).withNewSession.flashing(
              "error" -> "Problem logging in.")
          }
        }
      )
  }

  /** Logout and clean the session.
   *
   * @return side effect route to /index with a nice success message.
   */
  def logout = Action {
    Redirect(routes.Auth.login("/index")).withNewSession.flashing(
      "success" -> "You've been logged out"
    )
  }

}  // End of Auth


/** Provide security features - TODO: ScalaDoc when complete.
 */
trait Secured {

  /**
   * Retrieve the connected user's email. If it's there they are already authenticated.
   */
  private def checkUserEmail(request: RequestHeader) = request.session.get("email")

  /**
   * Redirect to login if the user is not authorized.
   */
  private def onUnauthorized(request: RequestHeader)(destPage: String = "") = {
    Logger.info("onUnauthorized destPage =" + destPage)
    Results.Redirect(routes.Auth.login(destPage))
  }

  /**
   * Action for authenticated users.
   */
  // If the user's email is on the session they are already authorized.
  def IsAuthenticated(destPage: String, f: => Request[AnyContent] => Result) =
    Security.Authenticated(checkUserEmail, onUnauthorized(_)(destPage))  {
      emailOpt =>  emailOpt match {
        case email  =>  Action(request => f(request))
// unreachable
//        case "" => {
//                  Logger.error("IsAuthenticated: Problem with parsing the npadmin cookie into a User")
//                  // TODO: Route to index with error string?
//                  Action(request => f(request))
//        }
      }
    }


}