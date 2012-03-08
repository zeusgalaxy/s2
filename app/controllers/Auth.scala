package controllers

import play.api.mvc.Controller
import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

import views._
import models._
import utils._


object Auth extends Controller {

  val loginForm = Form(
    tuple(
      "email" -> text,
      "password" -> text
    ) verifying("Invalid email or password", result => result match {
      case (email, password) =>
        User.authenticate(email, password).isDefined
    })
  )

  
  /**
   * Login page.
   */
  def login(destPage: String = "") = Action {
    implicit request =>
      Ok(html.login(loginForm)).withSession(session + ("page" -> destPage))
  }

  /**
   * Handle login form submission.
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
              val ss = session + ("id" -> u.id.toString) + ("fname" -> u.firstName) + ("lname" -> u.lastName) + ("email" -> u.email) + ("cmpId" -> u.compId.toString) +("oemId" -> u.oemId.toString)
              Redirect(targetPage).withSession(ss)              
            }
            case _ => Redirect(routes.Auth.login("/index")).withNewSession.flashing(
              "error" -> "Problem logging in.")
          }
        }
      )
  }


  /**
   * Logout and clean the session.
   */
  def logout = Action {
    Redirect(routes.Auth.login("/index")).withNewSession.flashing(
      "success" -> "You've been logged out"
    )
  }

}


/**
 * Provide security features
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

  // WAS:
//  def IsAuthenticated(destPage: String, f: => User => Request[AnyContent] => Result) = Security.Authenticated(npAdminCookie, onUnauthorized(_)(destPage)) {
//     npCookieString =>
//       User.parseNpadminCookie(Option(Cookie("npadmin",npCookieString,0,"",None,true,false))) match {
//         case Some(u) =>
//             Action(request => f(u)(request))
//         case _ => {
//             Logger.error("IsAuthenticated: Problem with parsing the npadmin cookie into a User")
//             Action(request => f(new User)(request))
//           }
//        }

}