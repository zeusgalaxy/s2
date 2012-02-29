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
    ) verifying ("Invalid email or password", result => result match {
      case (email, password) => true // User.authenticate(email, password).isDefined
    })
  )

  /**
   * Login page.
   */
  def login(destPage:String="") = Action { implicit request =>
    Logger.info("login: dest page: "+destPage)
    session.get("page").map {p => Logger.info("login session page: "+p) }
    Ok(html.login(loginForm)).withSession( session + ("page" -> destPage) )
  }

  /**
   * Handle login form submission.
   */
  def authenticate() = Action { implicit request =>
    session.get("page").map {p => Logger.info("auth session page: "+p) }

    loginForm.bindFromRequest.fold(
      formWithErrors => BadRequest(html.login(formWithErrors)),
      user => {
        var targetPage = "/login"
        session.get("page") match {
          case Some(page) =>  targetPage = page
          case _ => targetPage = "/index"
        }
        Redirect(targetPage).withSession("email" -> user._1) 
      }
    )
  }


  /**
   * Logout and clean the session.
   */
  def logout = Action {
    Redirect(routes.Application.index).withNewSession.flashing(
      "success" -> "You've been logged out"
    )
  }


}


/**
 * Provide security features
 */
trait Secured {

  /**
   * Retrieve the connected user email.
   */
  private def username(request: RequestHeader) = request.session.get("email")

  /**
   * Redirect to login if the user is not authorized.
   */
  private def onUnauthorized(request: RequestHeader)(destPage: String="") = {
    Logger.info("onUnauthorized destPage ="+destPage)
    Results.Redirect(routes.Auth.login(destPage))
  }

  /**
   * Action for authenticated users.
   */
  def IsAuthenticated(destPage:String, f: => String => Request[AnyContent] => Result) = Security.Authenticated(username, onUnauthorized(_)(destPage)) { user =>
    Action(request => f(user)(request))
  }

}