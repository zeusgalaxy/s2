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
      case (email, password) => User.authenticate(email, password).isDefined
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
        formWithErrors => BadRequest(html.login(formWithErrors)),
        user => {
          var targetPage = "/login"
          session.get("page") match {
            case Some(page) => targetPage = page
            case _ =>
          }
          Redirect(targetPage).withSession("npadmin" -> user._1)              // COOKIE SET HERE
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
  private def onUnauthorized(request: RequestHeader)(destPage: String = "") = {
    Logger.info("onUnauthorized destPage =" + destPage)
    Results.Redirect(routes.Auth.login(destPage))
  }

  /**
   * Action for authenticated users.
   */
  def IsAuthenticated(destPage: String, f: => String => Request[AnyContent] => Result) = Security.Authenticated(username, onUnauthorized(_)(destPage)) {
    user =>
      Action(request => f(user)(request))
  }

}