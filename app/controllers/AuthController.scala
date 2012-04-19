package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

import security._
import views._
import models._

/**
 * Controller for managing the user login and logout processes
 */
object AuthController extends Controller with PersonDao {

  /**Form for entering a user's email and password for login purposes.
   *
   * email, User's email address
   * password, User's existing password in unencrypted text form
   */
  val loginForm = Form(
    tuple(
      "email" -> text,
      "password" -> text
    ) verifying("Invalid email or password", result => result match {
      case (email, password) =>
        prAuthenticate(email, password).isDefined
    })
  )

  /**Presents the login form to the user, so they can enter their user name and password.
   *
   * @param destPage The page the user had requested before they were diverted into the login sequence.
   */
  def promptLogin(destPage: String = "/index") = Unrestricted {
    implicit request =>
      Ok(html.login(loginForm, destPage))
  }

  /**Presents the "you're not authorized" page to the user.
   *
   */
  def unauthorized() = Unrestricted {
    implicit request =>
      Ok(html.unauthorized.apply)
  }

  /**Handles login form submission
   *
   * Handles the login form submission by validating the submitted user name and password against the
   * database, and if okay, beginning a logged in user session. If successful, they will be forwarded to
   * whatever page they were requesting when they diverted to the login process.
   *
   * @param destPage The page the user had requested before they were diverted into the login sequence.
   * @return Errors on the login prompt page (if problems), else the page they were originally headed to.
   */
  def attemptLogin(destPage: String = "/index") = Unrestricted {
    implicit request =>
      loginForm.bindFromRequest.fold(
        formWithErrors => {
          BadRequest(html.login(formWithErrors, destPage))
        },
        user => {
          prFindByLogin(user._1) match {
            // we use their e-mail address as their login id
            case Some(u) if (u.isActive) => {
              request.context.user = Some(u)
              Logger.debug("We have user: " + u.firstName + " in AuthController.attemptLogin")
              Redirect(destPage)
            }
            case _ =>
              Logger.debug("We failed to get an active person for " + user._1)
              Redirect(routes.AuthController.promptLogin(destPage)).withNewSession.flashing(
                "error" -> "Problem logging in.")
          }
        }
      )
  }

  /**Logs out the current user by setting the sessions user variable to None.
   *
   * @return The index (home) page with a message showing that they logged out successfully
   */
  def logout = Unrestricted {
    implicit request =>
      request.context.user = None
      Redirect(routes.AuthController.promptLogin("/index")).flashing(
        "success" -> "You've been logged out"
      )
  }

}
