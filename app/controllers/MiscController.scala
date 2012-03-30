package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

import anorm._

import views._
import models._
import security._

object MiscController extends Controller {

  def restrictedHello = IfCanRead(tgReports) {
    implicit request =>
      println("we were allowed in to restrictedHello")
      Ok(html.kenner("Hello " + request.context.user.get.firstName.get))
  }

  def unrestrictedHello = Unrestricted {
    implicit request =>
      Ok(html.kenner("Hello Everybody!"))
  }

  def index = Unrestricted {
    implicit request =>
      Ok(html.index("This is the main page parameter"))
  }



  /** userForm
   * This is a special form to user for user editing since we don't want to use
   * the full User case class.
   */
  val userForm = Form(
    mapping(
      "firstName" -> optional(text),
      "lastName" -> optional(text),
      "email" -> email,
      "password" -> optional(text)
    )(UserEdit.apply)(UserEdit.unapply)
  )

  /** userEdit
   * Use the passed in ID value to get the user from the DB. Present that info in the form above.
   * @param id - User ID
   */
  def userEdit(id: Long) = IfCanUpdate(tgUsers) {
    implicit request =>
      User.findById(id).map(user => {
        Ok(html.userEdit(id, userForm.fill(UserEdit(user.firstName, user.lastName, user.email, None))))
      }).getOrElse(Redirect(routes.MiscController.userList()).flashing("failure" -> ("An error occured.")))
  }

  /** userSubmit
   * Handle form submission from an edit.
   * @param id User ID
   */
  def userSubmit(id: Long) = IfCanUpdate(tgUsers) {
    implicit request => {
      userForm.bindFromRequest.fold(
        formErrors => BadRequest(html.userEdit(id, formErrors)),
        uE => User.update(id, uE)
          match {
            case 1 =>
              Redirect(routes.MiscController.userList()).flashing("success" -> ("User: " + uE.email + " updated."))
            case _ =>
              Redirect(routes.MiscController.userList()).flashing("failure" -> ("An error occured. Make sure the email address is unique."))
          }
      )
    }
  }

  /**userList
   *
   * Display a page by page listing of the the admin users
   * @param page - which page of paginated results to display
   * @param orderBy - column to sort by
   * @param filter - partial last name to use as a match string
   */
  def userList(page: Int, orderBy: Int, filter: String) = IfCanRead(tgUsers) {
    implicit request =>
      Ok(html.userList(
        User.list(page = page, orderBy = orderBy, filter = ("%" + filter + "%")),
        orderBy, filter
      ))
  }
}
