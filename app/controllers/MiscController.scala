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
      "password" -> optional(text),
      "email" -> email
    )(UserEdit.apply)(UserEdit.unapply)
  )

  /** userEdit
   * Use the passed in ID value to get the user from the DB. Present that info in the form above.
   * @param id - User ID
   */
  def userEdit(id: Long) = IfCanUpdate(tgUsers) {
    implicit request =>
      User.findById(id).map(user => {
        Ok(html.userEdit(id, userForm.fill(UserEdit(user.firstName, user.lastName, None, user.email))))
      }).getOrElse(Redirect(routes.MiscController.userList()).flashing("failure" -> ("An error occured.")))
  }

  /** userEditSubmit
   * Handle form submission from an edit.
   * @param id User ID
   */
  def userEditSubmit(id: Long) = IfCanUpdate(tgUsers) {
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
  
  /** userAdd
   * Add a user.
   * @param - none
   */
  def userAdd() = IfCanUpdate(tgUsers) {
    implicit request => Ok(html.userEdit(-1, userForm))
  }

  /** userAddSubmit
   * Handle form submission from an edit.
   * @param - none
   */
  def userAddSubmit() = IfCanUpdate(tgUsers) {
    implicit request => {
      userForm.bindFromRequest.fold(
        formErrors => BadRequest(html.userEdit(-1, formErrors)),
        uE => User.insert(uE.toUser)
        match {
          case Some(u) =>
            Redirect(routes.MiscController.userList()).flashing("success" -> ("User added."))
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
