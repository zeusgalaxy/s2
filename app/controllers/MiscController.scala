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


  /*
 *
 *  User CRUD controllers, form etc.
 *        case class User(id: Long = 0, firstName: String="", lastName: String="", password: String="", email: String="",
               compId: Long=0,  oemId: Option[Long]=None, adId: Option[Long]=None  )
  */


  val userForm= Form(
    mapping(
      "firstName" -> optional(text),
      "lastName" -> optional(text),
      "email" -> email,
      "password" -> optional(text)
    )  (UserEdit.apply)(UserEdit.unapply)
  )

  def userEdit(id: Long) = IfCanUpdate(tgUsers) {
    implicit request =>
      User.findById(id) match {
        case Some(u) => {
          val uE = UserEdit(u.firstName, u.lastName, u.email, None)
          Ok(html.userEdit(id, userForm.fill(uE)))
        }
        case _ => Redirect(routes.MiscController.userList()).flashing("failure" -> ("An error occured."))
      }
  }

  /**
   * Handle form submission.
   */
  def userSubmit(id: Long) = IfCanUpdate(tgUsers) {
    implicit request => {
      userForm.bindFromRequest.fold(
        formErrors => BadRequest(html.userEdit(id, formErrors)),
        uE => {
          User.update(id, uE) match {
            case 1 =>
              Redirect(routes.MiscController.userList()).flashing("success" -> ("User: "+uE.email+" updated."))
            case _ =>
              Redirect(routes.MiscController.userList()).flashing("failure" -> ("An error occured."))
          }
        }
      )
    }
  }

  def userList(page: Int, orderBy: Int, filter: String) = IfCanRead(tgUsers) {
    implicit request =>
      Ok(html.userList(
        User.list(page = page, orderBy = orderBy, filter = ("%" + filter + "%")),
        orderBy, filter
      ))
  }
}
