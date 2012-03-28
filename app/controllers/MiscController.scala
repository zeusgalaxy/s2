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

  val userForm: Form[User] = Form(
    mapping(
      "firstName" -> text,
      "lastName" -> text,
      "email" -> text,
      "newPass" -> optional(text)
      //      "newPassConf" -> text
    ) {
      // apply
      (firstName: String, lastName: String, email: String, newPass: Option[String]) => User(0, Some(firstName), Some(lastName), "", email)
    } {
      // UnApply
      user: User => Option(user.firstName.getOrElse(""), user.lastName.getOrElse(""), user.email, Option(""))
    }
  )

  def userEdit(id: Long) = IfCanUpdate(tgUsers) {
    implicit request =>
      User.findById(id) match {
        case Some(u) => Ok(html.userEdit(userForm.fill(u)))
        case _ => Redirect(routes.MiscController.index()).flashing("failure" -> ("An error occured."))
      }
  }

  /**
   * Handle form submission.
   */
  def userSubmit = IfCanUpdate(tgUsers) {
    implicit request => {
      userForm.bindFromRequest.fold(
        errors => BadRequest(html.userEdit(errors)),
        user => {
          User.update(user.id, user) match {
            case 1 => Ok(html.userEdit(userForm.fill(user))).flashing("Success" -> ("User updated."))
            case _ => Redirect(routes.MiscController.index()).flashing("failure" -> ("An error occured."))
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
