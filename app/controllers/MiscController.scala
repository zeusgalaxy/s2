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

  def helloForA = IfCanRead(tgTestA) {
    implicit request =>
      println("we were allowed in to helloForA")
      Ok(html.kenner("Hello (A)" + request.context.user.get.firstName))
  }

  def helloForB = IfCanRead(tgTestB) {
    implicit request =>
      println("we were allowed in to helloForB")
      Ok(html.kenner("Hello (B)" + request.context.user.get.firstName))
  }

  def unrestrictedHello = Unrestricted {
    implicit request =>
      Ok(html.kenner("Hello Everybody!"))
  }

  def index = Unrestricted {
    implicit request =>
      Ok(html.index("This is the main page parameter"))
  }



  /** This is a special form to user for user case class editing since we don't want to use
   * the full User case class.
   */
  val personForm = Form(
    mapping(
      "firstName"   -> optional(text),
      "lastName"    -> optional(text),
      "portalLogin" -> optional(text),
      "password"    -> optional(text),
      "email"       -> email
    )(PersonEdit.apply)(PersonEdit.unapply)
  )

  /** Use the passed in ID value to get the user from the DB. Present that info in the form above.
   * @param id - User ID
   */
  def userEdit(id: Long) = Unrestricted {             // IfCanUpdate(tgUsers)
    implicit request =>
      Person.findById(id).map(user => {
        Ok(html.userEdit(id, personForm.fill(PersonEdit(Some(user.firstName), Some(user.lastName), None, None, user.email))))
      }).getOrElse(Redirect(routes.MiscController.userList()).flashing("failure" -> ("An error occurred.")))
  }

  /** Controller to Handle form submission from an edit.
   * @param id User ID
   */
  def userEditSubmit(id: Long) = Unrestricted {        //    IfCanUpdate(tgUsers)
    implicit request => {
      personForm.bindFromRequest.fold(
        formErrors => BadRequest(html.userEdit(id, formErrors)),
        uE => Person.update(id, uE)
        match {
          case 1 =>
            Redirect(routes.MiscController.userList()).flashing("success" -> ("User: " + uE.email + " updated."))
          case _ =>
            Redirect(routes.MiscController.userList()).flashing("failure" -> ("An error occurred. Make sure the email address is unique."))
        }
      )
    }
  }
  
  /** Controller for Adding a user.
   * @param - none
   */
  def userAdd() = Unrestricted {              // IfCanUpdate(tgUsers)
    implicit request => {
      Logger.debug("in userAdd controller")
      Ok(html.userEdit(-1, personForm))
    } 
  }

  /** Handle form submission from an edit.
   * @param - none
   */
  def userAddSubmit() = Unrestricted {                // IfCanUpdate(tgUsers)
    implicit request => {
      personForm.bindFromRequest.fold(
        formErrors => BadRequest(html.userEdit(-1, formErrors)),
        pE => {
          Logger.debug("UserAddSubmit uE to be added: "+pE.toString)
          Logger.debug("UserAddSubmit user from uE: "+pE.toPerson)
          Person.insert(pE.toPerson)  match {
            case Some(u) =>
              Redirect(routes.MiscController.userList()).flashing("success" -> ("User added."))
            case _ =>
              Redirect(routes.MiscController.userList()).flashing("failure" -> ("An error occurred. Make sure the email address is unique."))
            }
        }
      )
    }
  }
  
  /** Display a page by page listing of the the admin users
   * @param page - which page of paginated results to display
   * @param orderBy - column to sort by
   * @param filter - partial last name to use as a match string
   */
  def userList(page: Int, orderBy: Int, filter: String) = Unrestricted {      // = IfCanRead(tgUsers)
    implicit request =>
      Ok(html.userList(
        Person.list(page = page, orderBy = orderBy, filter = ("%" + filter + "%")),
        orderBy, filter
      ))
  }
}
