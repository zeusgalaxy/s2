package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

import anorm._

import views._
import models._
import security._
import org.joda.time.DateTime

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
    implicit request => {
      // Insert SQL test code here to see in log.
      Ok(html.index("This is the main page parameter: " ))
    }
  }



  /** This is a special form to user for user case class editing since we don't want to use
   * the full User case class.
   */
  val personForm = Form(
    mapping(
      "id"          -> ignored(-1L),
      "companyId"   -> longNumber,
      "roleId"      -> longNumber,
      "firstName"   -> text,
      "lastName"    -> text,
      "portalLogin" -> nonEmptyText,
      "password"    -> optional(text),
      "email"       -> email,
      "phone"       -> text
    )(Person.apply)(Person.unapply)
  )

  /** Use the passed in ID value to get the user from the DB. Present that info in the form above.
   * @param id - User ID
   */
  def userEdit(id: Long) = IfCanUpdate(tgUser) {
    implicit request =>
      Person.findById(id).map(user => {
        Ok(html.userEdit(id, personForm.fill(user)) )
      }).getOrElse(Redirect(routes.MiscController.userList()).flashing("failure" -> ("An error occurred.")))
  }

  /** Controller to Handle form submission from an edit.
   * @param id User ID
   */
  def userEditSubmit(id: Long ) = IfCanUpdate(tgUser) {
    implicit request => {
      personForm.bindFromRequest.fold(
        formErrors => BadRequest(html.userEdit(id, formErrors)),
        person =>
          Person.update(
            id,
            person,
            // Control the companyId and roleId values here based on user rights
            if (request.isFiltered) request.context.user.get.companyId else person.companyId,
            if (request.canCreate(security.tgUser)) request.context.user.get.roleId else person.roleId,
            request.context.user.get.id
          )
        match {
          case 1 =>
            Redirect(routes.MiscController.userList()).flashing("success" -> ("User: " + person.email + " updated."))
          case _ =>
            Redirect(routes.MiscController.userList()).flashing("failure" -> ("An error occurred. Make sure the email address is unique."))
        }
      )
    }
  }
  
  /** Controller for Adding a user.
   */
  def userAdd() = IfCanUpdate(tgUser) {
    implicit request => {
      Ok(html.userEdit(-1, personForm))
    } 
  }

  /** Handle form submission from an add.
   */
  def userAddSubmit() = IfCanUpdate(tgUser) {
    implicit request => {
      personForm.bindFromRequest.fold(
        formErrors => BadRequest(html.userEdit(-1, formErrors)),
        person => {
          Logger.debug("UserAddSubmit to be add: "+person)
          Person.insert(
              person,
              // If isFiltered put the admin user's company_id into the case class to be added
              if (request.isFiltered) request.context.user.get.companyId else person.companyId,
              request.context.user.get.id
          )  match {               // (companyId = adminUser.get.companyId, roleId = adminUser.get.roleId),
            case Some(u) =>
              Redirect(routes.MiscController.userList()).flashing("success" -> ("User " + person.portalLogin+ " added."))
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
  def userList(page: Int, orderBy: Int, filter: String) = IfCanRead(tgUser) {
    implicit request =>
      Ok(html.userList(
        // If the user is filtered pass Person.list their company info to limit their access here.
        Person.list(page = page, orderBy = orderBy, userFilter = ("%" + filter + "%"),  companyFilter = if (!request.isFiltered) "" else request.context.user.get.companyId.toString  ),
        orderBy, filter
      ))
  }
}
