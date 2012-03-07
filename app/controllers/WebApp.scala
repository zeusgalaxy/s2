package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

import anorm._

import views._
import models._


object WebApp extends Controller with Secured {

  def index = IsAuthenticated("/index", user => implicit request =>
      Ok(html.index(user, "This is the main page parameter"))
  )

  def testLogin = IsAuthenticated("/testLogin", user => implicit request =>
    Ok(html.testLogin(user, "test string"))
  )


  /*
  *
  *  User CRUD controllers, form etc.
  *        case class User(id: Long = 0, firstName: String="", lastName: String="", password: String="", email: String="",
                compId: Long=0,  oemId: Option[Long]=None, adId: Option[Long]=None  )
   */

  val userForm: Form[User] = Form(
    mapping(
      "firstName" -> text,
      "lastName"  -> text,
      "email"     -> text
//      "newPass"   -> text,
//      "newPassConf" -> text
    ){ // apply
      (firstName, lastName, email) => User(0,firstName,lastName,"",email)
    }
     { // UnApply
       user => Some(user.firstName, user.lastName, user.email )
     }
  )

  def userEdit = IsAuthenticated("/userEdit", user => implicit request =>
    Ok(html.userEdit(user, userForm))
  )

//    def userEdit = Action {
//      Ok(html.userEdit.form(userForm));
//    }

  /**
   * Handle form submission.
   */
  def userSubmit = IsAuthenticated("/userEdit", user => implicit request =>
    userForm.bindFromRequest.fold(
      errors => BadRequest(html.userEdit(user, errors)),
      user =>  Redirect(routes.WebApp.index).flashing(
        "success" -> ("User information updated: "+user.toString)
      )
        // Ok(html.userEdit(user, userForm))
    )
  )

  //  "id" -> Long,
  //    (
//      (id, firstName ) => User(id, firstName ),
//      (user: User) => (user.firstName)
//     )
//
  //      "lastName" ->  text,
  //      "password" -> nonEmptyText (6),
  //      "email" -> nonEmptyText()
  //      "compId" -> ignored (Long),
  //      "oemId" -> ignored(Option(Long)),
  //      "adId" -> ignored(Option(Long))
      //(User.apply)(User.unapply)



}
