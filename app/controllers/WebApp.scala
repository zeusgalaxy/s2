package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

import anorm._

import views._
import models._


object WebApp extends Controller with Secured {

  def index = IsAuthenticated("/index",  implicit request =>
      Ok(html.index("This is the main page parameter"))
  )

  def testLogin = IsAuthenticated("/testLogin", implicit request =>
    Ok(html.testLogin("test string"))
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
      "email"     -> nonEmptyText,
      "newPass"   -> text(6)
//      "newPassConf" -> text
    ){ // apply
      (firstName, lastName, email, newPass) => User(0,firstName,lastName,"",email)
    }
     { // UnApply
       user => Some(user.firstName, user.lastName, user.email, "" )
     }
  )

  def userEdit = IsAuthenticated("/userEdit", implicit request =>
    {
      session.get("id") match {
        case Some(id) =>
          User.findById(id.toLong)match {
            case Some(u) => Ok(html.userEdit(userForm.fill(u)))
            case _ =>  Redirect(routes.WebApp.index).flashing("failure" -> ("An error occured."))
          }
        case _ =>  Redirect(routes.WebApp.index).flashing("failure" -> ("An error occured."))
      }
    }
  )

//    def userEdit = Action {
//      Ok(html.userEdit.form(userForm));
//    }

  /**
   * Handle form submission.
   */
  def userSubmit = IsAuthenticated("/userEdit", implicit request =>
    userForm.bindFromRequest.fold(
      errors => BadRequest(html.userEdit(errors)),
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
