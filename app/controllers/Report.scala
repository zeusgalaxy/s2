package controllers

import play.api.mvc._
import play.api.libs.ws._
import play.api.Play.current

import views._
import reports._
import models.User

object Report extends Controller {


  def index = Action {
    implicit request =>
      Ok(html.index("This is the main page parameter"))
  }

  val Home = Redirect(routes.Report.listWorkoutLocations(0, 2, ""))

  def listWorkoutLocations(page: Int, orderBy: Int, filter: String) = Action {
    implicit request =>

      Ok(html.listUsers(
        User.list(page = page, orderBy = orderBy, filter = ("%" + filter + "%")),
        orderBy, filter
      ))
  }


}
