package controllers

import play.api.mvc._
import play.api.libs.ws._
import play.api.Play.current

import views._
import models.WorkoutLocation
import models.User

object Report extends Controller {


  def index = Action {
    implicit request =>
      Ok(html.index("This is the main page parameter"))
  }

  val Home = Redirect(routes.Report.showWorkoutLocations(0, 2, ""))

  def showWorkoutLocations(page: Int, orderBy: Int, filter: String) = Action {
    implicit request =>

      Ok(html.listWorkoutLocations(
        WorkoutLocation.list(page = page, orderBy = orderBy, filter = filter ),                         // ("%" + filter + "%")
        // User.list(page = page, orderBy = orderBy, filter = ("%" + filter + "%")),
        orderBy, filter
      ))
  }


}
