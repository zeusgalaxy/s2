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

  val Home = Redirect(routes.Report.showWorkoutLocations(0, 1, "", ""))

  def showWorkoutLocations(page: Int, orderBy: Int, filter: String, startDate: String, endDate: String) = Action {
    implicit request =>

      Ok(html.listWorkoutLocations(
        WorkoutLocation.list(page = page, orderBy = orderBy, filter = filter, startDate = startDate, endDate = endDate ),                         // ("%" + filter + "%")
        // User.list(page = page, orderBy = orderBy, filter = ("%" + filter + "%")),
        orderBy, filter, startDate, endDate
      ))
  }


}
