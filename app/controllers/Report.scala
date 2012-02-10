package controllers

import play.api.mvc._

import views._
import models.WorkoutLocation


object Report extends Controller {

  def showWorkoutLocations(page: Int, orderBy: Int, filter: String, startDate: String, endDate: String) = Action {
    implicit request =>

      Ok(html.listWorkoutLocations(
        WorkoutLocation.list(page = page, orderBy = orderBy, filter = filter, startDate = startDate, endDate = endDate ),     // ("%" + filter + "%")
        orderBy, filter, startDate, endDate
      ))
  }


}
