package controllers

import play.api.mvc._
import views._
import models._

object Report extends Controller with Secured {

  def showWorkoutLocations(page: Int, orderBy: Int, filter: String, startDate: String, endDate: String) = IsAuthenticated("/s2/reports/WorkoutLocations",
    implicit request =>
           Ok(html.listWorkoutLocations(
              WorkoutLocation.list(page = page, orderBy = orderBy, filter = filter, startDate = startDate, endDate = endDate ),
                orderBy, filter, startDate, endDate)
           )
  )


}


