package controllers

import play.api.mvc._
import views._
import models._

object Report extends Controller with Secured {

  def showWorkoutLocations(page: Int, orderBy: Int, filter: String, startDate: String, endDate: String) =  Action {
    implicit request => {
        User.parseNpadminCookie(request.cookies.get("npadmin")) match {
         case Some(u) if (u.compId == 1) =>      // Internal Users
           Ok(html.listWorkoutLocations(
              WorkoutLocation.list(page = page, orderBy = orderBy, filter = filter, startDate = startDate, endDate = endDate ),
                orderBy, filter, startDate, endDate )
           )
          case Some(u) =>                       // External Users
            Ok(html.listWorkoutLocations(
              WorkoutLocation.list(page = page, orderBy = orderBy, filter = u.compId.toString, startDate = startDate, endDate = endDate ),
              orderBy, filter, startDate, endDate )
            )
      }
    }
  }


}
