package controllers

import play.api.mvc._
import views._
import models._
import security._

/**Controller for reports
 *
 * Handle report requests
 */
object ReportController extends Controller {

  def showWorkoutLocations(page: Int, orderBy: Int, filter: String, startDate: String, endDate: String) =
    IfCanRead(tgReportWorkoutLocations) {
      implicit request => {
        val maybePage = WorkoutLocation.list(
          page = page,
          orderBy = orderBy,
          filter = filter,
          startDate = startDate,
          endDate = endDate)

        maybePage match {
          case Some(p) =>
            Ok(html.listWorkoutLocations(p, orderBy, filter, startDate, endDate))
          case _ => Redirect(routes.MiscController.index()).flashing("failure" -> ("An error occured."))
        }
      }
    }
}


