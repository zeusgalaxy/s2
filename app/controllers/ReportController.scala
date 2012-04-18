package controllers

import play.api.mvc._
import views._
import models._
import security._

/**Controller for reports
 *
 * Handle report requests
 */
object ReportController extends ReportController
                          with CompanyDao

class ReportController extends Controller {
  this: Controller with CompanyDao =>

  def showWorkoutLocations(page: Int, orderBy: Int, filter: String, startDate: String, endDate: String) =
    IfCanRead(tgReportWorkoutLocations) {
      implicit request => {
        val maybePage = WorkoutLocation.list(
          page = page,
          orderBy = orderBy,
          // If this is not a netpulse user limit their company selection here
          filter = if (!request.isFiltered) filter else request.context.user.get.companyId.toString,
          startDate = startDate,
          endDate = endDate)

        maybePage match {
          case Some(p) =>
            Ok(html.listWorkoutLocations(this, p, orderBy, filter, startDate, endDate))
          case _ => Redirect(routes.MiscController.index()).flashing("failure" -> ("An error occured."))
        }
      }
    }
}


