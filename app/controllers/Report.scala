package controllers

import play.api.mvc._

import views._
import models.WorkoutLocation
import play.api.Logger
import utils._

object Report extends Controller with Secured {
  // val token = play.api.libs.Crypto.sign("myem...@example.com")
  def showWorkoutLocations(page: Int, orderBy: Int, filter: String, startDate: String, endDate: String) = Action {
    implicit request => {

      Logger.info("Session :"+session.data.toString())
      //
      // Get the company filter from the npadmin cookie.
      //
      request.cookies.get("npadmin") match {
        case Some(c) => {
          val xmlStr = new DesEncrypter(DesEncrypter.SESSION_SECRET_KEY).decrypt(c.value.replace("\\r", "\r").replace("\\n", "\n"))
          Logger.info( "npadmin cookie = "+c+"\npassed value="+c.value+"\nresult="+xmlStr)
          // <adminUser id="89" compId="1" oemId="null" adId="null" email="dfaust@netpulse.com"></adminUser>
          (scala.xml.XML.loadString(xmlStr) \  "@compId").toString match {
            case "1"  =>                                                        // Only allow Netpulse users for now
              Ok(html.listWorkoutLocations(
                WorkoutLocation.list(page = page, orderBy = orderBy, filter = filter, startDate = startDate, endDate = endDate ),     // ("%" + filter + "%")
                orderBy, filter, startDate, endDate
              ))
            case _ => Forbidden
          }
        }
        case None => Forbidden 
      }      
    }
  }


}
