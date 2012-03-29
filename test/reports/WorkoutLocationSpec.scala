package test

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import play.api.test.FakeRequest._
import play.api.Logger
import utils._
import org.specs2.execute.PendingUntilFixed
import play.api.mvc._
import models.WorkoutLocation

object WorkoutLocationSpec extends Specification {

  /***
   *  Controller Tests
   *
   */
  // TODO: Need to be able to test authenticated Actions HERE

//  "Rejected if not authorized" in {
//    val result = controllers.Auth.login()(FakeRequest())
//
//    status(result) must equalTo(OK)
//    contentType(result) must beSome("text/html")
//    charset(result) must beSome("utf-8")
//    contentAsString(result) must contain("Hello Bob")
//  }

  
  
  /***
   *  Model Tests
   *
   */

  "Verify no data is returned without a company, then verify past netpulse data " in {
    running(FakeApplication())  {
      
      val page = WorkoutLocation.list()
      println (page.toString())

      // Verify no data is returned without a company,
      (
        page match {
          case Some(page) => page.items == List()    // equal empty List
          case None => true
        }
      ) must equalTo (true)

      // Pull netpulse data and verify meaningful data is returned
      val page1 = WorkoutLocation.list( endDate = "2011-09-31", filter = "1" )
      // Some(Page(List(WorkoutLocation(None,1,19,19,25,2,8.0000,25.0000,12.50000000,9.9600000,4.1500000)),List(WorkoutLocation(,1,19,19,25,2,8.0000,25.0000,12.50000000,9.9600000,4.1500000)),0,0,1))
      (
        page1 match {
          case Some(p) => {
            //          println(p.toString)
            //          println("values = "+
            //            (p.items != List()).toString+","+
            //            (p.items(0).screens).toString+","+
            //            (p.items(0).newReg).toString+","+
            //            (p.totals != List()).toString+","+
            //            (p.totals(0).screens).toString+","+
            //            (p.totals(0).newReg).toString+","
            //          )
  
            ( p.items != List()  &&
              p.items(0).screens == 1 &&
              p.items(0).newReg == java.math.BigDecimal.valueOf(19L) &&
              p.totals != List() &
              p.totals(0).screens == 1 &&
              p.totals(0).newReg == java.math.BigDecimal.valueOf(19L)
              )
          }
          case None => false
        }
      ) must equalTo (true)
    }
  }
}

