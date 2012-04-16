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

  val devMode = true

  /***
   *  Model Tests
   *
   */

  "Verify no data is returned without a company, then verify past netpulse data " in {
    running(FakeApplication())  {
      
      val page = WorkoutLocation.list()
      if (devMode) println ("Empty page = " + page.toString())
      // Verify no data is returned without a company,
      page.get.items mustEqual (List())

      // Pull netpulse data and verify meaningful data is returned
      val page1 = WorkoutLocation.list( endDate = "2011-09-31", filter = "1" )
      // Some(Page(List(WorkoutLocation(None,1,19,19,25,2,8.0000,25.0000,12.50000000,9.9600000,4.1500000)),List(WorkoutLocation(,1,19,19,25,2,8.0000,25.0000,12.50000000,9.9600000,4.1500000)),0,0,1))
      if (devMode) println ("Netpulse test page = " + page1.toString())
      page1.get.items mustEqual (List())
      page1.get.totals(0).newReg mustEqual (new java.math.BigDecimal(19) )
      page1.get.totals(0).totReg mustEqual (new java.math.BigDecimal(19) )
    }
  }
}

