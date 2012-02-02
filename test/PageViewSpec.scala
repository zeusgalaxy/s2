package test

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import play.api.mvc.AnyContentAsXml
import play.api.test.FakeRequest._
import play.api.Logger


object PageViewSpec extends Specification {

  import models._

  "The PageView App " should {

    // XML test data =>
    val xmlTestNode: scala.xml.NodeSeq =
      <pageviews machine_id='2334' date='2012-01-02'>
          <page name='attract' count='1'/>
          <page name='login' count='2'/>
          <page name='mainMenu' count='3'/>
          <page name='workoutHistory' count='4'/>
          <page name='endOfWorkout' count='5'/>
      </pageviews>

    val xmlBadNode: scala.xml.NodeSeq =
      <pageviews machine_id='2334' date='2012-01-02'>
      </pageviews>



    "Accept fully formed xml POST with known good data from above" in {
      running(FakeApplication()) {
        routeAndCall(FakeRequest(POST, "/", FakeHeaders(), AnyContentAsXml(xmlTestNode))) match {
          case Some(result) => {
            // println(result + " : " + contentAsString(result))
            status(result) must equalTo(200)
            contentAsString(result) must contain("Success")
          }
          case _ => false must equalTo(true)
        }
      }
    }

    "Reject xml without page views " in {
      running(FakeApplication()) {
        routeAndCall(FakeRequest(POST, "/", FakeHeaders(), AnyContentAsXml(xmlBadNode) )) match {
          case Some(result) => {
            // println(result + " : " + contentAsString(result))
            status(result) must equalTo(200)
            contentAsString(result) must contain("Failure")
          }
          case _ => false must equalTo(true)
        }
      }
    }

    "Reject missing xml" in {
      running(FakeApplication()) {
        routeAndCall(FakeRequest(POST, "/", FakeHeaders(), AnyContentAsXml(Nil))) match {
          case Some(result) => {
            // println(result + " : " + contentAsString(result))
            status(result) must equalTo(200)
            contentAsString(result) must contain("Failure")
          }
          case _ => false must equalTo(true)
        }
      }
    }

//    "parse xml into the model" in {
//      running(FakeApplication()) {
//        PageViewModel.parseXML(xmlTestNode) match {
//          case Some(pvm) => {
//            pvm.machineId must equalTo(2334)
//            pvm.pageCounts(0)._1 must equalTo("attract")
//          }
//          case _ => false must equalTo(true)
//        }
//      }
//    }
//
//    "verify the data in the model" in {
//      running(FakeApplication()) {
//        PageViewModel.parseXML(xmlTestNode) match {
//          case Some(pvm) => PageViewModel.verifyData(pvm) must equalTo(true)
//          case _ => false must equalTo(true)
//        }
//      }
//    }
//
//    "add a test record to the DB" in {
//      running(FakeApplication()) {
//        PageViewModel.parseXML(xmlTestNode) match {
//          case Some(pvm) => PageViewModel.insert(pvm) must equalTo(5)
//          case _ => false must equalTo(true)
//        }
//      }
//    }
//
//    "return an existing machine model from the ID" in {
//      running(FakeApplication()) {
//        MachineModel.findById(2334L) match {
//          case Some(mach) => mach.model must equalTo("n5i")
//          case _ => false must equalTo(true)
//        }
//        // println("machine: " + mach.toString)
//        // mach.id must equalTo(2334L)
//        true must equalTo(true)
//      }
//    }
//

  }
}

