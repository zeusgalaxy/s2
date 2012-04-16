package test

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._


object CompanySpec extends Specification {

  import models._

  "The Company Model " should {

    "Grab a list of companies from the DB which contain Netpulse, New York Sports Clubs and LifeTime Finess" in {
      running(FakeApplication()) {
        val l = Company.reportCompanyOptions
        // println(l.toString)
        ( l.exists(e => e._2 == "Netpulse") &&  l.exists(e => e._2 == "LifeFitness") )
        }
      }


    }


} // end company model
