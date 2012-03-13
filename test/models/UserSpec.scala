package test

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._


object UserSpec extends Specification {

  import models._

  "The User Model " should {

    "add a test user for use in other tests" in {
      running(FakeApplication()) {
      User.create(User(0,Some("Joe"),Some("Sample"),"ballyhoo*you","joe@company.com",Some(1),None,None)) match
        {
        case Some(u) => true
        case None => false
        }
      }
    }

   "Authenticate a user with a known good uname and pw." in  {
     running(FakeApplication()) {
       User.authenticate("dfaust@netpulse.com", "S@ndB0x!") match {
         case Some(u) => u.email == "dfaust@netpulse.com"
         case _ => false
       }
     }
   }

    "Reject a user with bad uname and good pw." in  {
      running(FakeApplication()) {
        User.authenticate("frudge@netpulse.com", "S@ndB0x!") match {
          case Some(u) => false
          case _ => true
        }
      }
    }

    "Reject a user with good uname and bad pw." in  {
      running(FakeApplication()) {
        User.authenticate("frudge@netpulse.com", "S@ndB0x!") match {
          case Some(u) => false
          case _ => true
        }
      }
    }
    
    "Grab a user from the DB by ID." in  {
      false
    }

    "Grab a user from the DB by email address." in  {
      false
    }

    "Return a page of users." in {
      false
    }
    
    "Create an npadmin cookie" in {
      false
    }
    
    "Parse an npadmin cookie" in {
      false
    }

  }


}