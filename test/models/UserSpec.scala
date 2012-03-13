package test

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._


object UserSpec extends Specification {

  import models._

  "The User Model " should {

    val devMode = false

    val fakeUser = User(0, Some("Joe"), Some("Sample"), "ballyhoo*you", "joe@company.com",
      Some(1), None, None)
    
    var fuID = -1L

    def compareU (u1: User, u2: User): Boolean = { u1.email == u2.email }

    
    "get or add a test user for use in other tests" in {
      running(FakeApplication()) {
        User.findByEmail(fakeUser.email) match {
          case Some(u) =>
            if (devMode) println("User found: "+u.toString)
            fuID = u.id
            true
          case None =>
            if (devMode) println("User not found: "+fakeUser.email)
            User.insert(fakeUser) match
            {
              case Some(u) =>
                if (devMode) println("Inserted User :"+u.toString)
                fuID = u
                (u > -1)
              case _ =>
                if (devMode) println("User insert failed :"+fakeUser.toString)
                false
            }
        }

      }
    }
    
    "get the fake user's ID from their email and check via findById" in {
      running(FakeApplication()) {
        User.findByEmail(fakeUser.email) match
        {
          case Some(u) =>  
            User.findById(u.id) match {
              case Some(u) => compareU(u, fakeUser)
              case _ => false
            }
          case _ => false
        }
      }
    }
    
    "Authenticate a user with a known good uname and pw." in {
      running(FakeApplication()) {
        val user = User.authenticate( fakeUser.email, fakeUser.password)
        if (devMode) println("User = " + user.toString)
        user match {
          case Some(u) => compareU(u, fakeUser)
          case _ => false
        }
      }
    }

    "Reject a user with bad uname and good pw." in {
      running(FakeApplication()) {
        User.authenticate("failme@netpulse.com", fakeUser.password) match {
          case Some(u) => compareU(u, fakeUser)
          case _ => true
        }
      }
    }

    "Reject a user with good uname and bad pw." in {
      running(FakeApplication()) {
        User.authenticate("frudge@netpulse.com", "S@ndB0x!") match {
          case Some(u) => false
          case _ => true
        }
      }
    }

//    "Return a page of users." in {
//      running(FakeApplication()) {
//        val result = User.list( page = 15, orderBy= 1, filter =  "")
//        result match {
//          case Some(page) => page.items.length > 0
//          case _ => false
//        }
//      }
//    }

    
  }

}