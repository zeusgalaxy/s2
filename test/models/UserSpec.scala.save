package test

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._


object UserSpec extends Specification {
  import models._

  "The User Model " should {
    "get or add a test user, " +
      "find them by email and id, " +
      "Authenticate them, " +
      "reject bad auth attempts, " +
      "update them, then delete them"   in {

      running(FakeApplication()) {

        val devMode = true

        val fakeUser = User(0, Some("Joe"), Some("Sample"), "ballyhoo*you", "joe@company.com",
          Some(1), None, None)

        var fuID = -1L

        def compareU (u1: User, u2: User): Boolean = { u1.email == u2.email }

        // Get or add a test user
        (
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
        ) must equalTo(true)

        // get the fake user's ID from their email and check via findById"
        (
          User.findByEmail(fakeUser.email) match
          {
            case Some(u) =>
              User.findById(u.id) match {
                case Some(u) => compareU(u, fakeUser)
                case _ => false
              }
            case _ => false
          }
        ) must equalTo (true)
      
        // Authenticate with known good uname and pw
        val user = User.authenticate( fakeUser.email, fakeUser.password)
        if (devMode) println("User = " + user.toString)
        ( user match {
          case Some(u) => compareU(u, fakeUser)
          case _ => false
          } 
        ) must equalTo(true)

        // Reject a user with bad uname and good pw.
        (
          User.authenticate("failme@netpulse.com", fakeUser.password) match {
            case Some(u) => compareU(u, fakeUser)
            case _ => true
          }
         ) must equalTo(true)

        // Reject a user with good uname and bad pw.
        (
          User.authenticate("frudge@netpulse.com", "S@ndB0x!") match {
            case Some(u) => false
            case _ => true
          }
        ) must equalTo (true)

        // Update the fake user 
        (
          User.findByEmail(fakeUser.email) match {
            case Some(u) => {
              User.update(u.id, User(u.id, Some("JimBo"),Some("Bobbins"), u.password, u.email, None, None, None )  )
              User.findById(u.id) match {
                case Some(fu)  => {
                  if (devMode) println("Updated user found: "+u.toString)
                  if ( fu.firstName == Some("JimBo") &&
                    fu.lastName == Some("Bobbins") &&
                    fu.password == u.password &&
                    fu.email == u.email )  true
                  else false
                }
                case _ => false
              }
            }
            case _ => false
          }
        ) must equalTo(true)
    
        // Delete the fake user"
        (
          User.findByEmail(fakeUser.email) match {
            case Some(u) => {
              User.hardDelete(u.id)
              User.findById(u.id) match {
                case Some(fu)  =>  false
                case _ => true              // user was deleted
              }
            }
            case _ => false
          }
        ) must equalTo (true)


        // Return a page of users.
//        val result = User.list( page = 15, orderBy= 1, filter =  "")
//        (
//          result match {
//            case Some(page) => page.items.length > 0
//            case _ => false
//          }
//        ) must equalTo(true)
        
        
      }
    }
  }
}

