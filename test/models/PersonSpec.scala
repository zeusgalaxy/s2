package test

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import org.joda.time.DateTime


object PersonSpec extends Specification {

  import models._

  "The Person Model " should {
    "get or add a test user, " +
      "find them by email and id, " +
      "Authenticate them, " +
      "reject bad auth attempts, " +
      "update them, then delete them" in {

      running(FakeApplication()) {

        val devMode = true

        val fakePerson = Person(id = 0, firstName = "Joe", lastName = "Sample", portalLogin = "loginstring",
          portalPassword = Some("testPassword"), email = "joe@sample.com", phone = "(555) 555-1212",
          lastLogin = (new DateTime), activeStatus = 1)

        var fpID = -1L

        def compareP(p1: Person, p2: Person): Boolean = {
          p1.email == p2.email
        }


        // Get or add a test user
        (
          Person.findByLogin(fakePerson.portalLogin) match {
            case Some(p) =>
              if (devMode) println("Person found: " + p.toString)
              fpID = p.id
              true
            case None =>
              if (devMode) println("Person not found: " + fakePerson.email)
              Person.insert(fakePerson) match {
                case Some(p) =>
                  if (devMode) println("Inserted Person :" + p.toString)
                  fpID = p
                  (p > -1)
                case _ =>
                  if (devMode) println("Person insert failed :" + fakePerson.toString)
                  false
              }
          }
          ) must equalTo(true)

        // get the fake user's ID from their email and check via findById"
        (
          Person.findByLogin(fakePerson.portalLogin) match {
            case Some(p) =>
              Person.findById(p.id) match {
                case Some(p) => compareP(p, fakePerson)
                case _ => false
              }
            case _ => false
          }
          ) must equalTo(true)


        // Reject a user with bad uname and good pw.
        (
          Person.authenticate("failme@netpulse.com",
            fakePerson.portalPassword.map(s => s).getOrElse("")) match {
            case Some(p) => compareP(p, fakePerson)
            case _ => true
          }
          ) must equalTo(true)


        // Reject a user with good uname and bad pw.
        (
          Person.authenticate("frudge@netpulse.com", "S@ndB0x!") match {
            case Some(p) => false
            case _ => true
          }
          ) must equalTo(true)



        // Authenticate with known good uname and pw
        val person = Person.authenticate(fakePerson.portalLogin, fakePerson.portalPassword.map(s => s).getOrElse(""))
        if (devMode) println("Person = " + person.toString)
        (person match {
          case Some(p) => compareP(p, fakePerson)
          case _ => false
        }
          ) must equalTo(true)


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





      }
    }
  }
}