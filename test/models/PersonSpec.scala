package test

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import org.joda.time.DateTime
import utils.Blowfish
import scalaz._
import Scalaz._


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

        val fakePerson2 = Person(id = 0, firstName = "JimBo", lastName = "Peebles", portalLogin = "JimBologin",
          portalPassword = Some("JimboPassword"), email = "jimbo@sample.com", phone = "(555) 555-1212",
          lastLogin = (new DateTime), activeStatus = 1)

        var fpID = -1L

        def compareP(p1: Person, p2: Person, encryptPW: Boolean = true): Boolean = (
          p1.firstName == p2.firstName &&
            p1.lastName == p2.lastName &&
            p1.portalLogin == p2.portalLogin &&
            p1.portalPassword.get == ((encryptPW) ? Blowfish.encrypt(p2.portalPassword.get) | p2.portalPassword.get) &&
            p1.email == p2.email)

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
                case Some(p) => {
                  println("findByLogin found: " + p.toString)
                  println("fakePerson found: " + fakePerson)
                  compareP(p, fakePerson)
                }
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


        // Update the fake person
        (
          Person.findByLogin(fakePerson.portalLogin) match {
            case Some(p) => {
              Person.update(p.id, PersonEdit(Some(fakePerson2.firstName), Some(fakePerson2.lastName), Some(fakePerson2.portalLogin),
                fakePerson2.portalPassword, fakePerson2.email))
              Person.findById(p.id) match {
                case Some(pp) => {
                  if (devMode) println("fakePerson2 : " + fakePerson2.toString + " pw: " + Blowfish.encrypt(fakePerson2.portalPassword.get))
                  if (devMode) println("Updated person found: " + pp.toString)
                  compareP(pp, fakePerson2)
                }
                case _ => false
              }
            }
            case _ => false
          }
          ) must equalTo(true)

        //
        // Update the fake person
        //
        val p1up = Person.findByLogin(fakePerson.portalLogin)
        p1up.get.firstName mustEqual fakePerson.firstName

        val p2up = Person.update(p1up.get.id, PersonEdit(Some(fakePerson2.firstName), Some(fakePerson2.lastName), Some(fakePerson2.portalLogin),
          fakePerson2.portalPassword, fakePerson2.email))
        p2up mustEqual (1L)

        val p3up = Person.findById(p1up.get.id)




        //
        // Delete the fake user
        //
        val p1del = Person.findByLogin(fakePerson2.portalLogin)
        p1del.get.firstName mustEqual fakePerson2.firstName

        val delcnt = Person.hardDelete(p1del.get.id)
        delcnt mustEqual(1L)

        val p2del = Person.findByLogin(fakePerson2.portalLogin)
        p2del mustEqual None



      }
    }
  }
}