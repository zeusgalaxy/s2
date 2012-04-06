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
    "delete if exists and then add back a test user, " +
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

        /**
         * Set up for the tests
         * existing test person(s) then Add a test user
         */
        var pGet = Person.findByLogin(fakePerson.portalLogin)
        if (pGet != None) Person.hardDelete(pGet.get.id)

        val pAdd = Person.insert(fakePerson)
        pAdd.get must be_>(-1L)


        /**
         * get the fake user's ID from their email and check via findById
         */
        val pCheck = Person.findByLogin(fakePerson.portalLogin)
        pCheck.get.firstName must equalTo(fakePerson.firstName)

        val pCheck1 = Person.findById(pCheck.get.id)
        pCheck1.get.firstName must equalTo(pCheck.get.firstName)

        compareP(pCheck1.get, fakePerson) must equalTo(true)


        /**
         * Reject a user with bad uname and good pw.
         */
        Person.authenticate("failme@netpulse.com", fakePerson.portalPassword.get)  must equalTo(None)


        /**
         * Reject a user with good uname and bad pw.
         */
        Person.authenticate("frudge@netpulse.com", "S@ndB0x!")  must equalTo(None)


        /**
         * Authenticate with known good uname and pw
         */
        val pAuth = Person.authenticate(fakePerson.portalLogin, fakePerson.portalPassword.map(s => s).getOrElse(""))
        if (devMode) println("Person = " + pAuth.toString)
        compareP(pAuth.get, fakePerson) must equalTo(true)


        /**
         * Update the fake person
         */
        val p1up = Person.findByLogin(fakePerson.portalLogin)
        p1up.get.firstName mustEqual fakePerson.firstName

        val p2up = Person.update(p1up.get.id, PersonEdit(Some(fakePerson2.firstName), Some(fakePerson2.lastName), fakePerson2.portalLogin,
          fakePerson2.portalPassword, fakePerson2.email))
        p2up mustEqual (1L)

        val p3up = Person.findById(p1up.get.id)


        /**
         * Delete the fake person
         */
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