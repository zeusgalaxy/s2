package test.models

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
    " check if exists and add a test user if not,\n " +
      "find them by email and id, \n" +
      "Authenticate them, \n" +
      "reject bad auth attempts, \n" +
      "update them, then delete them \n" in {

      running(FakeApplication()) {

        val devMode = false
        class PerDao extends PersonDao
        val pDao = new PerDao

        val fakePerson = Person(id = 0, companyId=Some(1L), roleId=1, firstName = "Joe", lastName = "Sample", portalLogin = "loginstring",
          portalPassword = Some("testPassword"), email = "joe@sample.com", phone = "(555) 555-1212", activeStatus = 1)
          // lastLogin = Some((new DateTime)), activeStatus = 1

        val fakePerson2 = Person(id = 0, companyId=Some(1L), roleId=1, firstName = "JimBo", lastName = "Peebles", portalLogin = "JimBologin",
          portalPassword = Some("JimboPassword"), email = "jimbo@sample.com", phone = "(555) 555-1212", activeStatus = 1)

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
        var pGet = pDao.prFindByLogin(fakePerson.portalLogin)
        if (pGet != None) pDao.prDelete(pGet.get.id, fpID)
        var pGet1 = pDao.prFindByLogin(fakePerson2.portalLogin)
        if (pGet1 != None) pDao.prDelete(pGet1.get.id, fpID)

        val pAdd = pDao.prInsert(fakePerson, 8234)
        if (devMode) println("pAdd = " + pAdd.toString)
        // Cannot add or update a child row: a foreign key constraint fails (`s2`.`person`, CONSTRAINT `company_person_fk` FOREIGN KEY (`company_id`) REFERENCES `company` (`id`))
        pAdd.get must be_>(-1L)


        /**
         * get the fake user's ID from their email and check via findById
         */
        val pCheck = pDao.prFindByLogin(fakePerson.portalLogin)
        if (devMode) println("pCheck = " + pCheck.toString)
        pCheck.get.firstName must equalTo(fakePerson.firstName)

        val pCheck1 = pDao.prFindById(pCheck.get.id)
        if (devMode) println("pCheck1 = " + pCheck1.toString)
        pCheck1.get.firstName must equalTo(pCheck.get.firstName)

        compareP(pCheck1.get, fakePerson) must equalTo(true)


        /**
         * Reject a user with bad uname and good pw.
         */
        pDao.prAuthenticate("failme@netpulse.com", fakePerson.portalPassword.get)  must equalTo(None)


        /**
         * Reject a user with good uname and bad pw.
         */
        pDao.prAuthenticate("frudge@netpulse.com", "S@ndB0x!")  must equalTo(None)


        /**
         * Authenticate with known good uname and pw
         */
        val pAuth = pDao.prAuthenticate(fakePerson.portalLogin, fakePerson.portalPassword.map(s => s).getOrElse(""))
        if (devMode) println("Person = " + pAuth.toString)
        compareP(pAuth.get, fakePerson) must equalTo(true)


        /**
         * Update the fake person
         */
        val p1up = pDao.prFindByLogin(fakePerson.portalLogin)
        if (devMode) println("p1up = " + p1up.toString)
        p1up.get.firstName mustEqual fakePerson.firstName

        val p2up = pDao.prUpdate(p1up.get.id, fakePerson2, fpID )
        if (devMode) println("p2up = " + p2up.toString)
        p2up mustEqual (1L)

        val p3up = pDao.prFindById(p1up.get.id)
        if (devMode) println("p3up = " + p3up.toString)
        p3up.get.companyId mustEqual(fakePerson2.companyId)
        p3up.get.roleId mustEqual(fakePerson2.roleId)
        p3up.get.firstName mustEqual(fakePerson2.firstName)
        p3up.get.lastName mustEqual(fakePerson2.lastName)
        p3up.get.email mustEqual(fakePerson2.email)

        /**
         * Delete the updated fake person
         */
        val p1del = pDao.prFindByLogin(fakePerson2.portalLogin)
        if (devMode) println("p1del = " + p1del.toString)
        val delcnt = pDao.prDelete(p1del.get.id, fpID)
        delcnt mustEqual(1L)

        val p2del = pDao.prFindByLogin(fakePerson.portalLogin)
        p2del mustEqual None



      }
    }
  }
}