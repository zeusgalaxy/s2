package test

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import org.joda.time.DateTime

object RoleSpec extends Specification {

  import models._

  "The Role Model " should {

    "Be role models. (pun intended) \n" +
      "verify the basic roles are in the DB \n " +
      "make sure findbyId works\n"+
      "make sure findByName works\n"+
      "verify that groupList returns a list of correct groups\n" in {
      running(FakeApplication()) {

        val dev = true
        class RlDao extends RoleDao
        val rlDao = new RlDao

        val fakeUserId = 8234L        // TODO: create a fakeUser here and remove below

        /**
         * verify the basic roles are in the DB
         */
        val npA = rlDao.rlFindByName("npAdmin")
        if (dev) println("npA = "+npA.toString)
        npA mustNotEqual(None)
        npA mustEqual(rlDao.rlFindById(npA.get.id ))


        val npU = rlDao.rlFindByName("npUser")
        if (dev) println("npU = "+npU.toString)
        npU mustNotEqual(None)
        npU mustEqual(rlDao.rlFindById(npU.get.id ))

        val oemA = rlDao.rlFindByName("oemAdmin")
        if (dev) println("oemA = "+oemA.toString)
        oemA mustNotEqual(None)

        val oemU = rlDao.rlFindByName("oemUser")
        if (dev) println("oemU = "+oemU.toString)
        oemU mustNotEqual(None)

        /**
         *  test the listing of groups
         */
        val groupList = rlDao.rlGroupList(npA.get.id)
        if (dev) println("groupList ="+groupList)
        groupList mustNotEqual(Seq())

      }
    }


  }


}
