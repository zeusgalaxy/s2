package test

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import org.joda.time.DateTime

object PersonRoleSpec extends Specification {

  import models._

  "The PersonRole and Role Models " should {

    "Be role models. (pun intended) \n" +
      "verify the basic roles are in the DB \n " +
      "delete existing role(s) for fakeUser"
      "insert a person_role \n" +
      "update the person_role table (deletes and the inserts the new one) \n" +
      "verify the person_role entry\n " +
      "remove the person_role entry\n" +
      "verify the person_role entry is gone\n" in {
      running(FakeApplication()) {

        val dev = true

        val fakeUserId = 8234L        // TODO: create a fakeUser here and remove below

        /**
         * verify the basic roles are in the DB
         */
        val npA = Role.findByName(Role.npAdmin)
        if (dev) println("npA = "+npA.toString)
        npA mustNotEqual(None)

        val npU = Role.findByName(Role.npUser)
        if (dev) println("npU = "+npU.toString)
        npU mustNotEqual(None)

        val oemA = Role.findByName(Role.oemAdmin)
        if (dev) println("oemA = "+oemA.toString)
        oemA mustNotEqual(None)

        val oemU = Role.findByName(Role.oemUser)
        if (dev) println("oemU = "+oemU.toString)
        oemU mustNotEqual(None)

        val delete = PersonRole.deleteByPersonId(fakeUserId)
        if (dev) println("delete = "+delete.toString)
        delete mustNotEqual(None)
        delete.get mustEqual (1)

        val insert = PersonRole.insert(fakeUserId, npA.get)
        if (dev) println("insert = "+insert.toString)
        insert mustNotEqual(None)

        val findByPersonId = PersonRole.findByPersonId(fakeUserId)
        if (dev) println("findById = "+findByPersonId.toString)
        findByPersonId mustNotEqual(None)
        findByPersonId.get mustEqual(Role.npAdmin)

        val update = PersonRole.update(fakeUserId, Role.oemAdmin)
        if (dev) println("update = "+update.toString)
        update mustNotEqual(None)

        val verify = PersonRole.findByPersonId(fakeUserId)
        if (dev) println("verify = "+verify.toString)
        verify mustNotEqual (None)
        verify.get must equalTo(Role.oemAdmin)

        val delete1 = PersonRole.deleteByPersonId(fakeUserId)
        if (dev) println("delete = "+delete1.toString)
        delete1 mustNotEqual(None)
        delete1.get mustEqual (1)

        val verify1 = PersonRole.findByPersonId(fakeUserId)
        if (dev) println("verify1 = "+verify1.toString)
        verify1 must equalTo(None)
      }
    }


  }


} // end company model
