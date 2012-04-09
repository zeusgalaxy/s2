package test

import utils._
import org.specs2.mutable._

import models._
import play.api.test._
import play.api.test.Helpers._
import play.api.Play._
import play.api.mvc.Session

object MiscControllerSpec extends Specification {

  "access the unrestricted hello page, restricted hello page with login, restricted hello page without login" in {

    running(FakeApplication()) {

      val setupOk = TestData.setup
      setupOk must equalTo(true)

      val s2a = Person.findByLogin("s2-a@netpulse.com")
      val s2b = Person.findByLogin("s2-b@netpulse.com")
      val s2aEncoded = Session.encode(Map("id" -> s2a.get.id.toString))
      val s2bEncoded = Session.encode(Map("id" -> s2b.get.id.toString))
      //
      //  access the unrestricted hello page
      //
      val rq = FakeRequest()
      val result = controllers.MiscController.unrestrictedHello()(rq)

      status(result) must equalTo(OK)
      contentAsString(result) must contain("Hello Everybody!")

      //
      // access the restricted hello page with a user that can
      //
      val rqA = FakeRequest().withHeaders(("Cookie" -> (Session.COOKIE_NAME + "=" + s2aEncoded)))
      val resultA = controllers.MiscController.helloForA()(rqA)
      status(resultA) must equalTo(OK)

      contentAsString(resultA) must contain("Hello to")
      //
      // access the restricted hello page with a user that cannot
      //

      val rqB = FakeRequest().withHeaders(("Cookie" -> (Session.COOKIE_NAME + "=" + s2aEncoded)))
      val resultB = controllers.MiscController.helloForB()(rqB)

      status(resultB) must equalTo(SEE_OTHER)

      //
      // access the restricted hello page without login
      //
      val rq2 = FakeRequest()
      val result2 = controllers.MiscController.helloForA()(rq2)

      status(result2) must equalTo(SEE_OTHER)
    }
  }
}