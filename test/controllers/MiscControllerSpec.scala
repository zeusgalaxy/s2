package test

import utils._
import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import play.api.Play._
import play.api.mvc.Session

object MiscControllerSpec extends Specification {

  "access the unrestricted hello page, restricted hello page with login, restricted hello page without login" in {

    running(FakeApplication()) {

      val s2aEncoded = Session.encode(Map("id" -> "186953"))
      val s2bEncoded = Session.encode(Map("id" -> "186954"))
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