package test

import utils._
import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import play.api.Play._

object MiscControllerSpec extends Specification {

  "access the unrestricted hello page, restricted hello page with login, restricted hello page without login" in {

    running(FakeApplication()) {
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
      val rq1 = FakeRequest().withHeaders(("Cookie" -> current.configuration.getString("user.test.cookie").get))
      val result1 = controllers.MiscController.restrictedHello()(rq1)

      status(result1) must equalTo(OK)
      contentAsString(result1) must contain("Hello to")

      //
      // access the restricted hello page with a user that cannot
      //
      val rq1a = FakeRequest().withHeaders(("Cookie" -> current.configuration.getString("user.report.cookie").get))
      val result1a = controllers.MiscController.restrictedHello()(rq1a)

      status(result1a) must equalTo(SEE_OTHER)

      //
      // access the restricted hello page without login
      //
      val rq2 = FakeRequest()
      val result2 = controllers.MiscController.restrictedHello()(rq2)

      status(result2) must equalTo(SEE_OTHER)
    }
  }
}