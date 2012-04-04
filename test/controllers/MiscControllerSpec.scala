package test

import utils._
import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._

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
      // access the restricted hello page with login
      //
      val rq1 = FakeRequest().withHeaders(("Cookie" -> "PLAY_SESSION=7861299e9d14d7716f0d23389576ffb2db9e825c-id%3A8234"))
      val result1 = controllers.MiscController.restrictedHello()(rq1)

      status(result1) must equalTo(SEE_OTHER)
//      contentAsString(result1) must contain("Sorry")

      //
      // access the restricted hello page without login
      //
      val rq2 = FakeRequest()
      val result2 = controllers.MiscController.restrictedHello()(rq2)

      status(result2) must equalTo(SEE_OTHER)
    }
  }
}