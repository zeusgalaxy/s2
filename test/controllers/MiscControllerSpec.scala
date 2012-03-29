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
      val rq1 = FakeRequest().withHeaders(("Cookie" -> "PLAY_SESSION=ffe5ab33858250cc8222e8fab9d81b2325859f50-id%3A179"))
      val result1 = controllers.MiscController.restrictedHello()(rq1)

      status(result1) must equalTo(OK)
      contentAsString(result1) must contain("Hello")

      //
      // access the restricted hello page without login
      //
      val rq2 = FakeRequest()
      val result2 = controllers.MiscController.restrictedHello()(rq2)

      status(result2) must equalTo(SEE_OTHER)
    }
  }
}