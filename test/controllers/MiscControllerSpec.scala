package test

import utils._
import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._

object MiscControllerSpec extends Specification {

  "access the unrestricted hello page" in {

    running(FakeApplication()) {

      val rq = FakeRequest()
      val result = controllers.MiscController.unrestrictedHello()(rq)

      status(result) must equalTo(OK)
      contentAsString(result) must contain("Hello Everybody!")
    }
  }

  "access the restricted hello page with login" in {

    running(FakeApplication()) {
      val rq = FakeRequest().withHeaders(("Cookie" -> "PLAY_SESSION=ffe5ab33858250cc8222e8fab9d81b2325859f50-id%3A179"))
      val result = controllers.MiscController.restrictedHello()(rq)

      status(result) must equalTo(OK)
      contentAsString(result) must contain("Hello")
    }
  }

  "access the restricted hello page without login" in {

    running(FakeApplication()) {

      val rq = FakeRequest()
      val result = controllers.MiscController.restrictedHello()(rq)

      status(result) must equalTo(SEE_OTHER)
    }
  }
}