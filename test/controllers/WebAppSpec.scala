package test

import utils._
import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._

object WebAppSpec extends Specification {

  "access the unrestricted hello page" in {

    running(FakeApplication()) {

      val rq = FakeRequest()
      val result = controllers.WebApp.unrestrictedHello()(rq)

      status(result) must equalTo(OK)
      contentAsString(result) must contain("Hello Everybody!")
    }
  }

  "access the restricted hello page with login" in {

    running(FakeApplication()) {

      val rq = FakeRequest().withHeaders(("Cookie" -> "PLAY_SESSION=35cd92a4a4e3c8ca034a4525a349598a90321817-email%3Akstross%40netpulse.com%00page%3A%2Findex%00fname%3ASylvester%00id%3A179%00cmpId%3ASome%281%29%00lname%3AKennedy%00oemId%3ANone"))
      val result = controllers.WebApp.restrictedHello()(rq)

      status(result) must equalTo(OK)
      contentAsString(result) must contain("Hello")
    }
  }

  "access the restricted hello page without login" in {

    running(FakeApplication()) {

      val rq = FakeRequest()
      val result = controllers.WebApp.restrictedHello()(rq)

      status(result) must equalTo(UNAUTHORIZED)
    }
  }
}