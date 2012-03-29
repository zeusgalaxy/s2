package test

import utils._
import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._

object WebAppSpec extends Specification {

  "access the unrestricted hello page, restricted hello page with login, restricted hello page without login" in {


    running(FakeApplication()) {
      //
      //  access the unrestricted hello page
      //
      val rq = FakeRequest()
      val result = controllers.WebApp.unrestrictedHello()(rq)

      status(result) must equalTo(OK)
      contentAsString(result) must contain("Hello Everybody!")

      //
      // access the restricted hello page with login
      //
      val rq1 = FakeRequest().withHeaders(("Cookie" -> "PLAY_SESSION=ffe5ab33858250cc8222e8fab9d81b2325859f50-id%3A179"))
      val result1 = controllers.MiscController.restrictedHello()(rq1)

<<<<<<< Updated upstream:test/controllers/WebAppSpec.scala
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
=======
      status(result1) must equalTo(OK)
      contentAsString(result1) must contain("Hello")

      //
      // access the restricted hello page without login
      //
      val rq2 = FakeRequest()
      val result2 = controllers.MiscController.restrictedHello()(rq2)

      status(result2) must equalTo(SEE_OTHER)
>>>>>>> Stashed changes:test/controllers/MiscControllerSpec.scala
    }
  }
}