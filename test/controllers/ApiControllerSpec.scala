package test

import utils._
import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._

object ApiControllerSpec extends Specification {

  val id = org.joda.time.DateTime.now.getMillis.toString.takeRight(10)
  val pwd = "kgs" + id + "@stross.com"
  val email = pwd

  "The API" should {
    "register a new exerciser, then log her in, then try to link" in {

      running(FakeApplication()) {

        var pth = "http://localhost:9000/n5iregister.jsp?machine_id=1070&id=" + id + "&membership_id=1&email=" + email + "&pic=22&DOB=03011960&gender=M&enableMail=true&weight=180&oem_tos=15"
        var result = controllers.DinoController.register()(FakeRequest("GET", pth))

        status(result) must equalTo(OK)
        contentAsString(result) must contain("adunit")
        contentAsString(result) must contain("virtualTrainer")
        contentAsString(result) must contain("workoutSegments")

        pth = "http://localhost:9000/n5ilogin.jsp?machine_id=1070&id=" + id + "&pic=22&oem_tos=15"
        result = controllers.DinoController.login(id, 1070L)(FakeRequest("GET", pth))

        status(result) must equalTo(OK)
        contentAsString(result) must contain("adunit")
        contentAsString(result) must contain("virtualTrainer")
        contentAsString(result) must contain("workoutSegments")

        pth = "http://localhost:9000/vtLinkUser?machine_id=1070&id=" + id + "&vt_password=" + pwd
        result = controllers.ApiController.vtLinkUser(id, pwd, 1070L)(FakeRequest("GET", pth))

        /**
         * TODO -- Testing the account linking feature requires more setup than this. We would need to
         * create accounts on both Virtual Trainer and Netpulse -- unlinked -- and then link them with
         * this call. That setup would need to be redone each time we run this test, since you cannot link
         * accounts that have already been linked. For now, we're just testing to see that we get the
         * failure we expect (which really doesn't even tell us if it's TRULY the failure we're expecting;
         * for that we'd need to look at the log output).
         */
        status(result) must equalTo(OK)
        contentAsString(result) must contain("<api error=\"1\"")
      }
    }
  }
}