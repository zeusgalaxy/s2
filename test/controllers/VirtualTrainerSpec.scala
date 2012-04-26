package test.controllers

import utils._
import services._
import controllers._
import models._
import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import mocks.VirtualTrainerMocks._

object VirtualTrainerSpec extends Specification {

  val channel = (scala.math.random * 100).round

  "VirtualTrainer" should {

    "Handle all possibilities when validating prior to registration" in {

      running(FakeApplication()) {
        FakeVirtualTrainer(validate = FakeVtResponse(status = 500)).vtRegister(vtrRegParams(VtVarData())) must
          equalTo(Left(apiVtRegistrationUserExists))

        FakeVirtualTrainer(validate = FakeVtResponse(status = 404)).vtRegister(vtrRegParams(VtVarData())) must
          equalTo(Left(apiVtRegistrationOtherError))

        FakeVirtualTrainer(register = vtrOkRegistration).vtRegister(vtrRegParams(VtVarData())).isRight must equalTo(true)
      }
    }

    "Make the individual calls to the real Virtual Trainer web service successfully" in {

      running(FakeApplication()) {

        val srvt = new SemiRealVirtualTrainer()
        val vData = VtVarData()
        val regBody = srvt.vtRegisterBody(vtrRegParams(vData)).toOption.getOrElse("body not created")
        val loginBody = srvt.vtLoginBody(vData.id, vData.pwd)

        srvt.vtDoValidate(regBody).status must equalTo(200)

        val regResult = srvt.vtDoRegister(regBody)
        regResult.status must equalTo(200)
        val vtUid = regResult.xml.map { x => (x \\ "userId").head.text}.getOrElse("0")
        vtUid must not equalTo("0")

        srvt.vtDoLink(vData.id, vtUid).status must equalTo(200)

        val loginResult = srvt.vtDoLogin(loginBody)
        loginResult.status must equalTo(200)

        val tEx = """(.*oauth_token=\")([^\"]*).*""".r
        val tsEx = """(.*oauth_token_secret=\")([^\"]*).*""".r

        val hdr = loginResult.header("Authorization")
        val tEx(_, vtToken) = tEx.findFirstIn(hdr.getOrElse("")).getOrElse("")
        val tsEx(_, vtSecret) = tsEx.findFirstIn(hdr.getOrElse("")).getOrElse("")

        vtToken must not equalTo("")
        vtSecret must not equalTo("")

        srvt.vtDoLogout(vtToken, vtSecret).status must equalTo(200)
      }
    }



//    "make all of its calls properly" in {
//
//      running(FakeApplication()) {
//
//        var pth = "http://localhost:9000/n5iregister.jsp?machine_id=1070&id=" + id + "&membership_id=1&email=" + email + "&pic=22&DOB=03011960&gender=M&enableMail=true&weight=180&oem_tos=15"
//        var result = controllers.DinoController.n5iRegister()(FakeRequest("GET", pth))
//
//        status(result) must equalTo(OK)
//        contentAsString(result) must contain("adunit")
//        contentAsString(result) must contain("virtualTrainer")
//        contentAsString(result) must contain("workoutSegments")
//
//        pth = "http://localhost:9000/n5ilogin.jsp?machine_id=1070&id=" + id + "&pic=22&oem_tos=15"
//        result = controllers.DinoController.n5iLogin(id, 1070L)(FakeRequest("GET", pth))
//
//        status(result) must equalTo(OK)
//        contentAsString(result) must contain("adunit")
//        contentAsString(result) must contain("virtualTrainer")
//        contentAsString(result) must contain("workoutSegments")
//
//        /**
//         * http://localhost:9000/exerciserStatus?id=2115180443
//         * http://localhost:9000/getChannels?id=2115180443,location_id=87
//         *
//         * setChannels:
//         *         An example call to test locally, when in the test/controllers directory:
//         *         curl --header "Content-Type: text/xml; charset=UTF-8" -d@setChannels.xml http://localhost:9000/setChannels
//         *
//         */
//
//        pth = "http://localhost:9000/vtLogin?machine_id=1070&id=" + id + "&vt_password=" + pwd
//        result = controllers.ApiController.apiVtLogin(id, pwd, 1070L)(FakeRequest("GET", pth))
//
//        status(result) must equalTo(OK)
//        contentAsString(result) must contain("virtualTrainer")
//        contentAsString(result) must contain("workoutSegments")
//
//        pth = "http://localhost:9000/vtLogout?id=" + id
//        result = controllers.ApiController.apiVtLogout(id)(FakeRequest("GET", pth))
//
//        status(result) must equalTo(OK)
//
//        pth = "http://localhost:9000/vtRegister?machine_id=1070&id=" + id
//        result = controllers.ApiController.apiVtRegister(id, 1070L)(FakeRequest("GET", pth))
//
//        status(result) must equalTo(OK)
//        contentAsString(result) must contain("api error=\"2\"") // They're already registered with vt!
//
//        pth = "http://localhost:9000/exerciserStatus?id=" + id
//        result = controllers.ApiController.apiExerciserStatus(id)(FakeRequest("GET", pth))
//
//        status(result) must equalTo(OK)
//        contentAsString(result) must contain("homeClub")
//        contentAsString(result) must contain("email")
//
//        pth = "http://localhost:9000/setChannels"
//        // TODO - Need a real record that we can pollute with our test data
//        val cnt = <channels npLogin='s2' locationId='99'>
//          <channel>{channel}</channel>
//        </channels>
//
//        val fr = FakeRequest("POST", pth, FakeHeaders(Map("Content-Type" -> List("text/xml"))), cnt)
//        result = controllers.ApiController.apiSetChannels()(fr)
//
//        //        status(result) must equalTo(OK)
//        //        contentAsString(result) must contain("api error=\"0\"")
//
//        pth = "http://localhost:9000/getChannels?id=s2&location_id=99"
//        result = controllers.ApiController.apiGetChannels("s2", 99L)(FakeRequest("GET", pth))
//
//        //        status(result) must equalTo(OK)
//        //        contentAsString(result) must contain("api error=\"0\"")
//        //        contentAsString(result) must contain(channel.toString)
//
//        pth = "http://localhost:9000/vtLinkUser?machine_id=1070&id=" + id + "&vt_password=" + pwd
//        result = controllers.ApiController.apiVtLinkUser(id, pwd, 1070L)(FakeRequest("GET", pth))
//
//        /**
//         * TODO -- Testing the account linking feature requires more setup than this. We would need to
//         * create accounts on both Virtual Trainer and Netpulse -- unlinked -- and then link them with
//         * this call. That setup would need to be redone each time we run this test, since you cannot link
//         * accounts that have already been linked. For now, we're just testing to see that we get the
//         * failure we expect (which really doesn't even tell us if it's TRULY the failure we're expecting;
//         * for that we'd need to look at the log output).
//         */
//        status(result) must equalTo(OK)
//        contentAsString(result) must contain("<api error=\"0\"")
//        contentAsString(result) must contain("virtualTrainer")
//        contentAsString(result) must contain("workoutSegments")
//
//        pth = "http://localhost:9000/gigyaProxy/notifyLogin?siteUID=" + id + "&newUser=true"
//        result = controllers.ApiController.apiGigyaProxy("notifyLogin")(FakeRequest("GET", pth))
//
//        contentAsString(result) must contain("UIDSignature")
//      }
//    }
  }
}
