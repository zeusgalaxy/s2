package controllers

import scalaz.{Logger => _, _}
import Scalaz._

import play.api.http._
import play.api.mvc._
import play.api.Play.current

import security._
import utils._
import models._
import play.api.Logger

/**
 * Controller for server API functions which "wrap" around old Dino functions.
 */
object DinoController extends Controller {
  
  lazy val dinoTimeout = current.configuration.getString("dino.timeout").getOrElse(throw new Exception("dino.timeout not in configuration")).toInt

  /** Forwards a request received by S2 on to Dino for processing. This method is used instead
   * of DinoWrapper.passthru for those cases where S2 needs to perform additional processing
   * on the response before returning it to the caller.
   *
   * @param request The API request that was received by S2.
   * @return A ValidationNEL with error messages if there was a problem, otherwise the response
   * if successful.
   */
  def forward(request: Request[AnyContent]): ValidationNEL[String, play.api.libs.ws.Response] = {

    implicit val loc = VL("DinoWrapper.forward")

    vld {

      val (newRequest, newBody) = toWSRequest(request, dinoTimeout)

      request.method match {
        case "GET" => {
          newRequest.get().await(dinoTimeout).get
        }
        case "POST" => {

          request.body match {
            case AnyContentAsFormUrlEncoded(fueBody) => {
              val wrt = Writeable.writeableOf_urlEncodedForm
              val ct = ContentTypeOf.contentTypeOf_urlEncodedForm
              (newRequest.post[Map[String, Seq[String]]](fueBody)(wrt, ct)).await(dinoTimeout).get
            }
            case _ => newRequest.post(newBody.get).await(dinoTimeout).get
          }
        }
        case "PUT" => newRequest.put(newBody.get).await(dinoTimeout).get
        case "DELETE" => newRequest.delete().await(dinoTimeout).get
        case m => throw new Exception("Unexpected method in Dino.forward: " + m)
      }
    }.error
  }

  /** Sends a request to Dino for processing, without allowing any additional processing on the
   * response that comes back. If additional processing is required (i.e., the call is "wrapped,")
   * use DinoWrapper.forward instead.
   *
   * @return HTTP response type 200 (ok) with the Dino response body, else the http response type
   * 500 (internal server error) with the error text as the body.
   */
  def passthru = Unrestricted {
    implicit request => {
      val r = forward(request)
      r.fold(e => InternalServerError("Problem during dino passthru. Errors: " + e.list.mkString(", ")),
        s => Ok(s.ahcResponse.getResponseBodyAsBytes))
    }
  }

  /** Processes "pageviews" as uploaded by clients in XML payloads. Pageviews are just one of the data types
   * that might appear within this xml payload. For now, only the pageviews type is processed by S2; the other
   * types are forwarded on to Dino for processing.
   *
   * To test locally from the test/controllers directory:
   * curl --header "Content-Type: text/xml; charset=UTF-8" -d@pageviews.xml http://localhost:9000/n5iworkout.jsp
   *
   * @return HTTP status 200, if successful, with a message showing the number of inserts as the body;
   * html status 500 with the error messages as the body, if unsuccessful.
   */
  def pageview = Unrestricted {

    implicit request => {

      implicit val loc = VL("DinoWrapper.pageView")

      vld {

        /**
         * We receive a variety of different uploads in this method. If we don't find the one we're interested in
         * (which is "pageviews"), then simply forward this on to dino for processing. If it does, in fact, have
         * pageviews in the payload, then we process it here.
         */
        if ((~request.body.asXml \\ "pageviews").isEmpty)
          forward(request).fold(e => InternalServerError("Problems forward pageview call. Errors: " + e.list.mkString(", ")),
            r => Ok(r.toString()))
        else {

          val cnt = PageViewModel.insert(~request.body.asXml).getOrThrow
          Ok("PageView load succeeded with " + cnt.toString + "inserts")
        }
      }.add("request body", request.body.toString).error.
        fold(e => InternalServerError("PageView load failed. Errors: " + e.list.mkString(", ")), s => s)
    }
  }

  /** Registers a new exerciser with Netpulse (via Dino) and with Virtual Trainer.
   *
   * @return HTTP status 200, with a body consisting of some combination of Dino's registration response
   * (such as the ad units to be displayed), along with the predefined_presets that Virtual
   * Trainer provides for that machine type. What will actually appear in the body depends
   * on which of these calls to Dino and Virtual Trainer were successful.
   *
   * An example call to test:
   * http://localhost:9000/n5iregister.jsp?machine_id=1070&id=2115180102&membership_id=1&email=sOCClkoE102%40stross.com&pic=22&DOB=03011960&gender=M&enableMail=true&weight=180&oem_tos=15&gigya_id=123
   */
  def register = Unrestricted {
    implicit request =>

      implicit val loc = VL("DinoWrapper.register")

      val rp = RegParams(request)
      val oldXml = forward(request).flatMap { r => vld(r.xml) }.error |
        <response code="2" desc="Unable to register. An error occurred when forwarding registration to Dino."></response>

      // either error code or object encapsulating vt user
      val rVal: Either[Int, VtUser] = (for {
        code <- tst((oldXml \\ "response" \ "@code").text)(_ == "0", "oldXml response code != 0").add("oldXml", oldXml.text)
        vtUser <- vld(VT.register(rp))
      } yield {
        vtUser
      }).error.fold(e => Left(apiGeneralError), s => s)

      val finalResult = rVal match {

        case Left(err) =>
          vld(XmlMutator(oldXml).add("response", <api error={err.toString}></api>))
        case Right(vtUser) =>
          for {
            vtAuth <- VT.login(vtUser.vtNickname, vtUser.vtNickname)
            (vtUid, vtToken, vtTokenSecret) = vtAuth
            updResult <- vld(Exerciser.loginVt(rp.npLogin, vtUid, vtToken, vtTokenSecret))
            machineId <- vld(rp.machineId.toLong)
            model <- Machine.getWithEquip(machineId).flatMap(_._2.map(e => e.model.toString)).
              toSuccess(NonEmptyList("Unable to rtrv mach/equip/model"))

            vtPredefinedPresets <- VT.predefinedPresets(vtToken, vtTokenSecret, model)

          } yield VT.insertIntoXml(oldXml, "response", vtPredefinedPresets)
      }
      finalResult.error.fold(e => Ok(e.list.mkString(", ")), s => Ok(s))
  }

  /** Logs a user in to a session with Netpulse (via Dino), and retrieves an appropriate set
   * of predefined_presets and/or workouts from Virtual Trainer.
   *
   * @param npLogin The identifier used by the exerciser to log into Netpulse.
   * @param machineId The machine id of the machine the exerciser is currently on.
   * @return HTTP status 200, with a body consisting of some combination of Dino's login response
   * (such as the ad units to be displayed), along with the predefined_presets that Virtual
   * Trainer provides for that machine type. What will actually appear in the body depends
   * on which of these calls to Dino and Virtual Trainer were successful.
   *
   * An example call to test:
   * http://localhost:9000/n5ilogin.jsp?machine_id=1070&id=2115180443&pic=22&oem_tos=15
   */
  def login(npLogin: String, machineId: Long) = Unrestricted {
    implicit request =>
      implicit val loc = VL("ApiWrapper.login")

      val ex: Option[Exerciser] = Exerciser.findByLogin(npLogin)

      val responseXml = (for {
        dinoResult <- forward(request)
        oldXml <- vld(dinoResult.xml)
      } yield {
        (oldXml \\ "response" \ "@code").find(n => true) match {

          case Some(code) if (code.text == "0" && ex.isDefined && ex.get.hasVtConnection) => {

            for {
              model <- Machine.getWithEquip(machineId).flatMap(_._2.map(e => e.model.toString)).
                toSuccess(NonEmptyList("Unable to retrieve machine/equipment/model"))

              vtPredefinedPresets <- VT.predefinedPresets(ex.get.vtToken, ex.get.vtTokenSecret, model)
              vtWorkouts <- VT.workouts(ex.get.vtToken, ex.get.vtTokenSecret, model)

            } yield VT.insertIntoXml(oldXml, "response", vtPredefinedPresets, vtWorkouts)
          }.error.toOption.getOrElse(XmlMutator(oldXml).add("response", <api error={apiGeneralError.toString}/>))

          case _ => XmlMutator(oldXml).add("response", <api error={apiNoError.toString}/>)
        }
      }).error.fold(e => <response desc="Login failed." code="5">
        {e.list.mkString(", ")}
      </response>, s => s)

      Ok(ex.isDefined ? ex.get.insertVtDetails(responseXml, "api") | responseXml)
  }
}
