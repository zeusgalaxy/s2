package controllers

import scalaz.{Logger => _, _}
import Scalaz._

import play.api.http._
import play.api.mvc._
import play.api.Play.current

import security._
import utils._
import models._
import services._
import play.api.Logger
import xml.NodeSeq

object DinoController extends DinoController
                        with VirtualTrainer
                        with ExerciserDao
                        with MachineDao
                        with EquipmentDao
                        with PageViewDao
/**
 * Controller for server API functions which "wrap" around old Dino functions.
 */
class DinoController extends Controller {
  this: Controller  with VirtualTrainer
                    with ExerciserDao
                    with MachineDao
                    with EquipmentDao
                    with PageViewDao =>

  lazy val dinoTimeout = current.configuration.getString("dino.timeout").getOrElse(throw new Exception("dino.timeout not in configuration")).toInt

  /** Forwards a request received by S2 on to Dino for processing. This method is used instead
   * of DinoController.passthru for those cases where S2 needs to perform additional processing
   * on the response before returning it to the caller.
   *
   * @param request The API request that was received by S2.
   * @return A ValidationNEL with error messages if there was a problem, otherwise the response
   * if successful.
   */
  def forward(request: Request[AnyContent]): ValidationNEL[String, play.api.libs.ws.Response] = {

    implicit val loc = VL("DinoController.forward")

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
    }.logError
  }

  /** Sends a request to Dino for processing, without allowing any additional processing on the
   * response that comes back. If additional processing is required (i.e., the call is "wrapped,")
   * use DinoController.forward instead.
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

      implicit val loc = VL("DinoController.pageView")

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

          val cnt = pvmInsert(~request.body.asXml).getOrThrow
          Ok("PageView load succeeded with " + cnt.toString + "inserts")
        }
      }.addLogMsg("request body", request.body.toString).logError.
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
   * If a gigya user id is passed in (param = gigya_uid), we will add it to the exerciser table so that
   * we know that the link to gigya has already been established.
   *
   * An example call to test:
   * http://localhost:9000/n5iregister.jsp?machine_id=18&id=9194247300&membership_id=1&email=dOxHaxVE73%91900stross.com&pic=22&DOB=03011960&gender=M&enableMail=true&weight=180&oem_tos=15&first_name=kenner&last_name=stross&city=denver&state=CO&country=usa
   * http://localhost:9000/n5iregister.jsp?machine_id=1070&id=2115180102&membership_id=1&email=sOCClkoE102%40stross.com&pic=22&DOB=03011960&gender=M&enableMail=true&weight=180&oem_tos=15&gigya_id=123
   */
  def n5iRegister = Unrestricted {
    implicit request =>

      implicit val loc = VL("DinoController.n5iRegister")

      val rp = VtRegistrationParams(request)
      val dinoXml = forward(request).flatMap { r => vld(r.xml) }.logError |
        <response code="2" desc="Unable to register. An error occurred when forwarding registration to Dino."></response>

      val extendedXml = (for {
        code <- safely[ApiError,String]({(dinoXml \\ "response" \ "@code").text}, ApiError(apiGeneralError))
        ok1 <- if (code != "0") ApiError(apiGeneralError).failNel else true.success
        vtUser <- vtRegister(rp)

        vtAuth <- safely[ApiError, (String, String, String)](vtLogin(vtUser.vtNickname, vtUser.vtNickname).
          toOption.get, ApiError(apiGeneralError))
        (vtUid, vtToken, vtTokenSecret) = vtAuth
        updResult <- safely[ApiError, Boolean](exLoginVt(rp.npLogin, vtUid, vtToken, vtTokenSecret), ApiError(apiGeneralError))
        model <- safely[ApiError, String]({
          mchGetWithEquip(rp.machineId.toLong).flatMap(_._2.map(e => e.model.toString))
        }.get, ApiError(apiGeneralError))

        vtPredefinedPresets <- safely[ApiError, NodeSeq](vtPredefinedPresets(vtToken, vtTokenSecret, model).toOption.get,
          ApiError(apiGeneralError))

      } yield vtInsertIntoXml(dinoXml, "response", vtPredefinedPresets))

      val gigyaUid = request.queryString.get("gigya_uid")
      if (gigyaUid.isDefined) exSetGigyaUid(rp.npLogin, gigyaUid.get(0))

      /**
       * Lastly, try to notify the s2 database about this new exerciser so that the two
       * databases are sync'd up.
       */
      exAddToS2(rp.npLogin)

      extendedXml.fold(e => Ok(XmlMutator(dinoXml).
        add("response", <api error={e.head.code.toString}></api>)), s => Ok(s))
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
  def n5iLogin(npLogin: String, machineId: Long) = Unrestricted {
    implicit request =>
      implicit val loc = VL("DinoController.n5iLogin")

      val ex: Option[Exerciser] = exFindByLogin(npLogin)

      val responseXml = (for {
        dinoResult <- forward(request)
        oldXml <- vld(dinoResult.xml)
      } yield {
        (oldXml \\ "response" \ "@code").find(n => true) match {

          case Some(code) if (code.text == "0" && ex.isDefined && ex.get.hasVtConnection) => {

            for {
              model <- mchGetWithEquip(machineId).flatMap(_._2.map(e => e.model.toString)).
                toSuccess(NonEmptyList("Unable to retrieve machine/equipment/model"))

              vtPredefinedPresets <- vtPredefinedPresets(ex.get.vtToken, ex.get.vtTokenSecret, model)
              vtWorkouts <- vtWorkouts(ex.get.vtToken, ex.get.vtTokenSecret, model)

            } yield vtInsertIntoXml(oldXml, "response", vtPredefinedPresets, vtWorkouts)
          }.logError.toOption.getOrElse(XmlMutator(oldXml).add("response", <api error={apiGeneralError.toString}/>))

          case _ => XmlMutator(oldXml).add("response", <api error={apiNoError.toString}/>)
        }
      }).logError.fold(e => <response desc="Login failed." code="5">
        {e.list.mkString(", ")}
      </response>, s => s)

      Ok(ex.isDefined ? ex.get.insertStatus(responseXml, "api") | responseXml)
  }
}
