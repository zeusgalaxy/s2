package controllers

import play.api.mvc._
import play.api.Play.current

import security._
import views._
import utils._
import models._
import play.api.Logger
import scala.xml._
import scalaz._
import Scalaz._

object ApiController extends ApiController
                        with VirtualTrainer
                        with ExerciserDao
                        with MachineDao
                        with EquipmentDao
/**
 * Controller for general server API functions.
 */
class ApiController extends Controller {
  this: Controller  with VirtualTrainer
                    with ExerciserDao
                    with MachineDao
                    with EquipmentDao =>

  /**Links a Netpulse user with their Virtual Trainer account in those situations where the
   * exerciser had created the Virtual Trainer account prior to creating their Netpulse account. The
   * connection between the two accounts is their e-mail address. If they used different e-mail addresses
   * to set up the two accounts, we cannot link them.
   *
   * An example call to test:
   * http://localhost:9000/vtLinkUser?machine_id=1070&id=2115180443&vt_password=sOCClkoE443%40stross.com
   *
   * @param npLogin Id used by the exerciser to log into Netpulse.
   * @param vtPassword Password used by the exerciser to access their Virtual Trainer account.
   * @param machineId Id of the machine from which this link is being attempted. This is needed
   *                  in order to return an appropriate set of predefined_presets and/or workouts after the linking
   *                  is completed.
   * @return HTTP status 200, with an xml body that contains Virtual Trainer predefined_presets and/or
   *         workouts appropriate for the machine that the exerciser is currently on.
   */
  def apiVtLinkUser(npLogin: String, vtPassword: String, machineId: Long) = Unrestricted {
    implicit request =>

      implicit val loc = VL("Api.vtLinkUser")

      val finalResult =
        for {

          model <- mchGetWithEquip(machineId).flatMap(_._2.map(e => e.model.toString)).
            toSuccess(NonEmptyList("Unable to retrieve machine/equipment/model"))
          ex <- exFindByLogin(npLogin).getOrFail("Exerciser " + npLogin + " not found")
          vtAuth <- vtLogin(ex.email, vtPassword) // tuple(token, tokenSecret)
          (vtUid, vtToken, vtTokenSecret) = vtAuth

          // Only notify them if never linked before
          linkStatus <- if (ex.vtUserId.isEmpty) vtLink(npLogin, vtUid) else true.successNel[String]

          updResult <- vld(exLoginVt(npLogin, vtUid, vtToken, vtTokenSecret))

          vtPredefinedPresets <- vtPredefinedPresets(vtToken, vtTokenSecret, model)
          vtWorkouts <- vtWorkouts(vtToken, vtTokenSecret, model)

        } yield vtAsApiResult(vtPredefinedPresets, vtWorkouts)

      finalResult.error.fold(e => Ok(<api error={apiGeneralError.toString}/>), s => Ok(s))
  }

  /**Establishes an active session for a Netpulse user that has a Virtual Trainer account, and for whom
   * the two accounts have been linked (either through an earlier auto registration, explicit registration
   * or explicit linking action). Once the session is established, the system will be able to return
   * predefined presets and workouts for this exerciser, and the session will remain active until explicitly
   * cancelled by calling vtLogout.
   *
   * An example call to test:
   * http://localhost:9000/vtLogin?machine_id=1070&id=2115180443&vt_password=sOCClkoE443%40stross.com
   *
   * @param npLogin Id used by the exerciser to log into Netpulse.
   * @param vtPassword Password used by the exerciser to access their Virtual Trainer account.
   * @param machineId Id of the machine from which this link is being attempted. This is needed
   *                  in order to return an appropriate set of predefined_presets and/or workouts after the linking
   *                  is completed.
   * @return HTTP status 200, with an xml body that contains Virtual Trainer predefined_presets and/or
   *         workouts appropriate for the machine that the exerciser is currently on.
   */
  def apiVtLogin(npLogin: String, vtPassword: String, machineId: Long) = Unrestricted {
    implicit request =>

      implicit val loc = VL("Api.vtLogin")

      val finalResult =
        for {

          model <- mchGetWithEquip(machineId).flatMap(_._2.map(e => e.model.toString)).
            toSuccess(NonEmptyList("Unable to retrieve machine/equipment/model"))
          ex <- exFindByLogin(npLogin).getOrFail("Exerciser " + npLogin + " not found")
          vtAuth <- vtLogin(ex.email, vtPassword) // tuple(token, tokenSecret)
          (vtUid, vtToken, vtTokenSecret) = vtAuth

          updResult <- vld(exLoginVt(npLogin, vtUid, vtToken, vtTokenSecret))

          vtPredefinedPresets <- vtPredefinedPresets(vtToken, vtTokenSecret, model)
          vtWorkouts <- vtWorkouts(vtToken, vtTokenSecret, model)

        } yield vtAsApiResult(vtPredefinedPresets, vtWorkouts)

      finalResult.error.fold(e => Ok(<api error={apiGeneralError.toString}/>), s => Ok(s))
  }

  /**Terminates any active session between a Netpulse user and their Virtual Trainer account.
   *
   * An example call to test:
   * http://localhost:9000/vtLogout?id=2115180443
   *
   * @param npLogin Id used by the exerciser to log into Netpulse.
   * @return HTTP status 200, with an xml body indicating whether the call was successful or not.
   */
  def apiVtLogout(npLogin: String) = Unrestricted {

    implicit request =>

      implicit val loc = VL("Api.vtLogout")

      val finalResult =
        for {
          ex <- exFindByLogin(npLogin).getOrFail("Exerciser " + npLogin + " not found")
          logoutStatus <- vtLogout(ex.vtToken, ex.vtTokenSecret)
          updResult <- vld(exLogoutVt(npLogin))

        } yield <api error={apiNoError.toString}/>

      finalResult.error.fold(e => Ok(<api error={apiGeneralError.toString}/>), s => Ok(s))
  }

  /**Registers an existing exerciser with Virtual Trainer. This call is used instead of
   * the DinoWrapper.register call for those cases where the exerciser is already set up within
   * Netpulse, and just needs to do the Virtual Trainer portion of the registration.
   *
   * @return HTTP status 200, with an xml body consisting of the predefined_presets that Virtual
   *         Trainer provides for that machine type, assuming the call was successful. If not, the xml message
   *         will include an error code indicating the nature of the problem.
   *
   *         An example call to test:
   *         http://localhost:9000/vtRegister?machine_id=1070&id=2020
   */
  def apiVtRegister(npLogin: String, machineId: Long) = Unrestricted {
    implicit request =>

      implicit val loc = VL("Api.vtRegister")

      // either error code or object encapsulating vt user
      val rVal: Either[Int, VtUser] = (for {
        ex: Exerciser <- vld(exFindByLogin(npLogin).get)
        rp <- vld(VtRegistrationParams(ex, machineId))
        vtUser <- vld(vtRegister(rp))
      } yield {
        vtUser
      }).error.fold(e => Left(apiGeneralError), s => s)

      (rVal match {

        case Left(err) =>
          vld(<api error={err.toString}></api>)
        case Right(vtUser) =>
          for {
            vtAuth <- vtLogin(vtUser.vtNickname, vtUser.vtNickname)
            (vtUid, vtToken, vtTokenSecret) = vtAuth
            updResult <- vld(exLoginVt(npLogin, vtUid, vtToken, vtTokenSecret))
            model <- mchGetWithEquip(machineId).flatMap(_._2.map(e => e.model.toString)).
              toSuccess(NonEmptyList("Unable to rtrv mach/equip/model"))

            vtPredefinedPresets <- vtPredefinedPresets(vtToken, vtTokenSecret, model)

          } yield vtAsApiResult(vtPredefinedPresets)
      }).error.fold(e => Ok(<api error={apiGeneralError.toString}/>), s => Ok(s))
  }

  /**Retrieves supplemental status information for a given exerciser.
   *
   * An example call to test:
   * http://localhost:9000/exerciserStatus?id=2115180443
   *
   * @param npLogin Id used by the exerciser to log into Netpulse.
   * @return HTTP status 200, with an xml body describing the status of an exerciser's
   *         current relationship with Virtual Trainer, Gigya, et. al.
   */
  def apiExerciserStatus(npLogin: String) = Unrestricted {
    implicit request =>

      implicit val loc = VL("Api.exerciserStatus")
      val ex = exFindByLogin(npLogin)
      Ok(ex.isDefined ? ex.get.insertStatus(<api error={apiNoError.toString}></api>, "api") |
          <api error={apiUnableToRetrieveExerciser.toString}/>)
  }

  /**Retrieves previously saved "favorite" tv channels for a given exerciser and location.
   *
   * An example call to test:
   * http://localhost:9000/getChannels?id=2115180443&location_id=87
   *
   * @param npLogin Id used by the exerciser to log into Netpulse.
   * @param locationId Club id where the exercise is currently located.
   * @return HTTP status 200, with an xml body listing the channels (if any) that the exerciser
   *         has previously saved as "favorites" for that location.
   */
  def apiGetChannels(npLogin: String, locationId: Long) = Unrestricted {
    implicit request =>

      implicit val loc = VL("Api.getChannels")
      val channels = exGetSavedChannels(npLogin, locationId)
      Ok(<api error={apiNoError.toString}>
        <channels>
          {for (ch <- channels) yield
          <channel>
            {ch}
          </channel>}
        </channels>
      </api>)
  }

  /**Saves a list of "favorite" tv channels
   *
   * @return HTTP status 200, with an xml body indicating whether the call was successful.
   *
   *         An example call to test locally, when in the test/controllers directory:
   *         curl --header "Content-Type: text/xml; charset=UTF-8" -d@setChannels.xml http://localhost:9000/setChannels
   */
  def apiSetChannels() = Unrestricted(parse.xml) {
    implicit request =>

      implicit val loc = VL("Api.setChannels")

      val finalResult = vld {

        val npLogin = (request.body \\ "@npLogin").text
        val locationId = (request.body \\ "@locationId").text.toLong
        val channels = request.body \\ "channel"
        val chList = (for (ch <- channels) yield ch.text.toLong).toList

        exSetSavedChannels(npLogin, locationId, chList)

      }.error.fold(e => false, s => s)

      finalResult ? Ok(<api error={apiNoError.toString}></api>) |
        Ok(<api error={apiGeneralError.toString}></api>)
  }

  /**Initiates the gigya/facebook login sequence.
   *
   * @return HTML/Javascript provided by gigya
   */
  def apiBeginLogin = Unrestricted {
    implicit request =>
      Ok(html.beginLogin())
  }

  /**Initiates the gigya/facebook connection sequence.
   *
   * @return HTML/Javascript provided by gigya
   */
  def apiBeginConnection = Unrestricted {
    implicit request =>
      Ok(html.beginConnection())
  }

  /**Landing page to give to Gigya so they can call back to Netpulse after completing social login.
   *
   * @return HTML/Javascript provided by gigya
   */
  def apiEndGigya = Unrestricted {
    implicit request =>
      Ok(html.endGigya())
  }

  /**Proxy any Gigya call by building the request as needed by Gigya (i.e, with the proper security headers, etc.)
   * and then making the Gigya call and returning whatever it yields as our own result.
   *
   * @param method The gigya API method to be invoked
   * @return Whatever the Gigya page returns.
   */
  def apiGigyaProxy(method: String) = Unrestricted {
    implicit request =>

      implicit val loc = VL("ApiController.gigyaProxy")

      val gigyaResp = Gigya.call(method, request.queryString)
      val ourResp = tst(gigyaResp)(_.getErrorCode == 0).
        add("gigya response log", gigyaResp.getLog).error.
        fold(e => Ok(gigyaResp.getResponseText), s => Ok(gigyaResp.getResponseText))
      //      ourResp.withHeaders() // TODO - Can we get the gigya headers some how and use their content-type?
      ourResp

  }

  /**Sets the exerciser's "show profile pic" flag to control whether or not their
   * profile picture is shown.
   *
   * An example call to test:
   * http://localhost:9000/setProfilePic?id=2115180443&show=0
   *
   * @param npLogin Id used by the exerciser to log into Netpulse.
   * @param show Boolean flag indicating whether or not to show the profile picture (must be 0 or 1)
   * @return HTTP status 200, with an xml body indicating whether the call was successful.
   */
  def apiSetShowProfilePic(npLogin: String, show: Boolean) = Unrestricted {
    implicit request =>

      implicit val loc = VL("Api.setShowProfilePic")
      val result = exSetShowProfilePic(npLogin, show)
      result ? Ok(<api error={apiNoError.toString}></api>) |
        Ok(<api error={apiGeneralError.toString}></api>)
  }
}