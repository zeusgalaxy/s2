package controllers

import play.api.mvc._
import play.api.Play.current

import views._
import utils._
import models._
import play.api.Logger
import scala.xml._
import scalaz._
import Scalaz._

/**
 * Controller for general server API functions.
 */
object Api extends Controller {

  // http://localhost:9000/vtLinkUser?machine_id=1070&id=2115180443&vt_password=sOCClkoE443%40stross.com

  /**Links a Netpulse user with their Virtual Trainer account in those situations where the
   * exerciser had created the Virtual Trainer account prior to creating their Netpulse account. The
   * connection between the two accounts is their e-mail address. If they used different e-mail addresses
   * to set up the two accounts, we cannot link them.
   *
   * @param npLogin Id used by the exerciser to log into Netpulse.
   * @param vtPassword Password used by the exerciser to access their Virtual Trainer account.
   * @param machineId Id of the machine from which this link is being attempted. This is needed
   * in order to return an appropriate set of predefined_presets and/or workouts after the linking
   * is completed.
   * @return Xml that contains Virtual Trainer predefined_presets and/or workouts appropriate for the
   * machine that the exerciser is currently on.
   */
  def vtLinkUser(npLogin: String, vtPassword: String, machineId: Long) = Action {
    implicit request =>

      implicit val loc = VL("Api.vtLinkUser")

      val finalResult =
        for {

          model <- Machine.getWithEquip(machineId).flatMap(_._2.map(e => e.model.toString)).
            toSuccess(NonEmptyList("Unable to retrieve machine/equipment/model"))
          ex <- Exerciser.findByLogin(npLogin).getOrFail("Exerciser " + npLogin + " not found")
          vtAuth <- VT.login(ex.email, vtPassword) // tuple(token, tokenSecret)
          (vtUid, vtToken, vtTokenSecret) = vtAuth

          // Only notify them if never linked before
          linkStatus <- if (ex.vtUserId.isEmpty) VT.link(npLogin, vtUid) else true.successNel[String]

          updResult <- vld(Exerciser.loginVt(npLogin, vtUid, vtToken, vtTokenSecret))

          vtPredefinedPresets <- VT.predefinedPresets(vtToken, vtTokenSecret, model)
          vtWorkouts <- VT.workouts(vtToken, vtTokenSecret, model)

        } yield VT.asXml(vtPredefinedPresets, vtWorkouts)

      finalResult.error.fold(e => Ok(<api error={apiGeneralError.toString}/>), s => Ok(s))
  }

  // http://localhost:9000/vtLogin?machine_id=1070&id=2115180443&vt_password=sOCClkoE443%40stross.com

  /**Establishes an active session for a Netpulse user that has a Virtual Trainer account, and for whom
   * the two accounts have been linked (either through an earlier auto registration, explicit registration
   * or explicit linking action). Once the session is established, the system will be able to return
   * predefined presets and workouts for this exerciser, and the session will remain active until explicitly
   * cancelled by calling vtLogout.
   *
   * @param npLogin Id used by the exerciser to log into Netpulse.
   * @param vtPassword Password used by the exerciser to access their Virtual Trainer account.
   * @param machineId Id of the machine from which this link is being attempted. This is needed
   * in order to return an appropriate set of predefined_presets and/or workouts after the linking
   * is completed.
   * @return Xml that contains Virtual Trainer predefined_presets and/or workouts appropriate for the
   * machine that the exerciser is currently on.
   */
  def vtLogin(npLogin: String, vtPassword: String, machineId: Long) = Action {
    implicit request =>

      implicit val loc = VL("Api.vtLogin")

      val finalResult =
        for {

          model <- Machine.getWithEquip(machineId).flatMap(_._2.map(e => e.model.toString)).
            toSuccess(NonEmptyList("Unable to retrieve machine/equipment/model"))
          ex <- Exerciser.findByLogin(npLogin).getOrFail("Exerciser " + npLogin + " not found")
          vtAuth <- VT.login(ex.email, vtPassword) // tuple(token, tokenSecret)
          (vtUid, vtToken, vtTokenSecret) = vtAuth

          updResult <- vld(Exerciser.loginVt(npLogin, vtUid, vtToken, vtTokenSecret))

          vtPredefinedPresets <- VT.predefinedPresets(vtToken, vtTokenSecret, model)
          vtWorkouts <- VT.workouts(vtToken, vtTokenSecret, model)

        } yield VT.asXml(vtPredefinedPresets, vtWorkouts)

      finalResult.error.fold(e => Ok(<api error={apiGeneralError.toString}/>), s => Ok(s))
  }

  // http://localhost:9000/vtLogout?id=2115180443
  def vtLogout(npLogin: String) = Action {

    implicit request =>

      implicit val loc = VL("Api.vtLogout")

      val finalResult =
        for {
          ex <- Exerciser.findByLogin(npLogin).getOrFail("Exerciser " + npLogin + " not found")
          logoutStatus <- VT.logout(ex.vtToken, ex.vtTokenSecret)
          updResult <- vld(Exerciser.logoutVt(npLogin))

        } yield <api error={apiNoError.toString}/>

      finalResult.error.fold(e => Ok(<api error={apiGeneralError.toString}/>), s => Ok(s))
  }

  // http://localhost:9000/vtStatus?id=2115180443

  def vtStatus(npLogin: String) = Action {
    implicit request =>

      implicit val loc = VL("Api.vtStatus")
      val ex = Exerciser.findByLogin(npLogin)
      Ok(ex.isDefined ? ex.get.insertVtDetails(<api error={apiNoError.toString}></api>, "api") |
          <api error={apiUnableToRetrieveExerciser.toString}/>)
  }

  // http://localhost:9000/vtRegister?machine_id=1070&id=2020

  def vtRegister(npLogin: String, machineId: Long) = Action {
    implicit request =>

      implicit val loc = VL("Api.vtRegister")

      // either error code or object encapsulating vt user
      val rVal: Either[Int, VtUser] = (for {
        ex: Exerciser <- vld(Exerciser.findByLogin(npLogin).get)
        rp <- vld(RegParams(ex, machineId))
        vtUser <- vld(VT.register(rp))
      } yield {
        vtUser
      }).error.fold(e => Left(apiGeneralError), s => s)

      (rVal match {

        case Left(err) =>
          vld(<api error={err.toString}></api>)
        case Right(vtUser) =>
          for {
            vtAuth <- VT.login(vtUser.vtNickname, vtUser.vtNickname)
            (vtUid, vtToken, vtTokenSecret) = vtAuth
            updResult <- vld(Exerciser.loginVt(npLogin, vtUid, vtToken, vtTokenSecret))
            model <- Machine.getWithEquip(machineId).flatMap(_._2.map(e => e.model.toString)).
              toSuccess(NonEmptyList("Unable to rtrv mach/equip/model"))

            vtPredefinedPresets <- VT.predefinedPresets(vtToken, vtTokenSecret, model)

          } yield VT.asXml(vtPredefinedPresets)
      }).error.fold(e => Ok(<api error={apiGeneralError.toString}/>), s => Ok(s))
  }

  def getChannels(npLogin: String, locationId: Long) = Action {
    implicit request =>

      implicit val loc = VL("Api.getChannels")
      val channels = Exerciser.getSavedChannels(npLogin, locationId)
      Ok(<api error={apiNoError.toString}>
        <channels>
          {for (ch <- channels) yield
          <channel>
            {ch}
          </channel>}
        </channels>
      </api>)
  }

  // Local test: curl --header "Content-Type: text/xml; charset=UTF-8" -d@setChannels.xml http://localhost:9000/setChannels

  def setChannels = Action(parse.xml) {
    implicit request =>

      implicit val loc = VL("Api.setChannels")

      val finalResult = vld {

        val npLogin = (request.body \\ "@npLogin").text
        val locationId = (request.body \\ "@locationId").text.toLong
        val channels = request.body \\ "channel"
        val chList = (for (ch <- channels) yield ch.text.toLong).toList

        Exerciser.setSavedChannels(npLogin, locationId, chList)

      }.error.fold(e => false, s => s)

      finalResult ? Ok(<api error={apiNoError.toString}></api>) |
        Ok(<api error={apiGeneralError.toString}></api>)
  }


  /**Landing page to give to Gigya so they can call back to Netpulse after completing social login.
   *
   * @return An html page that contains javascript code necessary to complete the social login process.
   */
  def gigyaLogin = Action {
    implicit request =>
      Ok(html.gigya(request))
  }
}
