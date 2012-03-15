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

  // http://localhost:9000/vtLinkUser?machine_id=1070&id=2115180102&vt_password=sOCClkoE103%40stross.com

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
   * machine that the exerciser is currently on. If the linkage was success, the "status" attribute of
   * the vtAccount element will be "0," otherwise it will be "1" to indicate failure.
   */
  def vtLinkUser(npLogin: String, vtPassword: String, machineId: Long) = Action {
    implicit request =>

      val genFailElem = <vtLinkUser error="1">Unable to complete Virtual Trainer account linkage</vtLinkUser>
      implicit val loc = VL("Api.vtLinkUser")

      val finalResult =
        for {

          model <- Machine.getWithEquip(machineId).flatMap(_._2.map(e => e.model.toString)).
            toSuccess(NonEmptyList("Unable to retrieve machine/equipment/model"))
          ex <- Exerciser.findByLogin(npLogin).getOrFail("Exerciser " + npLogin + " not found")
          vtAuth <- VT.login(ex.email, vtPassword) // tuple(token, tokenSecret)
          (vtUid, vtToken, vtTokenSecret) = vtAuth
          linkStatus <- VT.link(npLogin, vtUid)

          updResult <- vld(Exerciser.updVT(npLogin, vtUid, vtToken, vtTokenSecret))

          vtPredefinedPresets <- VT.predefinedPresets(ex.vtToken, ex.vtTokenSecret, model)
          vtWorkouts <- VT.workouts(ex.vtToken, ex.vtTokenSecret, model)

        } yield VT.asXml(vtPredefinedPresets, vtWorkouts)

      finalResult.error.fold(e => Ok(genFailElem), s => Ok(s))
  }

  // http://localhost:9000/vtStatus?id=2115180443

  def vtStatus(npLogin: String) = Action {
    implicit request =>

      val genFailElem = <vtStatus error="true">Unable to retrieve Virtual Trainer account status</vtStatus>
      implicit val loc = VL("Api.vtStatus")

      val finalResult =
        for {
          ex <- Exerciser.findByLogin(npLogin).getOrFail("Exerciser " + npLogin + " not found")
          status = (!ex.vtUserId.isEmpty || !ex.vtToken.isEmpty || !ex.vtTokenSecret.isEmpty).toString
        } yield
          <vtStatus linkedToVt={status}>
          </vtStatus>

      finalResult.error.fold(e => Ok(genFailElem), s => Ok(s))
  }

  // http://localhost:9000/vtRegister?machine_id=1070&id=2020

  def vtRegister(npLogin: String, machineId: Long) = Action {
    implicit request =>

      val genFailElem = <vtRegister error="true">Unable to complete Virtual Trainer account linkage</vtRegister>
      implicit val loc = VL("Api.vtRegister")

      // either error code or object encapsulating vt user
      val rVal: Either[Int, VtUser] = (for {
        ex: Exerciser <- vld(Exerciser.findByLogin(npLogin).get)
        rp <- vld(RegParams(ex, machineId))
        vtUser <- vld(VT.register(rp))
      } yield {
        vtUser
      }).error.fold(e => Left(99), s => s)

      (rVal match {

        case Left(err) =>
          vld(<virtualTrainer status={err.toString}></virtualTrainer>)
        case Right(vtUser) =>
          for {
            vtAuth <- VT.login(vtUser.vtNickname, vtUser.vtNickname)
            (vtUid, vtToken, vtTokenSecret) = vtAuth
            updResult <- vld(Exerciser.updVT(npLogin, vtUid, vtToken, vtTokenSecret))
            model <- Machine.getWithEquip(machineId).flatMap(_._2.map(e => e.model.toString)).
              toSuccess(NonEmptyList("Unable to rtrv mach/equip/model"))

            vtPredefinedPresets <- VT.predefinedPresets(vtToken, vtTokenSecret, model)

          } yield VT.asXml(vtPredefinedPresets)
      }).error.fold(e => Ok(genFailElem), s => Ok(s))
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
