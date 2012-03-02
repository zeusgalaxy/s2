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

object Api extends Controller {

  // http://localhost:9000/n5ilinkvtuser.jsp?machine_id=1070&id=sOCClkoE103%40stross.com&vt_password=sOCClkoE103%40stross.com

  def linkVtUser(npLogin: String, vtPassword: String, machineId: Long) = Action {
    implicit request =>

      val rp = RegParams(request)
      val genFailElem = <s2LinkResult>Unable to complete account linkage</s2LinkResult>
      implicit val loc = VL("Api.linkVtUser")

      val finalResult =
        for {

          model <- Machine.getWithEquip(machineId).flatMap(_._2.map (e => e.model.toString)).toSuccess(NonEmptyList("Unable to retrieve machine/equipment/model"))
          ex <- Exerciser.findByLogin(npLogin).getOrFail("Exerciser " + npLogin + " not found")
          vtAuth <- VT.login(ex.email, vtPassword) // tuple(token, tokenSecret)
          (vtUid, vtToken, vtTokenSecret) = vtAuth
          linkStatus <- VT.link(npLogin, vtUid)

          updResult <- vld(Exerciser.updVT(npLogin, vtUid, vtToken, vtTokenSecret))

          vtPredefinedPresets <- VT.predefinedPresets(ex.vtToken, ex.vtTokenSecret, model)
          vtWorkouts <- VT.workouts(ex.vtToken, ex.vtTokenSecret, model)

        } yield {
          <vtAccount status="0">
            <vtPredefinedPresets>
              {vtPredefinedPresets}
            </vtPredefinedPresets>
            <vtWorkouts>
              {vtWorkouts}
            </vtWorkouts>
          </vtAccount>
        }
      finalResult.error.fold(e => Ok(genFailElem), s => Ok(s))
  }

  def gigyaLogin = Action {
    implicit request =>
      Ok(html.gigya(request))
  }
}
