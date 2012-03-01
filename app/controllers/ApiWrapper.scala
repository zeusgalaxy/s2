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

object ApiWrapper extends Controller {

  // To test a post with curl, passing a file for the body: curl --header "Content-Type: text/xml; charset=UTF-8" -d@asset_history_upload.xml http://localhost:8080/n5iuploader.jsp
  // http://localhost:9000/n5iregister.jsp?machine_id=18&id=1115180902&membership_id=1&email=sOCClkoE%40stross.com&pic=22&DOB=03011960&gender=M&enableMail=true&weight=180&oem_tos=15

  def register = Action {
    implicit request =>

      val genFailElem = <s2RegisterResult>Unable to complete registration</s2RegisterResult>
      implicit val loc = VL("ApiWrapper.register")

      // Note that what is called "id" in the request is actually "login" in the database (per dino!)
      val params = postOrGetParams(request, List("DOB", "weight", "gender", "email", "id", "machine_id"))
      val dinoXml: Validation[NonEmptyList[String], scala.xml.Elem] = for {
        dinoResult <- Dino.forward(request)
        dXml <- validate(dinoResult.xml)
      } yield dXml

      dinoXml.error(Map("msg" -> "Problem forwarding register call to Dino"))
      val oldXml = dinoXml | genFailElem

      // either error code or tuple(uid, nickName, password)
      val rVal: Either[Int, (String, String, String)] = (for {
        code <- test((oldXml \\ "response" \ "@code").text)(_ == "0", "oldXml response code != 0")
        npLogin <- validate(params("id")(0))
        regResult <- validate(VirtualTrainer.register(params))
      } yield {
        regResult
      }).fold(e => Left(99), s => s)

      val finalResult = rVal match {

        case Left(err) =>
          validate(XmlMutator(oldXml).add("response", <vtAccount status={err.toString}></vtAccount>))
        case Right((vtUid, vtNickName, vtPassword)) =>
          for {
            npLogin <- validate(params("id")(0))
            vtAuth <- VirtualTrainer.login(vtNickName, vtPassword, npLogin) // tuple(token, tokenSecret)
            (vtUid, vtToken, vtTokenSecret) = vtAuth
            updResult <- validate(Exerciser.updateToken(npLogin, vtToken, vtTokenSecret))
            machineId <- validate(params("machine_id")(0).toLong)
            model <- Machine.getWithEquip(machineId).flatMap(_._2.map(e => e.model.toString)).toSuccess(NonEmptyList("Unable to retrieve machine/equipment/model"))

            vtPredefinedPresets <- VirtualTrainer.predefinedPresets(vtToken, vtTokenSecret, model)
            vtWorkouts <- VirtualTrainer.workouts(vtToken, vtTokenSecret, model)

          } yield {
            XmlMutator(oldXml).add("response",
              <vtAccount status="0">
                <vtUid>
                  {vtUid}
                </vtUid>
                <vtNickName>
                  {vtNickName}
                </vtNickName>
                <vtPassword>
                  {vtPassword}
                </vtPassword>
                <vtToken>
                  {vtToken}
                </vtToken>
                <vtTokenSecret>
                  {vtTokenSecret}
                </vtTokenSecret>
                <vtPredefinedPresets>
                  {vtPredefinedPresets}
                </vtPredefinedPresets>
                <vtWorkouts>
                  {vtWorkouts}
                </vtWorkouts>
              </vtAccount>
            )
          }
      }
      finalResult.fold(e => Ok(e.list.mkString(", ")), s => Ok(s))
  }


  // http://qa-ec2.netpulse.ws/core/n5ilogin.jsp?machine_id=18&id=1112925684&pic=22&oem_tos=15
  // http://localhost:9000/n5ilogin.jsp?machine_id=1070&id=1124247419&pic=22&oem_tos=15

  def login = Action {
    implicit request =>

      implicit val loc = VL("ApiWrapper.login")

      // Note that what is called "id" in the request is actually "login" in the database (per dino!)
      val params = postOrGetParams(request, List("id", "machine_id"))

      (for {
        dinoResult <- Dino.forward(request)
        oldXml <- validate(dinoResult.xml)
      } yield {
        (oldXml \\ "response" \ "@code").find(n => true) match {

          case Some(code) if (code.text == "0") => {

            for {
              npLogin <- validate(params("id")(0))
              machineId <- validate(params("machine_id")(0).toLong)
              model <- Machine.getWithEquip(machineId).flatMap(_._2.map(e => e.model.toString)).toSuccess(NonEmptyList("Unable to retrieve machine/equipment/model"))

              ex <- Exerciser.findByLogin(npLogin).getOrFail("Exerciser " + npLogin + " not found in ApiWrapper.login")
              vtPredefinedPresets <- VirtualTrainer.predefinedPresets(ex.vtToken, ex.vtTokenSecret, model)
              vtWorkouts <- VirtualTrainer.workouts(ex.vtToken, ex.vtTokenSecret, model)

            } yield

              XmlMutator(oldXml).add("response",
                <vtAccount>
                  <vtToken>
                    {ex.vtToken}
                  </vtToken>
                  <vtTokenSecret>
                    {ex.vtTokenSecret}
                  </vtTokenSecret>
                  <vtPredefinedPresets>
                    {vtPredefinedPresets}
                  </vtPredefinedPresets>
                  <vtWorkouts>
                    {vtWorkouts}
                  </vtWorkouts>
                </vtAccount>
              )
          }.debug(Map("msg" -> "Problems encountered"))
            .toOption.getOrElse(XmlMutator(oldXml).add("response", <vtAccount></vtAccount>))

          case _ => oldXml
        }
      }).debug(Map("msg" -> "Problems encountered"))
        .fold(e => Ok(<response desc="Login failed" code="1">
        {e.list.mkString(", ")}
      </response>), s => Ok(s))
  }

  // http://localhost:9000/n5ilinkvtuser.jsp?machineid=1070&nplogin=1124247419&vtpwd=dOx2HaxVE419%40stross.com
  def linkVtUser(nplogin: String, vtpwd: String, machineid: Long) = Action {
    implicit request =>

      val genFailElem = <s2LinkResult>Unable to complete account linkage</s2LinkResult>
      implicit val loc = VL("ApiWrapper.linkVtUser")

      val finalResult =
        for {

          model <- Machine.getWithEquip(machineid).flatMap(_._2.map (e => e.model.toString)).toSuccess(NonEmptyList("Unable to retrieve machine/equipment/model"))
          ex <- Exerciser.findByLogin(nplogin).getOrFail("Exerciser " + nplogin + " not found")
          vtAuth <- VirtualTrainer.login(nplogin, vtpwd, nplogin) // tuple(token, tokenSecret)
          (vtUid, vtToken, vtTokenSecret) = vtAuth
          linkStatus <- VirtualTrainer.link(nplogin, ex.email)

          updResult <- validate(Exerciser.updateToken(nplogin, vtUid, vtTokenSecret))

          vtPredefinedPresets <- VirtualTrainer.predefinedPresets(ex.vtToken, ex.vtTokenSecret, model)
          vtWorkouts <- VirtualTrainer.workouts(ex.vtToken, ex.vtTokenSecret, model)

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
      finalResult.error(Map("msg" -> "Problems during linkVtUser")).fold(e => Ok(genFailElem), s => Ok(s))
  }


  def gigyaLogin = Action {
    implicit request =>
      Ok(html.gigya(request))
  }
}
