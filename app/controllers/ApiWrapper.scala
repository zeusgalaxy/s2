package controllers

import play.api.mvc._
import play.api.Play.current

import views._
import utils._
import models._
import play.api.Logger

object ApiWrapper extends Controller {

  // To test a post with curl, passing a file for the body: curl --header "Content-Type: text/xml; charset=UTF-8" -d@asset_history_upload.xml http://localhost:8080/n5iuploader.jsp
  // http://localhost:9000/n5iregister.jsp?machine_id=18&id=1115180902&membership_id=1&email=sOCClkoE%40stross.com&pic=22&DOB=03011960&gender=M&enableMail=true&weight=180&oem_tos=15

  def register = Action {
    implicit request =>

      implicit val loc = VL("ApiWrapper.register")
      // Note that what is called "id" in the request is actually "login" in the database (per dino!)
      val params = postOrGetParams(request, List("DOB", "weight", "gender", "email", "id", "machine_id"))

      (for {

        dinoResult <- Dino.forward(request)
        oldXml <- validate(dinoResult.xml)
        code <- test((oldXml \\ "response" \ "@code").text) {
          _ == "0"
        }
        npLogin <- validate(params("id")(0))
        vtAcct <- VirtualTrainer.register(params) // tuple(uid, nickName, password)
        (vtUid, vtNickName, vtPassword) = vtAcct

        vtAuth <- VirtualTrainer.login(vtNickName, vtPassword, npLogin) // tuple(token, tokenSecret)
        (vtToken, vtTokenSecret) = vtAuth
        updResult <- validate(Exerciser.updateToken(npLogin, vtToken, vtTokenSecret))
        machine <- validate(params("machine_id")(0).toLong)

        model <- validate(Machine.getWithEquip(machine).
          getOrFail("Machine " + machine.toString + " not found in ApiWrapper.login").
          info(Map("msg" -> "Model retrieval problems")).
          fold(e => "", s => s._2.getOrFail("No equipment for machine " + machine.toString + " in ApiWrapper.login").
          info(Map("msg" -> "Model retrieval problems")).
          fold(e => "", s => s.model.toString)))
        vtPredefinedPresets <- VirtualTrainer.predefinedPresets(vtToken, vtTokenSecret, model)
        vtWorkouts <- VirtualTrainer.workouts(vtToken, vtTokenSecret, model)

      } yield
        XmlMutator(oldXml).add("response",
          <vtAccount>
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
        ).debug(Map("msg" -> "Problems encountered"))
        .fold(e => Ok(<response desc="Registration failed" code="1">
        {e.list.mkString(", ")}
      </response>), s => Ok(s))
  }

  // http://qa-ec2.netpulse.ws/core/n5ilogin.jsp?machine_id=18&id=1112925684&pic=22&oem_tos=15
  // http://localhost:9000/n5ilogin.jsp?machine_id=18&id=1114247378&pic=22&oem_tos=15

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

            val npLogin = params("id")(0)
            val machine = params("machine_id")(0).toLong
            val model = Machine.getWithEquip(machine).
              getOrFail("Machine " + machine.toString + " not found in ApiWrapper.login").
              info(Map("msg" -> "Model retrieval problems")).
              fold(e => "", s => s._2.getOrFail("No equipment for machine " + machine.toString + " in ApiWrapper.login").
              info(Map("msg" -> "Model retrieval problems")).
              fold(e => "", s => s.model.toString))

            for {
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

  def gigyaLogin = Action {

    implicit request =>
      Ok(html.gigya(request))
  }
}
