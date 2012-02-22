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

    // Note that what is called "id" in the request is actually "login" in the database (per dino!)
      val params = postOrGetParams(request, List("DOB", "weight", "gender", "email", "id", "machine_id"))

      (for {

        dinoResult <- Dino.forward(request)
        oldXml <- validate(dinoResult.xml)

      } yield {

        (oldXml \\ "response" \ "@code").find(_ => true) match {

          case Some(code) if (code.text == "0") => {

            val npLogin = params("id")(0)
            for {
              vtAcct <- VirtualTrainer.register(params) // tuple(uid, nickName, password)
              (vtUid, vtNickName, vtPassword) = vtAcct
xxx <- validate(println("we got passed the vt register. will pass: " + vtUid + ", " + vtNickName + ", " + vtPassword))
              vtAuth <- VirtualTrainer.login(vtNickName, vtPassword, npLogin) // tuple(token, tokenSecret)
              (vtToken, vtTokenSecret) = vtAuth
yyy <- validate(println("we got passed the vt login. will pass: " + vtToken + ", " + vtTokenSecret))
              updResult <- validate(Exerciser.updateToken(npLogin, vtToken, vtTokenSecret))

              val machine = params("machine_id")(0).toLong
              val model = Machine.getWithEquip(machine).
                         getOrFail("Machine " + machine.toString + " not found in ApiWrapper.login").
                         info("ApiWrapper.login", "model retrieval problems").
                         fold(e => "", s => s._2.getOrFail("No equipment for machine " + machine.toString + " in ApiWrapper.login").
                         info("ApiWrapper.login", "model retrieval problems").
                         fold(e => "", s => s.model.toString))

              vtPredefinedPresets <- VirtualTrainer.predefinedPresets(vtToken, vtTokenSecret, model)
zzz <- validate(println("we got passed the vt updateToken. will pass: " + vtToken + ", " + vtTokenSecret + ", " + model))
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
          }.debug("ApiWrapper.register", "Problems encountered during processing of dino result and/or calling Virtual Trainer:")
            .toOption.getOrElse(XmlMutator(oldXml).add("response", <vtAccount></vtAccount>))

          case _ => XmlMutator(oldXml).add("response", <vtAccount></vtAccount>)
        }
      }).debug("ApiWrapper.register", "Problems encountered")
        .fold(e => Ok(<response desc="Registration failed" code="1">
        {e.list.mkString(", ")}
      </response>), s => Ok(s))
  }

  // http://qa-ec2.netpulse.ws/core/n5ilogin.jsp?machine_id=18&id=1112925684&pic=22&oem_tos=15
  // http://localhost:9000/n5ilogin.jsp?machine_id=18&id=1114247378&pic=22&oem_tos=15

  def login = Action {
    implicit request =>

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
              info("ApiWrapper.login", "model retrieval problems").
                fold(e => "", s => s._2.getOrFail("No equipment for machine " + machine.toString + " in ApiWrapper.login").
              info("ApiWrapper.login", "model retrieval problems").
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
          }.debug("ApiWrapper.register", "Problems encountered during processing of dino result and/or calling Virtual Trainer:")
            .toOption.getOrElse(XmlMutator(oldXml).add("response", <vtAccount></vtAccount>))

          case _ => oldXml
        }
      }).debug("ApiWrapper.login", "Problems encountered")
        .fold(e => Ok(<response desc="Login failed" code="1">
        {e.list.mkString(", ")}
      </response>), s => Ok(s))
  }

  //  def registerOLD = Action {
  //    implicit request =>
  //
  //      val params = postOrGetParams(request, List("DOB", "weight", "gender", "email", "id", "machine_id"))
  //
  //      Dino.forward(request).fold(e => InternalServerError("Problem during forwarding of dino call. Errors: " + e.list.mkString(", ")),
  //        dinoResult => {
  //
  //          validate {
  //            val oldXml = dinoResult.xml
  //
  //            validate {
  //              (oldXml \\ "response" \ "@code").find(_ => true) match {
  //
  //                case Some(code) if (code.text == "0") => {
  //
  //                  val (vtUid, vtNickName, vtPassword) = VirtualTrainer.register(params).getOrThrow
  //                  val (vtToken, vtTokenSecret) = VirtualTrainer.login(vtNickName, vtPassword).getOrThrow
  //                  val vtPredefinedPresets = VirtualTrainer.predefinedPresets(vtToken, vtTokenSecret).getOrThrow
  //                  val vtWorkouts = VirtualTrainer.workouts(vtToken, vtTokenSecret).getOrThrow
  //                  VirtualTrainer.logout(vtToken, vtTokenSecret)
  //
  //                  XmlMutator(oldXml).add("response",
  //                    <vtAccount>
  //                      <vtUid>
  //                        {vtUid}
  //                      </vtUid>
  //                      <vtNickName>
  //                        {vtNickName}
  //                      </vtNickName>
  //                      <vtPassword>
  //                        {vtPassword}
  //                      </vtPassword>
  //                      <vtToken>
  //                        {vtToken}
  //                      </vtToken>
  //                      <vtTokenSecret>
  //                        {vtTokenSecret}
  //                      </vtTokenSecret>
  //                      <vtPredefinedPresets>
  //                        {scala.xml.XML.loadString(vtPredefinedPresets)}
  //                      </vtPredefinedPresets>
  //                      <vtWorkouts>
  //                        {scala.xml.XML.loadString(vtWorkouts)}
  //                      </vtWorkouts>
  //                    </vtAccount>
  //                  )
  //                }
  //                case _ => oldXml
  //              }
  //            }.debug("ApiWrapper.register", "Failure when trying to register and/or log in to Virtual Trainer").
  //              fold(e => oldXml, s => s)
  //          }.debug("ApiWrapper.register", "Problem with xml from dino call. Body = " + dinoResult.body).
  //            fold(e => InternalServerError("Problem with xml from dino call. Errors: " + e.list.mkString(", ")), Ok(_))
  //        })
  //  }

  //  // http://qa-ec2.netpulse.ws/core/n5ilogin.jsp?machine_id=18&id=1112925684&pic=22&oem_tos=15
  //  // http://localhost:9000/n5ilogin.jsp?machine_id=18&id=1112925684&pic=22&oem_tos=15
  //
  //  def loginOLD = Action {
  //    implicit request =>
  //
  //      val params = postOrGetParams(request, List("id"))
  //
  //      Dino.forward(request).fold(e => InternalServerError("Problems forward login rqst to dino. Errors: " + e.list.mkString(", ")),
  //        dinoResult => {
  //
  //          validate {
  //            val oldXml = dinoResult.xml
  //            (oldXml \\ "response" \ "@code").find(n => true) match {
  //
  //              case Some(code) if (code.text == "0") => {
  //                val (vtToken, vtTokenSecret) = VirtualTrainer.login(params("id")(0), params("id")(0)).getOrThrow
  //                val vtPredefinedPresets = VirtualTrainer.predefinedPresets(vtToken, vtTokenSecret).getOrThrow
  //                val vtWorkouts = VirtualTrainer.workouts(vtToken, vtTokenSecret).getOrThrow
  //                XmlMutator(oldXml).add("response",
  //                  <vtAccount>
  //                    <vtToken>
  //                      {vtToken}
  //                    </vtToken>
  //                    <vtTokenSecret>
  //                      {vtTokenSecret}
  //                    </vtTokenSecret>
  //                    <vtPredefinedPresets>
  //                      {scala.xml.XML.loadString(vtPredefinedPresets)}
  //                    </vtPredefinedPresets>
  //                    <vtWorkouts>
  //                      {scala.xml.XML.loadString(vtWorkouts)}
  //                    </vtWorkouts>
  //                  </vtAccount>
  //                )
  //              }
  //              case _ => oldXml
  //            }
  //          }.info("ApiWrapper.login", "dinoResult body: " + dinoResult.body).
  //            fold(e => Ok(dinoResult.xml), s => Ok(s.asInstanceOf[scala.xml.Elem]))
  //        })
  //  }

  def gigyaLogin = Action {

    implicit request =>
      Ok(html.gigya(request))
  }
}
