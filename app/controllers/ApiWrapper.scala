package controllers

import play.api.mvc._
import play.api.Play.current

import views._
import utils._
import play.api.Logger

object ApiWrapper extends Controller {

  def register = Action {
    implicit request =>

      val params = postOrGetParams(request, List("DOB", "weight", "gender", "email", "id", "machine_id"))

      Dino.forward(request).fold(e => InternalServerError("Problem during forwarding of dino call. Errors: " + e.list.mkString(", ")),
        dinoResult => {

          validate {
            val oldXml = dinoResult.xml

            validate {
              (oldXml \\ "response" \ "@code").find(n => true) match {

                case Some(code) if (code.text == "0") => {

                  val (vtUid, vtNickName, vtPassword) = VirtualTrainer.register(params).getOrThrow
                  val (vtToken, vtTokenSecret) = VirtualTrainer.login(vtNickName, vtPassword).getOrThrow
                  val vtPredefinedPresets = VirtualTrainer.predefinedPresets(vtToken, vtTokenSecret).getOrThrow
                  val vtWorkouts = VirtualTrainer.workouts(vtToken, vtTokenSecret).getOrThrow
                  VirtualTrainer.logout(vtToken, vtTokenSecret)

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
                        {scala.xml.XML.loadString(vtPredefinedPresets)}
                      </vtPredefinedPresets>
                      <vtWorkouts>
                        {scala.xml.XML.loadString(vtWorkouts)}
                      </vtWorkouts>
                    </vtAccount>
                  )
                }
                case _ => oldXml
              }
            }.debug("ApiWrapper.register", "Failure when trying to register and/or log in to Virtual Trainer").fold(e => oldXml, s => s)
          }.debug("ApiWrapper.register", "Problem with xml from dino call. Body = " + dinoResult.body).
            fold(e => InternalServerError("Problem with xml from dino call. Errors: " + e.list.mkString(", ")), Ok(_))
        })
  }

  // http://qa-ec2.netpulse.ws/core/n5ilogin.jsp?machine_id=18&id=1112925684&pic=22&oem_tos=15
  // http://localhost:9000/n5ilogin.jsp?machine_id=18&id=1112925684&pic=22&oem_tos=15

  def login = Action {
    implicit request =>

      val params = postOrGetParams(request, List("id"))

      Dino.forward(request).fold(e => InternalServerError("Problems forward login rqst to dino. Errors: " + e.list.mkString(", ")),
        dinoResult => {

          validate {
            val oldXml = dinoResult.xml
            (oldXml \\ "response" \ "@code").find(n => true) match {

              case Some(code) if (code.text == "0") => {
                val (vtToken, vtTokenSecret) = VirtualTrainer.login(params("id")(0), params("id")(0)).getOrThrow
                val vtPredefinedPresets = VirtualTrainer.predefinedPresets(vtToken, vtTokenSecret).getOrThrow
                val vtWorkouts = VirtualTrainer.workouts(vtToken, vtTokenSecret).getOrThrow
                XmlMutator(oldXml).add("response",
                  <vtAccount>
                    <vtToken>
                      {vtToken}
                    </vtToken>
                    <vtTokenSecret>
                      {vtTokenSecret}
                    </vtTokenSecret>
                    <vtPredefinedPresets>
                      {scala.xml.XML.loadString(vtPredefinedPresets)}
                    </vtPredefinedPresets>
                    <vtWorkouts>
                      {scala.xml.XML.loadString(vtWorkouts)}
                    </vtWorkouts>
                  </vtAccount>
                )
              }
              case _ => oldXml
            }
          }.info("ApiWrapper.login", "dinoResult body: " + dinoResult.body).
            fold(e => Ok(dinoResult.xml), s => Ok(s.asInstanceOf[scala.xml.Elem]))
        })
  }

  def gigyaLogin = Action {

    implicit request =>
      Ok(html.gigya(request))
  }
}
