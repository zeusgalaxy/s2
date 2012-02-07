package controllers

import play.api.mvc._
import play.api.Play.current

import views._
import utils._

object ApiWrapper extends Controller {

  lazy val npV1Scheme = current.configuration.getString("np.v1.scheme").getOrElse(throw new Exception("np.v1.scheme not in configuration"))
  lazy val npV1Host = current.configuration.getString("np.v1.s").getOrElse(throw new Exception("np.v1.s not in configuration"))
  lazy val npV1RegisterPath = current.configuration.getString("np.v1.path.register").getOrElse(throw new Exception("np.v1.path.register not in configuration"))

  def register = Action {
    implicit request =>

      val params = postOrGetParams(request, List("DOB", "weight", "gender", "email", "id"))

      Dino.forward(request).fold(e => Ok(e), dinoResult => {

//        validate[SimpleResult[_]] {
        validate {

          val oldXml = dinoResult.xml
          (oldXml \\ "response" \ "@code").find(n => true) match {

            case Some(code) if (code.text == "0") => {
              val (vtUid, vtNickName, vtPassword) = VirtualTrainer.register(params).getOrThrow
              val (vtToken, vtTokenSecret) = VirtualTrainer.login(vtNickName, vtPassword).getOrThrow
              val vtPredefinedPresets = VirtualTrainer.predefinedPresets(vtToken, vtTokenSecret)
              val vtWorkouts = VirtualTrainer.workouts(vtToken, vtTokenSecret)
              VirtualTrainer.logout(vtToken, vtTokenSecret)

//              Ok(XmlMutator(oldXml).add("response",
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
//            case _ => Ok(oldXml)
            case _ => oldXml
          }
        }.debug("ApiWrapper.register", "dinoResult body: " + dinoResult.body).fold(e => Ok(dinoResult.body), s => Ok(s))
      })
  }

// http://qa-ec2.netpulse.ws/core/n5ilogin.jsp?machine_id=18&id=1112925684&pic=22&oem_tos=15
// http://localhost:9000/n5ilogin.jsp?machine_id=18&id=1112925684&pic=22&oem_tos=15

  def login = Action {
    implicit request =>

      val params = postOrGetParams(request, List("id"))

      Dino.forward(request).fold(e => Ok(e), dinoResult => {

        validate {
          val oldXml = dinoResult.xml
          (oldXml \\ "response" \ "@code").find(n => true) match {

            case Some(code) if (code.text == "0") => {
              val (vtToken, vtTokenSecret) = VirtualTrainer.login(params("id")(0), params("id")(0)).getOrThrow
              val vtPredefinedPresets = VirtualTrainer.predefinedPresets(vtToken, vtTokenSecret)
              val vtWorkouts = VirtualTrainer.workouts(vtToken, vtTokenSecret)
              VirtualTrainer.logout(vtToken, vtTokenSecret)

//              Ok(XmlMutator(oldXml).add("response",
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
//            case _ => Ok(oldXml)
            case _ => oldXml
          }
        }.info("ApiWrapper.login", "dinoResult body: " + dinoResult.body).fold(e => Ok(dinoResult.body), s => Ok(s))
      })
  }

  def gigyaLogin = Action {

    implicit request =>
      Ok(html.gigya(request))
  }
}
