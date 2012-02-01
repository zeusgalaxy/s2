package controllers

import play.api.mvc._
import play.api.libs.ws._
import play.api.Play.current

import views._
import utils._

object ApiWrapper extends Controller {

  lazy val npV1Scheme = current.configuration.getString("np.v1.scheme").getOrElse(throw new Exception("np.v1.scheme not in configuration"))
  lazy val npV1Host = current.configuration.getString("np.v1.host").getOrElse(throw new Exception("np.v1.host not in configuration"))
  lazy val npV1RegisterPath = current.configuration.getString("np.v1.path.register").getOrElse(throw new Exception("np.v1.path.register not in configuration"))

  def switchHosts(host: String): String = host.replaceFirst("localhost:9000", "qa-v1.netpulse.ws").replaceFirst("ec2", "v1").replaceFirst("v2", "v1")

  def register = Action {
    implicit request =>

      val params = postOrGetParams(request, List("DOB", "weight", "gender", "email"))
      val formBody = request.body.asFormUrlEncoded

      val npV1Host = switchHosts(request.host)
      val v1Addr = npV1Scheme + "://" + npV1Host + request.uri + {
        formBody match {
          case None => "" // We came in via GET
          case Some(body) => {
            "?" + (body.flatMap {
              t => t._2.map(value => t._1 + "=" + value.toString)
            }
              mkString ("&"))
          }
        }
      }

      try {

        val result = WS.url(v1Addr).get().value
        result.isDefined match {
          case true => {
            val oldXml = result.get.xml
            (oldXml \\ "response" \ "@code").find(n => true) match {
              case Some(code) if (code.text == "0") => {
                val (vtUid, vtNickName, vtPassword) = VirtualTrainer.register(params).get
                val (vtToken, vtTokenSecret) = VirtualTrainer.login(vtNickName, vtPassword).get
                val vtPredefinedPresets = VirtualTrainer.predefinedPresets(vtToken, vtTokenSecret)
                val vtWorkouts = VirtualTrainer.workouts(vtToken, vtTokenSecret)
                VirtualTrainer.logout(vtToken, vtTokenSecret)

                Ok(XmlMutator(oldXml).add("response",
                  <vtAccount>
                    <vtUid>{vtUid}</vtUid>
                    <vtNickName>{vtNickName}</vtNickName>
                    <vtPassword>{vtPassword}</vtPassword>
                    <vtToken>{vtToken}</vtToken>
                    <vtTokenSecret>{vtTokenSecret}</vtTokenSecret>
                    <vtPredefinedPresets>{vtPredefinedPresets}</vtPredefinedPresets>
                    <vtWorkouts>{vtWorkouts}</vtWorkouts>
                  </vtAccount>
                ))
              }
              case _ => Ok(oldXml)
            }
          }
          case false => throw new Exception("The result from v1 registration call was not defined")
        }

      } catch {
        case ex => Ok("Something went wrong: " + ex.getMessage)
      }
  }

  def gigyaLogin = Action {

    implicit request =>
      Ok(html.gigya(request))
  }
}
