package utils

import scalaz.{Node => _, Logger => _, _}
import Scalaz._

import models._

import play.api.libs.ws.WS
import play.api.Play.current
import org.joda.time._
import org.apache.commons.lang._
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import play.api.Logger


object VirtualTrainer {
  lazy val vtPathPrefix = current.configuration.getString("vt.path.prefix").getOrElse(throw new Exception("vt.path.prefix not in configuration"))
  lazy val vtPathValidate = current.configuration.getString("vt.path.validate").getOrElse(throw new Exception("vt.path.validate not in configuration"))
  lazy val vtPathRegister = current.configuration.getString("vt.path.register").getOrElse(throw new Exception("vt.path.register not in configuration"))
  lazy val vtPathLink = current.configuration.getString("vt.path.link").getOrElse(throw new Exception("vt.path.link not in configuration"))
  lazy val vtPathLogin = current.configuration.getString("vt.path.login").getOrElse(throw new Exception("vt.path.login not in configuration"))
  lazy val vtPathLogout = current.configuration.getString("vt.path.logout").getOrElse(throw new Exception("vt.path.logout not in configuration"))
  lazy val vtPathPredefinedPresets = current.configuration.getString("vt.path.predefinedPresets").getOrElse(throw new Exception("vt.path.predefinedPresets not in configuration"))
  lazy val vtPathWorkouts = current.configuration.getString("vt.path.workouts").getOrElse(throw new Exception("vt.path.workouts not in configuration"))
  lazy val vtConsumerKey = current.configuration.getString("vt.consumer.key").getOrElse(throw new Exception("vt.consumer.key not in configuration"))
  lazy val vtConsumerSecret = current.configuration.getString("vt.consumer.secret").getOrElse(throw new Exception("vt.consumer.secret not in configuration"))

  def utcNowInMillis = DateTime.now(DateTimeZone.UTC).getMillis

  def utcNowInSecs = utcNowInMillis / 1000

  def nonce = utcNowInMillis.toString + RandomStringUtils.randomAlphanumeric(6)

  def age(dob: String): Int = {
    val born = new DateTime(dob.slice(4, 8).toInt, dob.slice(0, 2).toInt, dob.slice(2, 4).toInt, 0, 0, 0)
    new Interval(born, DateTime.now).toPeriod.getYears
  }

  def headerNoToken(consKey: String = vtConsumerKey, consSecret: String = vtConsumerSecret): String =
    "OAuth oauth_consumer_key=\"" +
      consKey + "\", oauth_nonce=\"" + nonce + "\", oauth_timestamp=\"" + utcNowInSecs +
      "\", oauth_signature=\"" + (new sun.misc.BASE64Encoder()).encode(consSecret.getBytes("UTF-8")) + "\""

  def headerWithToken(token: String, tokenSecret: String,
                      consKey: String = vtConsumerKey, consSecret: String = vtConsumerSecret): String =
    "OAuth oauth_consumer_key=\"" +
      consKey + "\", oauth_nonce=\"" + nonce + "\", oauth_timestamp=\"" + utcNowInSecs +
      "\", oauth_token=\"" + token +
      "\", oauth_signature=\"" + (new sun.misc.BASE64Encoder()).
      encode((consSecret + "&" + tokenSecret).getBytes("UTF-8")) + "\""

  private def registerBody(params: Map[String, Seq[String]])(implicit nmField: String = "id", pwdField: String = "id") = {

    val machineId = params.getOrElse("machine_id", throw new Exception("machine_id not supplied"))(0)
    val locationId = Machine.getBasic(machineId.toLong).
      getOrElse(throw new Exception("machine_id " + machineId.toString + " not found in database")).locationId
    val password = params.getOrElse(pwdField, throw new Exception(pwdField + " not supplied"))(0)
    val json = ("age" -> age(params.getOrElse("DOB", throw new Exception("DOB not supplied"))(0))) ~
      ("nickName" -> params.getOrElse(nmField, throw new Exception(nmField + " not supplied"))(0)) ~
      ("password" -> (new sun.misc.BASE64Encoder()).encode(password.getBytes("UTF-8"))) ~
      ("gender" -> params.getOrElse("gender", throw new Exception("gender not supplied"))(0).toLowerCase) ~
      ("emailAddress" -> params.getOrElse("email", throw new Exception("email not supplied"))(0)) ~
      ("weight" -> params.getOrElse("weight", throw new Exception("weight not supplied"))(0)) ~
      ("weightUnit" -> "I") ~
      ("preferredLanguageCode" -> "en_US") ~
      ("locationId" -> locationId)
    Printer.compact(JsonAST.render(json))

  }

  private def linkBody(npId: String, vtId: String) = {
    Printer.compact(JsonAST.render(
      ("externalUserId" -> npId) ~
      ("vtUserId" -> vtId) ~
      ("type" -> "NP")
    ))
  }

  private def loginBody(username: String, password: String) = {
    Printer.compact(JsonAST.render(
      ("username" -> (new sun.misc.BASE64Encoder()).encode(username.getBytes("UTF-8"))) ~
        ("password" -> (new sun.misc.BASE64Encoder()).encode(password.getBytes("UTF-8")))
    ))
  }

  def vtRequest(path: String, header: => String) = {
    val h = header
    WS.url(vtPathPrefix + path).withHeaders(("Content-Type", "application/json"), ("Authorization", h))
  }

  /**
   * @return A tuple with Virtual Trainer userId, nickName and password, if successful
   */
  def register(params: Map[String, Seq[String]]): ValidationNEL[String,  (String, String, String)] = {

    validate {

      val npId = params.getOrElse("id", throw new Exception("id not found"))(0)
      
      val valPromise = vtRequest(vtPathValidate, headerNoToken()).post(registerBody(params))
      valPromise.await(20000) // TODO -- too high!
      val valResult = valPromise.value.get
      if (valResult.status != 200) throw new Exception("Did not got ok from VT validate_new_account: " + valResult.body)

      val regPromise = vtRequest(vtPathRegister, headerNoToken()).post(registerBody(params))
      regPromise.await(20000) // TODO
      val regXml = regPromise.value.get.xml

      (regXml \\ "userId").find(n => true) match {

        case Some(id) =>
          val linkResult = vtRequest(vtPathLink, headerNoToken()).post(linkBody(npId, id.text)).value.get
          if (linkResult.status != 200) Logger.info("VT link_external_user returned status " + linkResult.status.toString)
          (id.text, (regXml \\ "nickName").text, (regXml \\ "nickName").text)

        case _ => throw new Exception("VirtualTrainer.register: Couldn't find userId in vt xml response: " + regXml.toString)
      }
    }
  }

  /**
   * @return A tuple with token and token secret
   */
  def login(username: String, password: String): ValidationNEL[String, (String, String)] = {

    validate {
      val result = vtRequest(vtPathLogin, headerNoToken()).post(loginBody(username, password)).value.get
      val Token = """(.*oauth_token=\")([^\"]*).*""".r
      val Secret = """(.*oauth_token_secret=\")([^\"]*).*""".r
      val token = result.header("Authorization") match {
        case Some(Token(begin, t)) => t
        case _ => throw new Exception("No token returned from vt login. Authorization header is: " + result.header("Authorization") + " Body is: " + result.body)
      }
      val secret = result.header("Authorization") match {
        case Some(Secret(begin, s)) => s
        case _ => throw new Exception("No secret returned from vt login. Authorization header is: " + result.header("Authorization") + " Body is: " + result.body)
      }
      (token, secret)
    }
  }

  def logout(token: String, tokenSecret: String) = {

    // TODO - confirm that this is working properly
    vtRequest(vtPathLogout, headerWithToken(token, tokenSecret)).post("").value
  }

  /**
   * @return An xml string with the predefined presets
   */
  def predefinedPresets(token: String, tokenSecret: String): ValidationNEL[String, String] = {

    validate { vtRequest(vtPathPredefinedPresets, headerWithToken(token, tokenSecret)).get().value.get.body.toString }
  }

  /**
   * @return An xml string with the predefined presets
   */
  def workouts(token: String, tokenSecret: String): ValidationNEL[String, String] = {

    validate { vtRequest(vtPathWorkouts, headerWithToken(token, tokenSecret)).get().value.get.body.toString }
  }

}