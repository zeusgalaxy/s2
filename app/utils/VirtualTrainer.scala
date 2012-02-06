package utils

import scalaz.{Node => _, Logger => _, _}
import Scalaz._

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

    val password = params.getOrElse(pwdField, throw new Exception(pwdField + " not found"))(0)
    val json = ("age" -> age(params.getOrElse("DOB", throw new Exception("DOB not found"))(0))) ~
      ("nickName" -> params.getOrElse(nmField, throw new Exception(nmField + " not found"))(0)) ~
      ("password" -> (new sun.misc.BASE64Encoder()).encode(password.getBytes("UTF-8"))) ~
      ("gender" -> params.getOrElse("gender", throw new Exception("gender not found"))(0).toLowerCase) ~
      ("emailAddress" -> params.getOrElse("email", throw new Exception("email not found"))(0)) ~
      ("weight" -> params.getOrElse("weight", throw new Exception("weight not found"))(0)) ~
      ("weightUnit" -> "I") ~
      ("preferredLanguageCode" -> "en_US") ~
      ("locationId" -> 1L) // TODO - What should this be?
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
  def register(params: Map[String, Seq[String]]): Validation[String,  (String, String, String)] = {

    validate {

      val npId = params.getOrElse("id", throw new Exception("id not found"))(0)
      val vResult = vtRequest(vtPathValidate, headerNoToken()).post(registerBody(params)).value.get
      if (vResult.status != 200) throw new Exception("Did not got ok from VT validate_new_account: " + vResult.body)
      val vtXml = vtRequest(vtPathRegister, headerNoToken()).post(registerBody(params)).value.get.xml

      (vtXml \\ "userId").find(n => true) match {
        case Some(id) =>
          val linkResult = vtRequest(vtPathLink, headerNoToken()).post(linkBody(npId, id.text)).value.get
          if (linkResult.status != 200) Logger.info("VT link_external_user returned status " + linkResult.status.toString)
          (id.text, (vtXml \\ "nickName").text, (vtXml \\ "nickName").text)
        case _ => throw new Exception("VirtualTrainer.register: Couldn't find userId in vt xml response: " + vtXml.toString)
      }
    }
  }

  /**
   * @return A tuple with token and token secret
   */
  def login(username: String, password: String): Validation[String, (String, String)] = {

    validate {
      val result = vtRequest(vtPathLogin, headerNoToken()).post(loginBody(username, password)).value.get
      val Token = """(.*oauth_token=\")([^\"]*).*""".r
      val Secret = """(.*oauth_token_secret=\")([^\"]*).*""".r
      val token = result.header("Authorization") match {
        case Token(begin, t) => t
        case _ => ""
      }
      val secret = result.header("Authorization") match {
        case Secret(begin, s) => s
        case _ => ""
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
  def predefinedPresets(token: String, tokenSecret: String): String = {

    val result = vtRequest(vtPathPredefinedPresets, headerWithToken(token, tokenSecret)).get().value
    result.isDefined match {
      case true => {
        result.get.body.toString
      }
      case false => throw new Exception("VirtualTrainer.predefinedPresets: The result from the vt predefined_presets call was not defined")
    }
  }

  /**
   * @return An xml string with the predefined presets
   */
  def workouts(token: String, tokenSecret: String): String = {

    val result = vtRequest(vtPathWorkouts, headerWithToken(token, tokenSecret)).get().value
    result.isDefined match {
      case true => {
        result.get.body.toString
      }
      case false => throw new Exception("VirtualTrainer.workouts: The result from the vt workouts call was not defined")
    }
  }

}