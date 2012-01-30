package utils

import play.api.libs.WS
import play.api.Play.current
import org.joda.time._
import org.apache.commons.lang._
import net.liftweb.json._
import net.liftweb.json.JsonDSL._


object VirtualTrainer {
  lazy val vtPathPrefix = current.configuration.getString("vt.path.prefix").getOrElse(throw new Exception("vt.path.prefix not in configuration"))
  lazy val vtPathRegister = current.configuration.getString("vt.path.register").getOrElse(throw new Exception("vt.path.register not in configuration"))
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
    val born = new DateTime(dob.slice(4,8).toInt, dob.slice(0,2).toInt, dob.slice(2,4).toInt, 0, 0, 0)
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
        encode((consSecret+"&"+tokenSecret).getBytes("UTF-8")) + "\""

  private def registerBody(params: Map[String, Seq[String]]) = {

    val password = params.getOrElse("email", throw new Exception("email not found"))(0)
    val json = ("age" -> age(params.getOrElse("DOB", throw new Exception("DOB not found"))(0))) ~
    ("nickName" -> params.getOrElse("email", throw new Exception("email not found"))(0)) ~
    ("password" -> (new sun.misc.BASE64Encoder()).encode(password.getBytes("UTF-8"))) ~
    ("gender" -> params.getOrElse("gender", throw new Exception("gender not found"))(0).toLowerCase) ~
    ("emailAddress" -> params.getOrElse("email", throw new Exception("email not found"))(0)) ~
    ("weight" -> params.getOrElse("weight", throw new Exception("weight not found"))(0))~
    ("weightUnit" -> "I") ~
    ("preferredLanguageCode" -> "en_US") ~
    ("locationId" -> 1L)                        // TODO - What should this be?
    Printer.compact(JsonAST.render(json))

  }
  
  private def loginBody(username: String, password: String) = {
    Printer.compact(JsonAST.render(
      ("username" -> (new sun.misc.BASE64Encoder()).encode(username.getBytes("UTF-8"))) ~
      ("password" -> (new sun.misc.BASE64Encoder()).encode(password.getBytes("UTF-8")))
    ))
  }

  def vtRequest(path: String, header: => String) = {
    val h = header
    WS.url(vtPathPrefix + path).withHeaders(("Content-Type", "application/json"),("Authorization", h))
  }

  /**
   * @return A tuple with Virtual Trainer userId, nickName and password, if successful
   */
  def register(params: Map[String, Seq[String]]): Option[(String,String,String)] = {

    val result = vtRequest(vtPathRegister, headerNoToken()).post(registerBody(params)).value
    result.isDefined match {
      case true => {
        try {
          val vtXml = result.get.xml
          (vtXml \\ "userId").find(n => true) match {
            case Some(id) => Some((id.text, (vtXml \\ "nickName").text, (vtXml \\ "nickName").text))
            case _ => throw new Exception("VirtualTrainer.register: Couldn't find userId in vt xml response: " + vtXml.toString)
          }
        } catch {
          case ex => throw new Exception("Barfed, but the body was: " + result.get.body)
        }
      }
      case false => throw new Exception("VirtualTrainer.register: The result from the vt registration call was not defined")
    }
  }

  /**
   * @return A tuple with token and token secret
   */
  def login(username: String, password: String): Option[(String, String)] = {

    val result = vtRequest(vtPathLogin, headerNoToken()).post(loginBody(username, password)).value
    result.isDefined match {
      case true => {
        val Token = """(.*oauth_token=\")([^\"]*).*""".r
        val Secret = """(.*oauth_token_secret=\")([^\"]*).*""".r
        val token = result.get.header("Authorization") match {
          case Token(begin, t) => t
          case _ => ""
        }
        val secret = result.get.header("Authorization") match {
          case Secret(begin, s) => s
          case _ => ""
        }
        Some((token, secret))
      }
      case false => throw new Exception("VirtualTrainer.login: The result from the vt login call was not defined")
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