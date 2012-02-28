package utils

import scalaz.{Node => _, Logger => _, _}
import Scalaz._

import models._

import play.api.Play.current
import org.joda.time._
import org.apache.commons.lang._
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import play.api.Logger
import play.api.libs.ws._
import play.api.libs.ws.WS._
import play.api.libs.concurrent.Promise
import xml.{Elem, NodeSeq}


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
  lazy val vtTimeout = current.configuration.getString("vt.timeout").getOrElse(throw new Exception("vt.timeout not in configuration")).toInt

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

  def validSegs(segsXml: String, model: String) = {
    for (w <- scala.xml.XML.loadString(segsXml) \\ "workoutSegments"
         if (w \\ "deviceType").exists {
           _.text == model
         }) yield w
  }

  private def registerBody(params: Map[String, Seq[String]])(implicit nmField: String = "id", pwdField: String = "id"):
  Validation[String, String] = {

    for {

      machineId <- params.get("machine_id").toSuccess("machine_id not supplied").map(_(0))
      locationId <- Machine.getBasic(machineId.toLong).
        toSuccess("machine_id " + machineId + " not found in database").map(_.locationId)
      password <- params.get(pwdField).toSuccess(pwdField + " password not supplied").map(_(0))
      dob <- params.get("DOB").toSuccess("DOB not supplied").map(_(0))
      nickName <- params.get(nmField).toSuccess(nmField + " nickName not supplied").map(_(0))
      gender <- params.get("gender").toSuccess("gender not supplied").map(_(0).toLowerCase)
      emailAddr <- params.get("email").toSuccess("email not supplied").map(_(0))
      weight <- params.get("weight").toSuccess("weight not supplied").map(_(0))

    } yield {

      val json = ("age" -> age(dob)) ~
        ("nickName" -> nickName) ~
        ("password" -> (new sun.misc.BASE64Encoder()).encode(password.getBytes("UTF-8"))) ~
        ("gender" -> gender) ~
        ("emailAddress" -> emailAddr) ~
        ("weight" -> weight) ~
        ("weightUnit" -> "I") ~
        ("preferredLanguageCode" -> "en_US") ~
        ("locationId" -> locationId)

      Printer.compact(JsonAST.render(json))
    }
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

  def vtRequest(path: String, header: => String): WSRequestHolder = {
    val h = header
    WS.url(vtPathPrefix + path).withHeaders(("Content-Type", "application/json"), ("Authorization", h))
  }

  /**
   * @return Either an error code OR a tuple with Virtual Trainer userId, nickName and password
   */
  val regVtUserExists = 1
  val regVtUnableToGetStatus = 2
  val regVtOtherError = 3

  def register(params: Map[String, Seq[String]]): Either[Int, (String, String, String)] = {

    implicit val loc = VL("VirtualTrainer.register")

    val rVal: Option[(String, String, Int)] = for {

      npId <- params.get("id").map(_(0))
      rBody <- registerBody(params).toOption
      valResult <- validate(waitVal(vtRequest(vtPathValidate, headerNoToken()).post(rBody), vtTimeout)).toOption
      valStatus = valResult.status

    } yield {
      (npId, rBody, valStatus)
    }

    rVal match {
      case None => Left(regVtUnableToGetStatus)
      case Some((_, _, status)) if (status == 500) => Left(regVtUserExists)
      case Some((_, _, status)) if (status != 200) => Left(regVtOtherError)
      case Some((npId, rBody, _)) =>
        val result = for {
          regResult <- some(waitVal(vtRequest(vtPathRegister, headerNoToken()).post(rBody), vtTimeout))
          if (regResult.status == 200)
          regXml <- validate(regResult.xml).toOption
          id <- validate((regXml \\ "userId" head).text).toOption
          nickName <- validate((regXml \\ "nickName" head).text).toOption

        } yield {

          val linkResult = waitVal(vtRequest(vtPathLink, headerNoToken()).post(linkBody(npId, id)), vtTimeout)
          if (linkResult.status != 200) Logger.info("VT link_external_user returned status " + linkResult.status.toString)
          (id, nickName, nickName)

        }
        result match {
          case None => Left(regVtOtherError)
          case Some(r) => Right(r)
        }
    }
  }

  /**
   * @return A tuple with token and token secret, which we also save in the Exerciser table
   */
  def login(vtUsername: String, vtPassword: String, npLogin: String): ValidationNEL[String, (String, String)] = {

    val tEx = """(.*oauth_token=\")([^\"]*).*""".r
    val tsEx = """(.*oauth_token_secret=\")([^\"]*).*""".r

    for {
      lBody <- validate(loginBody(vtUsername, vtPassword))
      loginResult <- test(waitVal(vtRequest(vtPathLogin, headerNoToken()).post(lBody), vtTimeout))(_.status == 200)
      hdr <- loginResult.header("Authorization").toSuccess("Authorization header not found").liftFailNel
      token <- validate({
        val tEx(_, t) = tEx.findFirstIn(hdr).get;
        t
      })
      secret <- validate({
        val tsEx(_, s) = tsEx.findFirstIn(hdr).get;
        s
      })

    } yield (token, secret)
  }

  /**
   * @return An xml string with the predefined presets
   */
  def predefinedPresets(token: String, tokenSecret: String, model: String): ValidationNEL[String, NodeSeq] = {

    for {
      ppResult <- test(waitVal(vtRequest(vtPathPredefinedPresets, headerWithToken(token, tokenSecret)).get(), vtTimeout))(_.status == 200)
      segs <- validate(ppResult.xml \\ "workoutSegments")
      ws <- test(segs)(ws => (ws \\ "deviceType").exists(_.text == model))
    } yield ws
  }

  /**
   * @return An xml string with the user's workouts
   */
  def workouts(token: String, tokenSecret: String, model: String): ValidationNEL[String, NodeSeq] = {

    for {
      ppResult <- test(waitVal(vtRequest(vtPathWorkouts, headerWithToken(token, tokenSecret)).get(), vtTimeout))(_.status == 200)
      segs <- validate(ppResult.xml \\ "workoutSegments")
      ws <- test(segs)(ws => (ws \\ "deviceType").exists(_.text == model))
    } yield ws
  }
}