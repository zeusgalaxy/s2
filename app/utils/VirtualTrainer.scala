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
import play.api.mvc._
import scalaz._
import Scalaz._

// http://localhost:9000/n5iregister.jsp?machine_id=1070&id=2115180102&membership_id=1&email=sOCClkoE102%40stross.com&
// pic=22&DOB=03011960&gender=M&enableMail=true&weight=180&oem_tos=15

case class VtUser(vtUserId: String, vtNickname: String, vtToken: String, vtTokenSecret: String)
object VtUser {
  def apply(x: Elem, tok: String = "", tokSec: String = ""): VtUser = {
    val id = (x \\ "userId" head).text
    val nm = (x \\ "nickName" head).text
    VtUser(id, nm, tok, tokSec)
  }
}

case class RegParams(npLogin: String, email: String, pic: String, dob: String, machineId: String,
                      membershipId: String, gender: String, enableMail: String, weight: String,
                      oemTos: String, vtNickname: String, vtPassword: String)
object RegParams {
  def apply(rq: Request[AnyContent]): RegParams = {

    implicit val source = rq.body.asFormUrlEncoded match {
      case Some(form) => form
      case None => rq.queryString
    }

    def getS(k: String)(implicit p: Map[String, Seq[String]]) = {
      p.getOrElse(k, Seq(""))(0)
    }

    val npLogin = getS("id")
    val email = getS("email")
    val pic = getS("pic")
    val dob = getS("DOB")
    val machineId = getS("machine_id")
    val membershipId = getS("membership_id")
    val gender = getS("gender")
    val enableMail = getS("enableMail")
    val weight = getS("weight")
    val oemTos = getS("oem_tos")
    val vtNickname = email
    val vtp = getS("vt_password")
    val vtPassword = if (vtp == "") email else vtp
    RegParams(npLogin, email, pic, dob, machineId, membershipId, gender, enableMail, weight, oemTos, vtNickname, vtPassword)
  }
}

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

  private def registerBody(rp: RegParams): Validation[String, String] = {

    for {

      locationId <- Machine.getBasic(rp.machineId.toLong).
        toSuccess("machine_id " + rp.machineId + " not found in database").map(_.locationId)

    } yield {

      val json = ("age" -> age(rp.dob)) ~
        ("nickName" -> rp.vtNickname) ~
        ("password" -> (new sun.misc.BASE64Encoder()).encode(rp.vtPassword.getBytes("UTF-8"))) ~
        ("gender" -> rp.gender.toLowerCase) ~
        ("emailAddress" -> rp.email) ~
        ("weight" -> rp.weight) ~
        ("weightUnit" -> "I") ~
        ("preferredLanguageCode" -> "en_US") ~
        ("locationId" -> locationId)

      Printer.compact(JsonAST.render(json))
    }
  }

  private def linkBody(npLogin: String, vtUid: String) = {
    Printer.compact(JsonAST.render(
      ("externalUserId" -> npLogin) ~
//      ("externalUserId" -> "1") ~       // Put a not-previously-used number here for testing purposes
        ("vtUserId" -> vtUid) ~
        ("type" -> "NP")
    ))
  }

  private def loginBody(emailOrLogin: String, password: String) = {
    Printer.compact(JsonAST.render(
      ("username" -> (new sun.misc.BASE64Encoder()).encode(emailOrLogin.getBytes("UTF-8"))) ~
        ("password" -> (new sun.misc.BASE64Encoder()).encode(password.getBytes("UTF-8")))
    ))
  }

  def vtRequest(path: String, header: => String): WSRequestHolder = {
    val h = header
    WS.url(vtPathPrefix + path).withHeaders(("Content-Type", "application/json"), ("Authorization", h))
  }

  val regVtUserExists = 1
  val regVtUnableToGetStatus = 2
  val regVtOtherError = 3

  def register(rp: RegParams): Either[Int, VtUser] = {

    implicit val loc = VL("VirtualTrainer.register")

    val rVal: Option[(String, Int)] = for {

      rBody <- registerBody(rp).toOption
      valResult <- validate(waitVal(vtRequest(vtPathValidate, headerNoToken()).post(rBody), vtTimeout)).toOption
      valStatus = valResult.status

    } yield {
      (rBody, valStatus)
    }

    rVal match {
      case None => Left(regVtUnableToGetStatus)
      case Some((_, status)) if (status == 500) => Left(regVtUserExists)
      case Some((_, status)) if (status != 200) => Left(regVtOtherError)
      case Some((rBody, _)) =>
        val result: Validation[NonEmptyList[String], VtUser] = for {
          regResult <- validate(waitVal(vtRequest(vtPathRegister, headerNoToken()).post(rBody), vtTimeout))
          status <- test(regResult)(_.status == 200, "vt register result status was " + regResult.status.toString).
            error(Map("body" -> regResult.body))

          regXml <- validate(regResult.xml).add("regResult", regResult.toString)
          vtUid <- validate((regXml \\ "userId" head).text).add("regXml", regXml.toString)
          vtNickname <- validate((regXml \\ "nickName" head).text).add("regXml", regXml.toString)
          vtUser <- validate(VtUser(regXml)).add("regXml", regXml.toString)

        } yield {

          Logger.debug("linkBody will be: " + linkBody(rp.npLogin, vtUid).toString)

          val linkResult = waitVal(vtRequest(vtPathLink, headerNoToken()).post(linkBody(rp.npLogin, vtUid)), vtTimeout)
          if (linkResult.status != 200) Logger.info("VT link_external_user returned status " + linkResult.status.toString)
          vtUser
        }
        result.error(Map("msg"->"Something failed: ")).fold(e => Left(regVtOtherError), s => Right(s))
    }
  }

  def link(npLogin: String, vtUid: String): ValidationNEL[String, Boolean] = {

    implicit val loc = VL("VirtualTrainer.link")
    Logger.debug("linkBody will be: " + linkBody(npLogin, vtUid).toString)
    for {
      linkResult <- validate(waitVal(vtRequest(vtPathLink, headerNoToken()).post(linkBody(npLogin, vtUid)), vtTimeout))
      status <- test(linkResult)(_.status == 200, "vt link external account result status was " + linkResult.status.toString).
        error(Map("body" -> linkResult.body))
    } yield true
  }

  def login(emailOrNickname: String, vtPassword: String): ValidationNEL[String, (String, String, String)] = {

    implicit val loc = VL("VirtualTrainer.login")
    val tEx = """(.*oauth_token=\")([^\"]*).*""".r
    val tsEx = """(.*oauth_token_secret=\")([^\"]*).*""".r

    for {
      lBody <- validate(loginBody(emailOrNickname, vtPassword))
      loginResult <- validate(waitVal(vtRequest(vtPathLogin, headerNoToken()).post(lBody), vtTimeout))
      status <- test(loginResult)(_.status == 200, "vt login account result status was " + loginResult.status.toString).
        error(Map("body" -> loginResult.body))
      hdr <- loginResult.header("Authorization").toSuccess("Authorization header not found").liftFailNel
      token <- validate({
        val tEx(_, t) = tEx.findFirstIn(hdr).get;
        t
      })
      secret <- validate({
        val tsEx(_, s) = tsEx.findFirstIn(hdr).get;
        s
      })
      id <- validate((loginResult.xml \\ "userId" head).text)

    } yield (id, token, secret)
  }

  /**
   * @return An xml string with the predefined presets
   */
  def predefinedPresets(token: String, tokenSecret: String, model: String): ValidationNEL[String, NodeSeq] = {

    for {
      ppResult <- test(waitVal(vtRequest(vtPathPredefinedPresets, headerWithToken(token, tokenSecret)).
        get(), vtTimeout))(_.status == 200, "vt predefined presets result status != 200")
      segs <- validate(ppResult.xml \\ "workoutSegments")
    } yield segs.withFilter(s => (s \\ "deviceType").exists{ _.text == model}).map{ s => s }
  }

  /**
   * @return An xml string with the user's workouts
   */
  def workouts(token: String, tokenSecret: String, model: String): ValidationNEL[String, NodeSeq] = {

    for {
      ppResult <- test(waitVal(vtRequest(vtPathWorkouts, headerWithToken(token, tokenSecret))
        get(), vtTimeout))(_.status == 200, "vt workouts result status != 200")
      segs <- validate(ppResult.xml \\ "workoutSegments")
    } yield segs.withFilter(s => (s \\ "deviceType").exists{ _.text == model}).map{ s => s }
  }
}