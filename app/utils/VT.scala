package utils

import scalaz.{Node => _, Logger => _, _}
import Scalaz._

import models._

import play.api.Play.current
import org.joda.time._
import play.api.libs.json._
import play.api.libs.json.Json._
import play.api.libs.ws._
import play.api.libs.ws.WS._
import xml._
import play.api.mvc._
import play.api.Logger

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
                     membershipId: String, gender: String, weight: String,
                     vtNickname: String, vtPassword: String)

object RegParams {

  val jodaMMDDYYYY = org.joda.time.format.DateTimeFormat.forPattern("MMddyyyy")

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
    val weight = getS("weight")
    val vtNickname = npLogin
    val vtp = getS("vt_password")
    val vtPassword = if (vtp == "") npLogin else vtp
    RegParams(npLogin, email, pic, dob, machineId, membershipId, gender, weight, vtNickname, vtPassword)
  }

  def apply(ex: Exerciser, machineId: Long): RegParams = {

    val gender = ex.gender ? "M" | "F"
    RegParams(ex.login, ex.email, ex.pic.toString, ex.dob.toString(jodaMMDDYYYY), machineId.toString,
      ex.membershipId.getOrElse("1"), gender, ex.weight.toString, ex.email, ex.email)
  }
}

object VT {

  def age(dob: String): Int = {
    val born = new DateTime(dob.slice(4, 8).toInt, dob.slice(0, 2).toInt, dob.slice(2, 4).toInt, 0, 0, 0)
    new Interval(born, DateTime.now).toPeriod.getYears
  }

  def headerNoToken(consKey: String = vtConsumerKey, consSecret: String = vtConsumerSecret): String =
    "OAuth oauth_consumer_key=\"" +
      consKey + "\", oauth_nonce=\"" + nonce + "\", oauth_timestamp=\"" + utcNowInSecs +
      "\", oauth_signature=\"" + b64Enc.encode(consSecret.getBytes("UTF-8")) + "\""

  def headerWithToken(token: String, tokenSecret: String,
                      consKey: String = vtConsumerKey, consSecret: String = vtConsumerSecret): String =
    "OAuth oauth_consumer_key=\"" +
      consKey + "\", oauth_nonce=\"" + nonce + "\", oauth_timestamp=\"" + utcNowInSecs +
      "\", oauth_token=\"" + token +
      "\", oauth_signature=\"" + b64Enc.encode((consSecret + "&" + tokenSecret).getBytes("UTF-8")) + "\""

  def validSegs(segsXml: String, model: String) = {
    for (w <- scala.xml.XML.loadString(segsXml) \\ "workoutSegments"
         if (w \\ "deviceType").exists {
           _.text == model
         }) yield w
  }

  private def registerBody(rp: RegParams): ValidationNEL[String, String] = {

    implicit val loc = VL("VT.registerBody")

    Logger.debug("VT registerBody params = " + rp.toString)

    vld {

      val locationId = Machine.getBasic(rp.machineId.toLong).get.locationId
      val json = JsObject(List(
        "age" -> JsNumber(age(rp.dob)),
        "nickName" -> JsString(rp.vtNickname),
        "password" -> JsString(b64Enc.encode(rp.vtPassword.getBytes("UTF-8"))),
        "gender" -> JsString(rp.gender.toLowerCase),
        "emailAddress" -> JsString(rp.email),
        "weight" -> JsString(rp.weight),
        "weightUnit" -> JsString("I"),
        "preferredLanguageCode" -> JsString("en_US"),
        "locationId" -> JsNumber(locationId)
       ))
      stringify(json)
    }.error
  }

  private def linkBody(npLogin: String, vtUid: String) = {
    stringify(JsObject(List(
      "externalUserId" -> JsString(npLogin),
        //      ("externalUserId" -> "1") ~       // Put a not-previously-used number here for testing purposes
       "vtUserId" -> JsString(vtUid),
       "type" -> JsString("NP")
    )))
  }

  private def loginBody(emailOrLogin: String, password: String) = {
    stringify(JsObject(List(
      "username" -> JsString(b64Enc.encode(emailOrLogin.getBytes("UTF-8"))),
       "password" -> JsString(b64Enc.encode(password.getBytes("UTF-8")))
    )))
  }

  def vtRequest(path: String, header: => String): WSRequestHolder = {
    val h = header
    WS.url(vtPathPrefix + path).withHeaders(("Content-Type", "application/json"), ("Authorization", h))
  }

  private def doVtRegister(rBody: String) = {
    waitVal(vtRequest(vtPathRegister, headerNoToken()).post(rBody), vtTimeout)
  }

  private def doVtLogin(lBody: String) = {
    waitVal(vtRequest(vtPathLogin, headerNoToken()).post(lBody), vtTimeout)
  }

  private def doVtLogout(token: String, tokenSecret: String) = {
    waitVal(vtRequest(vtPathLogout, headerWithToken(token, tokenSecret)).post(""), vtTimeout)
  }

  private def doVtLink(npLogin: String, vtUid: String) = {
    waitVal(vtRequest(vtPathLink, headerNoToken()).post(linkBody(npLogin, vtUid)), vtTimeout)
  }

  private def doVtPredefineds(token: String, tokenSecret: String) = {
    waitVal(vtRequest(vtPathPredefinedPresets, headerWithToken(token, tokenSecret)).get(), vtTimeout)
  }

  private def doVtWorkouts(token: String, tokenSecret: String) = {
    waitVal(vtRequest(vtPathWorkouts, headerWithToken(token, tokenSecret)).get(), vtTimeout)
  }

  def register(rp: RegParams): Either[Int, VtUser] = {

    implicit val loc = VL("VT.register")

    val rVal: Option[(String, Int)] = for {

    //      d0 <- vld({throw new Exception("at d0 in VT.register"); "d0"}).error.toOption
      rBody <- registerBody(rp).error.toOption
      //      d1 <- vld({throw new Exception("at d1 in VT.register"); "d1"}).error.toOption
      valResult <- vld(waitVal(vtRequest(vtPathValidate, headerNoToken()).post(rBody), vtTimeout)).error.toOption
      valStatus = valResult.status

    } yield {
      (rBody, valStatus)
    }

    rVal match {
      case None => Left(apiVtRegistrationUnableToGetStatus)
      case Some((_, status)) if (status == 500) => Left(apiVtRegistrationUserExists)
      case Some((_, status)) if (status != 200) => Left(apiVtRegistrationOtherError)
      case Some((rBody, _)) =>
        val result: Validation[NonEmptyList[String], VtUser] =
          for {
            regResult <- vld(doVtRegister(rBody))
            status <- tst(regResult)(_.status == 200).
              add("vt register result status", regResult.status.toString).
              add("body sent", rBody).
              add("body received", regResult.body).error

            regXml <- vld(regResult.xml).add("regResult", regResult.toString())
            vtUid <- vld((regXml \\ "userId" head).text).add("regXml", regXml.toString())
            vtNickname <- vld((regXml \\ "nickName" head).text).add("regXml", regXml.toString())
            vtUser <- vld(VtUser(regXml)).add("regXml", regXml.toString())
            linkResult <- link(rp.npLogin, vtUid)

          } yield vtUser

        result.add("Result", "Failure").error.fold(e => Left(apiVtRegistrationOtherError), s => Right(s))
    }
  }

  def link(npLogin: String, vtUid: String): ValidationNEL[String, Boolean] = {

    implicit val loc = VL("VT.link")

    (for {
      linkResult <- vld(doVtLink(npLogin, vtUid))
      status <- tst(linkResult)(_.status == 200).
        add("vt link external account result status", linkResult.status.toString).
        add("body", linkResult.body).error
    } yield true).error
  }

  def login(emailOrNickname: String, vtPassword: String): ValidationNEL[String, (String, String, String)] = {

    implicit val loc = VL("VT.login")
    val tEx = """(.*oauth_token=\")([^\"]*).*""".r
    val tsEx = """(.*oauth_token_secret=\")([^\"]*).*""".r

    (for {
      lBody <- vld(loginBody(emailOrNickname, vtPassword))
      loginResult <- vld(doVtLogin(lBody))
      status <- tst(loginResult)(_.status == 200).
        add("vt login result status", loginResult.status.toString).
        add("body", loginResult.body).error
      hdr <- loginResult.header("Authorization").toSuccess("Authorization header not found").liftFailNel

      token <- vld({
        val tEx(_, t) = tEx.findFirstIn(hdr).get;
        t
      }).add("Auth header", hdr)

      secret <- vld({
        val tsEx(_, s) = tsEx.findFirstIn(hdr).get;
        s
      }).add("Auth header", hdr)
      id <- vld((loginResult.xml \\ "userId" head).text).add("vt login xml", loginResult.xml.toString())

    } yield (id, token, secret)).error
  }

  def logout(token: String, tokenSecret: String): ValidationNEL[String, Boolean] = {

    implicit val loc = VL("VT.logout")

    (for {
      logoutResult <- vld(doVtLogout(token, tokenSecret))
      status <- tst(logoutResult)(_.status == 200).
        add("vt logout result status", logoutResult.status.toString).error

    } yield true).error
  }

  /**
   * @return An xml string with the predefined presets
   */
  def predefinedPresets(token: String, tokenSecret: String, model: String): ValidationNEL[String, NodeSeq] = {

    implicit val loc = VL("VT.predefinedPresets")

    (for {
      ppResult <- vld(doVtPredefineds(token, tokenSecret))
      status <- tst(ppResult)(_.status == 200).add("vt result status", ppResult.status.toString)
      segs <- vld(ppResult.xml \\ "workoutSegments").add("pp result xml", ppResult.xml.toString())

    } yield segs.withFilter(s => (s \\ "deviceType").exists {
        _.text == model
      }).map {
        s => s
      }).error
  }

  /**
   * @return An xml string with the user's workouts
   */
  def workouts(token: String, tokenSecret: String, model: String): ValidationNEL[String, NodeSeq] = {

    implicit val loc = VL("VT.workouts")

    (for {
      ppResult <- vld(doVtWorkouts(token, tokenSecret))
      status <- tst(ppResult)(_.status == 200).add("vt result status", ppResult.status.toString)
      segs <- vld(ppResult.xml \\ "workoutSegments")
    } yield segs.withFilter(s => (s \\ "deviceType").exists {
        _.text == model
      }).map {
        s => s
      }).error
  }

  def insertIntoXml(x: Node, parent: String, presets: NodeSeq, workouts: NodeSeq = NodeSeq.Empty) = {

    XmlMutator(x).add(parent, asXml(presets, workouts))
  }

  def asXml(presets: NodeSeq, workouts: NodeSeq = NodeSeq.Empty) = {

    <api error={apiNoError.toString}>
      <virtualTrainer>
        <predefinedPresets>
          {presets}
        </predefinedPresets>
        <workouts>
          {workouts}
        </workouts>
      </virtualTrainer>
    </api>
  }

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

}