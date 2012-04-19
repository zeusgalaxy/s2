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

/** Encapsulates the basic description of a VirtualTrainer user as resulting from a registration call
 * to the VT servers.
 *
 * @param vtUserId The permanent user id assigned by VirtualTrainer to this account
 * @param vtNickname The nickname associated with this account; provides an alternate means of loggin in to the VT website
 */
case class VtUser(vtUserId: String, vtNickname: String)

/**
 * Helper object to build our internal VtUser representation from the xml returned by the VT servers.
 */
object VtUser {
  def apply(x: Elem): VtUser = {
    val id = (x \\ "userId" head).text
    val nm = (x \\ "nickName" head).text
    VtUser(id, nm)
  }
}

/** Encapsulates all of the variables that will be needed when registering a user with VirtualTrainer.
 * Because users may or may not already be registered with Netpulse when the VT registration occurs, these
 * values may come from different places: either from the database (if an existing Netpulse exerciser) or
 * from the registration params passed in from the client.
 *
 * @param npLogin Netpulse login id
 * @param email Email address
 * @param dob Date of birth, in MMDDYYYY format
 * @param machineId Machine id from which the registration is occurring
 * @param gender M or F
 * @param weight Weight
 * @param vtNickname The nickname (alternate login id) for the exerciser at VT. Defaults to npLogin.
 * @param vtPassword The password for the exerciser at VT. Defaults to npLogin.
 */
case class VtRegistrationParams(npLogin: String, email: String, dob: String, machineId: String, gender: String,
                     weight: String, vtNickname: String, vtPassword: String)

/**
 * Helper object to instantiate RegParams, either from incoming parameters or from the exerciser's db record.
 */
object VtRegistrationParams {

  val jodaMMDDYYYY = org.joda.time.format.DateTimeFormat.forPattern("MMddyyyy")

  /** Populates a RegParams from values in an incoming request. Handles either a POST body or
   * GET query params. The outgoing nickname always defaults to the incoming npLogin. The
   * outgoing vt password will use any incoming value that is supplied; if none, it will
   * default to the npLogin.
   *
   * @param rq Incoming request values, either in the POST body or the GET query string
   * @return RegParams populated appropriately.
   */
  def apply(rq: Request[AnyContent]): VtRegistrationParams = {

    implicit val source = rq.body.asFormUrlEncoded match {
      case Some(form) => form
      case None => rq.queryString
    }

    def getS(k: String)(implicit p: Map[String, Seq[String]]) = {
      p.getOrElse(k, Seq(""))(0)
    }

    val npLogin = getS("id")
    val email = getS("email")
    val dob = getS("DOB")
    val machineId = getS("machine_id")
    val gender = getS("gender")
    val weight = getS("weight")
    val vtNickname = npLogin
    val vtp = getS("vt_password")
    val vtPassword = if (vtp == "") npLogin else vtp
    VtRegistrationParams(npLogin, email, dob, machineId, gender, weight, vtNickname, vtPassword)
  }

  /** Populates a RegParams from values in an existing exerciser's database record.
   * GET query params. The outgoing vt password and the vt nickname will both default to the exerciser's login.
   *
   * @param ex Exerciser encapsulation of database values
   * @param machineId Machine from which the registration is occurring
   * @return RegParams populated appropriately.
   */
  def apply(ex: Exerciser, machineId: Long): VtRegistrationParams = {

    val gender = ex.gender ? "M" | "F"
    VtRegistrationParams(ex.login, ex.email, ex.dob.toString(jodaMMDDYYYY), machineId.toString,
                  gender, ex.weight.toString, ex.login, ex.login)
  }
}

/**
 * Provides various functions for interfacing with the Virtual Trainer servers.
 */
trait VirtualTrainer {
  this: VirtualTrainer  with MachineDao
                        with EquipmentDao =>

  /** Calculates an age, in years, from a given birth date.
   *
   * @param dob Date of birth, in MMDDYYYY format
   * @return Age in years
   */
  def vtAge(dob: String): Int = {
    val born = new DateTime(dob.slice(4, 8).toInt, dob.slice(0, 2).toInt, dob.slice(2, 4).toInt, 0, 0, 0)
    new Interval(born, DateTime.now).toPeriod.getYears
  }

  /** Constructs an HTTP OAuth header that includes the Netpulse security components, but does NOT
   * include the exerciser-level security token. This header is appropriate for certain VT API calls
   * that are not "secured" at the user level.
   *
   * @param consKey The Netpulse consumer key, provided to us by VirtualTrainer
   * @param consSecret The Netpulse consumer secret, provided to us by Virtual Trainer
   * @return An HTTP OAuth header for use in "unsecured" calls to Virtual Trainer
   */
  def vtHeaderNoToken(consKey: String = vtConsumerKey, consSecret: String = vtConsumerSecret): String =
    "OAuth oauth_consumer_key=\"" +
      consKey + "\", oauth_nonce=\"" + nonce + "\", oauth_timestamp=\"" + utcNowInSecs +
      "\", oauth_signature=\"" + b64Enc.encode(consSecret.getBytes("UTF-8")) + "\""

  /** Constructs an HTTP OAuth header that includes the Netpulse security components, AND
   * includes the exerciser-level security token. This header is appropriate for VT API calls
   * that are "secured" at the user level.
   *
   * @param token The exerciser's VT session token
   * @param tokenSecret The exerciser's VT session token secret
   * @param consKey The Netpulse consumer key, provided to us by VirtualTrainer
   * @param consSecret The Netpulse consumer secret, provided to us by Virtual Trainer
   * @return An HTTP OAuth header for use in "secured" calls to Virtual Trainer
   */
  def vtHeaderWithToken(token: String, tokenSecret: String, consKey: String = vtConsumerKey,
                                                consSecret: String = vtConsumerSecret): String =
    "OAuth oauth_consumer_key=\"" +
      consKey + "\", oauth_nonce=\"" + nonce + "\", oauth_timestamp=\"" + utcNowInSecs +
      "\", oauth_token=\"" + token +
      "\", oauth_signature=\"" + b64Enc.encode((consSecret + "&" + tokenSecret).getBytes("UTF-8")) + "\""

  /** Builds the JSON registration body that will be passed to the VirtualTrainer servers.
   *
   * @param rp Registration param values, enapsulated into a single abstraction
   * @return ValidationNEL with error messages if problems, else stringified JSON with the registration params needed by VT
   */
  private def vtRegisterBody(rp: VtRegistrationParams): ValidationNEL[String, String] = {

    implicit val loc = VL("VT.registerBody")

    Logger.debug("VT registerBody params = " + rp.toString)

    vld {

      val locationId = mchGetBasic(rp.machineId.toLong).get.locationId
      val json = JsObject(List(
      "age" -> JsNumber(vtAge(rp.dob)),
      "nickName" -> JsString(rp.vtNickname),
      "password" -> JsString(b64Enc.encode(rp.vtPassword.getBytes("UTF-8"))),
      "gender" -> JsString(rp.gender.toLowerCase),
      "emailAddress" -> JsString(rp.email),
      "weight" -> JsString(rp.weight),
      "weightUnit" -> JsString("I"),
      "preferredLanguageCode" -> JsString("en_US"),
      "locationId" -> JsNumber(locationId)
      ))
      Logger.debug("VT registerBody json = " + stringify(json))
      stringify(json)
    }.error
  }

  /** Builds the JSON link_external_user body that will be passed to the VirtualTrainer servers.
   *
   * @param npLogin Netpulse login id of the exerciser being linked to Virtual Trainer
   * @param vtUid Virtual Trainer user id representing this exerciser
   * @return Stringified JSON with the link body needed by VT
   */
  private def vtLinkBody(npLogin: String, vtUid: String) = {
    stringify(JsObject(List(
      "externalUserId" -> JsString(npLogin),
       "vtUserId" -> JsString(vtUid),
       "type" -> JsString("NP")
    )))
  }

  /** Builds the JSON login body that will be passed to the Virtual Trainer servers.
   *
   * @param emailOrLogin Username value that will be basis for login; can either be their email or vt login id.
   * @param password Exerciser's password with VirtualTrainer
   * @return Stringified JSON with the login body needed by VT
   */
  private def vtLoginBody(emailOrLogin: String, password: String) = {
    stringify(JsObject(List(
      "username" -> JsString(b64Enc.encode(emailOrLogin.getBytes("UTF-8"))),
       "password" -> JsString(b64Enc.encode(password.getBytes("UTF-8")))
    )))
  }

  /** Prepares the Play! WSRequestHolder needed to execute a web services call to VT, given a destination
   * URL and a security header (which may or may not be at the user-level of security).
   *
   * @param path Endpoint for the call
   * @param secHdr HTTP OAuth header with security information
   * @return WSRequestHolder encapsulating the given call to VT
   */
  def vtRequest(path: String, secHdr: => String): WSRequestHolder =
    WS.url(vtPathPrefix + path).withHeaders(("Content-Type", "application/json"), ("Authorization", secHdr)).
      withTimeout(vtTimeout)

  /** Builds and executes the registration call to Virtual Trainer.
   *
   * @param rBody Registration body (JSON) needed by VT
   * @return play.api.libs.ws.WS.Response resulting from the call
   */
  private def vtDoRegister(rBody: String) = (vtRequest(vtPathRegister, vtHeaderNoToken()).post(rBody)).await(vtTimeout).get

  /** Builds and executes the login call to Virtual Trainer
   *
   * @param lBody Login body (JSON) needed by VT
   * @return play.api.libs.ws.WS.Response resulting from the call
   */
  private def vtDoLogin(lBody: String) = (vtRequest(vtPathLogin, vtHeaderNoToken()).post(lBody)).await(vtTimeout).get

  /** Builds and executes the logout call to Virtual Trainer
   *
   * @param token Exerciser's Virtual Trainer session token
   * @param tokenSecret Exerciser's Virtual Trainer session token secret
   * @return play.api.libs.ws.WS.Response resulting from the call
   */
  private def vtDoLogout(token: String, tokenSecret: String) =
    (vtRequest(vtPathLogout, vtHeaderWithToken(token, tokenSecret)).post("")).await(vtTimeout).get

  /** Builds and executes the link_external_user call to Virtual Trainer
   *
   * @param npLogin Exerciser's Netpulse login
   * @param vtUid Exerciser's Virtual Trainer user id
   * @return play.api.libs.ws.WS.Response resulting from the call
   */
  private def vtDoLink(npLogin: String, vtUid: String) =
    (vtRequest(vtPathLink, vtHeaderNoToken()).post(vtLinkBody(npLogin, vtUid))).await(vtTimeout).get

  /** Builds and executes the get_predefined_presets call to Virtual Trainer
   *
   * @param token Exerciser's Virtual Trainer session token
   * @param tokenSecret Exerciser's Virtual Trainer session token secret
   * @return play.api.libs.ws.WS.Response resulting from the call
   */
  private def vtDoPredefineds(token: String, tokenSecret: String) =
    (vtRequest(vtPathPredefinedPresets, vtHeaderWithToken(token, tokenSecret)).get()).await(vtTimeout).get

  /** Builds and executes the get_workouts call to Virtual Trainer
   *
   * @param token Exerciser's Virtual Trainer session token
   * @param tokenSecret Exerciser's Virtual Trainer session token secret
   * @return play.api.libs.ws.WS.Response resulting from the call
   */
  private def vtDoWorkouts(token: String, tokenSecret: String) =
    (vtRequest(vtPathWorkouts, vtHeaderWithToken(token, tokenSecret)).get()).await(vtTimeout).get

  /** Manages the process of registering an exerciser with Virtual Trainer, given a set of
   * registration values that have been packaged into a RegParams object. Will return either
   * an error code (left side) if problems, or a VtUser object representing the newly registered Virtual
   * Trainer user (right side) if successful. The error codes are derived from the api... series
   * of error codes, documented at https://netpulse.atlassian.net/wiki/display/netpulse/API+Error+Codes
   *
   * @param rp RegParams object populated with the registration values needed by Virtual Trainer
   * @return Either an error code if problems (Left), or a VtUser if successful (Right)
   */
  def vtRegister(rp: VtRegistrationParams): Either[Int, VtUser] = {

    implicit val loc = VL("VT.register")

    val rVal: Option[(String, Int)] = for {

      rBody <- vtRegisterBody(rp).error.toOption
      valResult <- vld((vtRequest(vtPathValidate, vtHeaderNoToken()).post(rBody)).await(vtTimeout).get).error.toOption
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
            regResult <- vld(vtDoRegister(rBody))
            status <- tst(regResult)(_.status == 200).
              add("vt register result status", regResult.status.toString).
              add("body sent", rBody).
              add("body received", regResult.body).error

            regXml <- vld(regResult.xml).add("regResult", regResult.toString())
            vtUid <- vld((regXml \\ "userId" head).text).add("regXml", regXml.toString())
            vtNickname <- vld((regXml \\ "nickName" head).text).add("regXml", regXml.toString())
            vtUser <- vld(VtUser(regXml)).add("regXml", regXml.toString())
            linkResult <- vtLink(rp.npLogin, vtUid)

          } yield vtUser

        result.add("Result", "Failure").error.fold(e => Left(apiVtRegistrationOtherError), s => Right(s))
    }
  }

  /** Manages the process of linking a given Netpulse user to a given Virtual Trainer account.
   *
   * @param npLogin Exerciser's Netpulse login
   * @param vtUid Virtual Trainer user id for this exerciser
   * @return ValidationNEL with error message(s) if problems, otherwise Boolean true indicating success
   */
  def vtLink(npLogin: String, vtUid: String): ValidationNEL[String, Boolean] = {

    implicit val loc = VL("VT.link")

    (for {
      linkResult <- vld(vtDoLink(npLogin, vtUid))
      status <- tst(linkResult)(_.status == 200).
        add("vt link external account result status", linkResult.status.toString).
        add("body", linkResult.body).error
    } yield true).error
  }

  /** Manages the process of logging a given Netpulse user into Virtual Trainer. Once logged in, the system
   * can retrieve predefined_presets and/or workouts for the exerciser. The login session remains active
   * until explictly logged out.
   *
   * @param emailOrNickname Username value that will be basis for login; can either be their email or vt login id.
   * @param vtPassword Exerciser's password with Virtual Trainer
   * @return ValidationNEL with error string(s) if problems, else a tuple of vt id, token and token secret
   */
  def vtLogin(emailOrNickname: String, vtPassword: String): ValidationNEL[String, (String, String, String)] = {

    implicit val loc = VL("VT.login")
    val tEx = """(.*oauth_token=\")([^\"]*).*""".r
    val tsEx = """(.*oauth_token_secret=\")([^\"]*).*""".r

    (for {
      lBody <- vld(vtLoginBody(emailOrNickname, vtPassword))
      loginResult <- vld(vtDoLogin(lBody))
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

  /** Manages the process of logging an exerciser out of their currently active session with Virtual Trainer.
   * Once logged out, they will need to relogin in order to access predefined_presets and/or workouts.
   * Logging in again will require that they provide their Virtual Trainer password.
   *
   * @param token The exercisers' current Virtual Trainer session token
   * @param tokenSecret The exerciser's current Virtual Trainer session token secret
   * @return ValidationNEL with error string(s) if a problem occurred, otherwise Boolean true
   */
  def vtLogout(token: String, tokenSecret: String): ValidationNEL[String, Boolean] = {

    implicit val loc = VL("VT.logout")

    (for {
      logoutResult <- vld(vtDoLogout(token, tokenSecret))
      status <- tst(logoutResult)(_.status == 200).
        add("vt logout result status", logoutResult.status.toString).error

    } yield true).error
  }

  /** Returns all Virtual Trainer predefined presets for the given exerciser session (represented by the Virtual
   * Trainer token and token secret) which are applicable to the given machine model.

   * @param token The exercisers' current Virtual Trainer session token
   * @param tokenSecret The exerciser's current Virtual Trainer session token secret
   * @return An xml string with the applicable predefined presets
   */
  def vtPredefinedPresets(token: String, tokenSecret: String, model: String): ValidationNEL[String, NodeSeq] = {

    implicit val loc = VL("VT.predefinedPresets")

    (for {
      ppResult <- vld(vtDoPredefineds(token, tokenSecret))
      status <- tst(ppResult)(_.status == 200).add("vt result status", ppResult.status.toString)
      segs <- vld(ppResult.xml \\ "workoutSegments").add("pp result xml", ppResult.xml.toString())

    } yield segs.withFilter(s => (s \\ "deviceType").exists {
        _.text == model
      }).map {
        s => s
      }).error
  }

  /** Returns all Virtual Trainer workouts for the given exerciser session (represented by the Virtual
   * Trainer token and token secret) which are applicable to the given machine model.

   * @param token The exercisers' current Virtual Trainer session token
   * @param tokenSecret The exerciser's current Virtual Trainer session token secret
   * @return An xml string with the applicable predefined presets
   */
  def vtWorkouts(token: String, tokenSecret: String, model: String): ValidationNEL[String, NodeSeq] = {

    implicit val loc = VL("VT.workouts")

    (for {
      ppResult <- vld(vtDoWorkouts(token, tokenSecret))
      status <- tst(ppResult)(_.status == 200).add("vt result status", ppResult.status.toString)
      segs <- vld(ppResult.xml \\ "workoutSegments")
    } yield segs.withFilter(s => (s \\ "deviceType").exists {
        _.text == model
      }).map {
        s => s
      }).error
  }

  /** Utility function that accepts XML of predefined_presets and workouts, and inserts those
   * into another XML chunk under the specified element. This function is needed because the
   * presets and workouts that are returned from Virtual Trainer are often packaged inside some
   * other XML data (such as the XML coming back from a Dino register or login call) before
   * being returned to the caller.
   *
   * @param x XML into which the presets and workoutw will be inserted
   * @param parent Element name which will be the parent for the presets and workouts
   * @param presets XML of predefined presets to be inserted under the parent element of the target XML
   * @param workouts XML of workouts to be inserted under the parent element of the target XML
   * @return XML which combines the original input with the given presets and workouts
   */
  def vtInsertIntoXml(x: Node, parent: String, presets: NodeSeq, workouts: NodeSeq = NodeSeq.Empty) =
          XmlMutator(x).add(parent, vtAsApiResult(presets, workouts))

  /** Packages the provided predefined_presets and workouts into an XML chunk with <api> as the
   * parent element.
   *
   * @param presets XML of predefined presets retrieved from Virtual Trainer
   * @param workouts XML of workouts retrieved from Virtual Trainer
   * @return XML which packages the given presets and workouts inside standardized elements
   */
  def vtAsApiResult(presets: NodeSeq, workouts: NodeSeq = NodeSeq.Empty) = {

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