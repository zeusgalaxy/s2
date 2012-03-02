package controllers

import scalaz.{Logger => _, _}
import Scalaz._

import play.api.http._
import play.api.mvc._
import play.api.Play.current

import utils._
import models._
import play.api.Logger

object DinoWrapper extends Controller {

  lazy val dinoTimeout = current.configuration.getString("dino.timeout").getOrElse(throw new Exception("dino.timeout not in configuration")).toInt

  def forward(request: Request[AnyContent]): ValidationNEL[String, play.api.libs.ws.Response] = {

    validate {

      val (newRequest, newBody) = toWSRequest(request)

//      Logger.debug("DinoWrapper.forward old request was " + request.toString())
//      Logger.debug("DinoWrapper.forward new request is " + newRequest.toString())
      request.method match {
        case "GET" => {
          waitVal(newRequest.get(), dinoTimeout)
        }
        case "POST" => {

          request.body match {
            case AnyContentAsFormUrlEncoded(fueBody) => {
              val wrt = Writeable.writeableOf_urlEncodedForm
              val ct = ContentTypeOf.contentTypeOf_urlEncodedForm
              waitVal(newRequest.post[Map[String, Seq[String]]](fueBody)(wrt, ct), dinoTimeout)
            }
            case _ => waitVal(newRequest.post(newBody.get), dinoTimeout)
          }
        }
        case "PUT" => newRequest.put(newBody.get).value.get
        case "DELETE" => newRequest.delete().value.get
        case m => throw new Exception("Unexpected method in Dino.forward: " + m)
      }
    }
  }

  def passthru = Action {
    implicit request => {
      val r = forward(request)
      r.fold(e => InternalServerError("Problem during dino passthru. Errors: " + e.list.mkString(", ")),
        s => Ok(s.ahcResponse.getResponseBodyAsBytes))
    }
  }

  // Local test: curl --header "Content-Type: text/xml; charset=UTF-8" -d@pageviews.xml http://localhost:9000/n5iworkout.jsp
  def pageview = Action {

    implicit request => {

      implicit val loc = VL("Dino.pageView")

      validate {

        /**
         * We receive a variety of different uploads in this method. If we don't find the one we're interested in
         * (which is "pageviews"), then simply forward this on to dino for processing. If it does, in fact, have
         * pageviews in the payload, then we process it here.
         */
        if ((~request.body.asXml \\ "pageviews").isEmpty)
          forward(request).fold(e => InternalServerError("Problems forward pageview call. Errors: " + e.list.mkString(", ")),
            r => Ok(r.toString()))
        else {

          val cnt = PageViewModel.insert(~request.body.asXml).getOrThrow("Dino.pageview call of PageViewModel.insert")
          Ok("PageView load succeeded with " + cnt.toString + "inserts")
        }
      }.add("request body", request.body.toString).error.
        fold(e => InternalServerError("PageView load failed. Errors: " + e.list.mkString(", ")), s => s)
    }
  }

  // To test a post with curl, passing a file for the body: curl --header "Content-Type: text/xml; charset=UTF-8" -d@asset_history_upload.xml http://localhost:8080/n5iuploader.jsp
  // http://localhost:9000/n5iregister.jsp?machine_id=1070&id=2115180102&membership_id=1&email=sOCClkoE102%40stross.com&pic=22&DOB=03011960&gender=M&enableMail=true&weight=180&oem_tos=15

  def register = Action {
    implicit request =>

      val genFailElem = <s2RegisterResult>Unable to complete registration</s2RegisterResult>
      implicit val loc = VL("DinoWrapper.register")

      val rp = RegParams(request)
      val dinoXml: Validation[NonEmptyList[String], scala.xml.Elem] = for {
        dinoResult <- forward(request)
        dXml <- validate(dinoResult.xml)
      } yield dXml

      dinoXml.error
      val oldXml = dinoXml | genFailElem

      // either error code or object encapsulating vt user
      val rVal: Either[Int, VtUser] = (for {
        code <- test((oldXml \\ "response" \ "@code").text)(_ == "0", "oldXml response code != 0")
        vtUser <- validate(VirtualTrainer.register(rp))
      } yield {
        vtUser
      }).error.fold(e => Left(99), s => s)

      val finalResult = rVal match {

        case Left(err) =>
          validate(XmlMutator(oldXml).add("response", <vtAccount status={err.toString}></vtAccount>))
        case Right(vtUser) =>
          for {
            vtAuth <- VirtualTrainer.login(vtUser.vtNickname, vtUser.vtNickname) // tuple(token, tokenSecret)
            (vtUid, vtToken, vtTokenSecret) = vtAuth
            updResult <- validate(Exerciser.updateVirtualTrainer(rp.npLogin, vtUid, vtToken, vtTokenSecret))
            machineId <- validate(rp.machineId.toLong)
            model <- Machine.getWithEquip(machineId).flatMap(_._2.map(e => e.model.toString)).toSuccess(NonEmptyList("Unable to retrieve machine/equipment/model"))

            vtPredefinedPresets <- VirtualTrainer.predefinedPresets(vtToken, vtTokenSecret, model)
            vtWorkouts <- VirtualTrainer.workouts(vtToken, vtTokenSecret, model)

          } yield {
            XmlMutator(oldXml).add("response",
              <vtAccount status="0">
                <vtUid>
                  {vtUid}
                </vtUid>
                <vtNickName>
                  {vtUser.vtNickname}
                </vtNickName>
                <vtToken>
                  {vtToken}
                </vtToken>
                <vtTokenSecret>
                  {vtTokenSecret}
                </vtTokenSecret>
                <vtPredefinedPresets>
                  {vtPredefinedPresets}
                </vtPredefinedPresets>
                <vtWorkouts>
                  {vtWorkouts}
                </vtWorkouts>
              </vtAccount>
            )
          }
      }
      finalResult.fold(e => Ok(e.list.mkString(", ")), s => Ok(s))
  }

  // http://qa-ec2.netpulse.ws/core/n5ilogin.jsp?machine_id=18&id=1112925684&pic=22&oem_tos=15
  // http://localhost:9000/n5ilogin.jsp?machine_id=1070&id=2115180111&pic=22&oem_tos=15

  def login = Action {
    implicit request =>

      implicit val loc = VL("ApiWrapper.login")

      // Note that what is called "id" in the request is actually "login" in the database (per dino!)
      val params = postOrGetParams(request, List("id", "machine_id"))

      (for {
        dinoResult <- forward(request)
        oldXml <- validate(dinoResult.xml)
      } yield {
        (oldXml \\ "response" \ "@code").find(n => true) match {

          case Some(code) if (code.text == "0") => {

            for {
              npLogin <- validate(params("id")(0))
              machineId <- validate(params("machine_id")(0).toLong)
              model <- Machine.getWithEquip(machineId).flatMap(_._2.map(e => e.model.toString)).toSuccess(NonEmptyList("Unable to retrieve machine/equipment/model"))

              ex <- Exerciser.findByLogin(npLogin).getOrFail("Exerciser " + npLogin + " not found in ApiWrapper.login")
              vtPredefinedPresets <- VirtualTrainer.predefinedPresets(ex.vtToken, ex.vtTokenSecret, model)
              vtWorkouts <- VirtualTrainer.workouts(ex.vtToken, ex.vtTokenSecret, model)

            } yield

              XmlMutator(oldXml).add("response",
                <vtAccount>
                  <vtToken>
                    {ex.vtToken}
                  </vtToken>
                  <vtTokenSecret>
                    {ex.vtTokenSecret}
                  </vtTokenSecret>
                  <vtPredefinedPresets>
                    {vtPredefinedPresets}
                  </vtPredefinedPresets>
                  <vtWorkouts>
                    {vtWorkouts}
                  </vtWorkouts>
                </vtAccount>
              )
          }.debug.toOption.getOrElse(XmlMutator(oldXml).add("response", <vtAccount></vtAccount>))

          case _ => oldXml
        }
      }).debug.fold(e => Ok(<response desc="Login failed" code="1">
        {e.list.mkString(", ")}
      </response>), s => Ok(s))
  }
}
