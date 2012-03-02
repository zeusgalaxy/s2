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

    vld {

      val (newRequest, newBody) = toWSRequest(request)

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

      vld {

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

      implicit val loc = VL("DinoWrapper.register")

      val rp = RegParams(request)
      val oldXml = forward(request).flatMap { r => vld(r.xml) }.error | <s2Reg>Unable to register</s2Reg>

      // either error code or object encapsulating vt user
      val rVal: Either[Int, VtUser] = (for {
        code <- tst((oldXml \\ "response" \ "@code").text)(_ == "0", "oldXml response code != 0")
        vtUser <- vld(VT.register(rp))
      } yield {
        vtUser
      }).error.fold(e => Left(99), s => s)

      val finalResult = rVal match {

        case Left(err) =>
          vld(XmlMutator(oldXml).add("response", <vtAccount status={err.toString}></vtAccount>))
        case Right(vtUser) =>
          for {
            vtAuth <- VT.login(vtUser.vtNickname, vtUser.vtNickname)
            (vtUid, vtToken, vtTokenSecret) = vtAuth
            updResult <- vld(Exerciser.updVT(rp.npLogin, vtUid, vtToken, vtTokenSecret))
            machineId <- vld(rp.machineId.toLong)
            model <- Machine.getWithEquip(machineId).flatMap(_._2.map(e => e.model.toString)).
              toSuccess(NonEmptyList("Unable to rtrv mach/equip/model"))

            vtPredefinedPresets <- VT.predefinedPresets(vtToken, vtTokenSecret, model)
            vtWorkouts <- VT.workouts(vtToken, vtTokenSecret, model)

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
      finalResult.error.fold(e => Ok(e.list.mkString(", ")), s => Ok(s))
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
        oldXml <- vld(dinoResult.xml)
      } yield {
        (oldXml \\ "response" \ "@code").find(n => true) match {

          case Some(code) if (code.text == "0") => {

            for {
              npLogin <- vld(params("id")(0))
              machineId <- vld(params("machine_id")(0).toLong)
              model <- Machine.getWithEquip(machineId).flatMap(_._2.map(e => e.model.toString)).
                toSuccess(NonEmptyList("Unable to retrieve machine/equipment/model"))

              ex <- Exerciser.findByLogin(npLogin).getOrFail("Exerciser " + npLogin + " not found in ApiWrapper.login")
              vtPredefinedPresets <- VT.predefinedPresets(ex.vtToken, ex.vtTokenSecret, model)
              vtWorkouts <- VT.workouts(ex.vtToken, ex.vtTokenSecret, model)

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
          }.error.toOption.getOrElse(XmlMutator(oldXml).add("response", <vtAccount></vtAccount>))

          case _ => oldXml
        }
      }).error.fold(e => Ok(<response desc="Login failed" code="1">
        {e.list.mkString(", ")}
      </response>), s => Ok(s))
  }
}
