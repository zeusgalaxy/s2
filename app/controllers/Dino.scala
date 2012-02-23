package controllers

import scalaz.{Logger => _, _}
import Scalaz._

import play.api.Play.current
import play.api.mvc._
import play.api.libs.ws.Response
import play.api.libs.concurrent.Promise
import play.api.http._

import models._
import utils._

object Dino extends Controller {

  lazy val dinoTimeout = current.configuration.getString("dino.timeout").getOrElse(throw new Exception("dino.timeout not in configuration")).toInt

  def forward(request: Request[AnyContent]): ValidationNEL[String, play.api.libs.ws.Response] = {

    validate {

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

      implicit val loc: ValLoc = "Dino.pageView"

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
      }.error(Map("request body" -> request.body.toString)).
        fold(e => InternalServerError("PageView load failed. Errors: " + e.list.mkString(", ")), s => s)
    }
  }
}
