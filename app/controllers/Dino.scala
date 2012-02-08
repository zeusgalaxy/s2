package controllers

import scalaz.{Logger => _, _}
import Scalaz._

import play.api._
import play.api.mvc._

import models._
import utils._

object Dino extends Controller {

  def forward(request: Request[AnyContent]): Validation[String, play.api.libs.ws.Response] = {

    validate {

      val (newRequest, newBody) = toWSRequest(request)

      request.method match {
        case "GET" => newRequest.get().value.get
        case "POST" => newRequest.post(newBody.getOrElse(throw new Exception("POST body is missing"))).value.get
        case "PUT" => newRequest.put(newBody.getOrElse(throw new Exception("PUT body is missing"))).value.get
        case "DELETE" => newRequest.delete().value.get
        case m => throw new Exception("Unexpected method in Dino.forward: " + m)
      }
    }
  }

  def passthru = Action {
    implicit request => {
      val r = forward(request)
      r.fold(e => Ok(e), s => Ok(s.ahcResponse.getResponseBodyAsBytes))
    }
  }

  // Local test: curl --header "Content-Type: text/xml; charset=UTF-8" -d@pageviews.xml http://localhost:9000/n5iworkout.jsp
  def pageview = Action {
    implicit request => {

      validate {

        /**
         * We receive a variety of different uploads in this method. If we don't find the one we're interested in
         * (which is "pageviews"), then simply forward this on to dino for processing. If it does, in fact, have
         * pageviews in the payload, then we process it here.
         */
        if ((~request.body.asXml \\ "pageviews").isEmpty)
          forward(request).fold(e => Ok(e), r => Ok(r.toString()))
        else {

          val cnt = PageViewModel.insert(~request.body.asXml).getOrThrow("Dino.pageview call of PageViewModel.insert")
          Ok("PageView load succeeded with " + cnt.toString + "inserts")
        }
      }.error("Dino.pageview", "request body: " + request.body.toString).fold(e => Ok("PageView load failed"), s => s)
    }
  }
}
