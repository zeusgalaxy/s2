package controllers

import org.apache.commons.lang.RandomStringUtils

import scalaz.{Logger => _, _}
import Scalaz._
import scala.util.control.Exception._
import scala.xml._

import play.api._
import http.Writeable
import play.api.libs.ws._
import play.api.libs.ws.WS._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

import views._
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

      if ((~request.body.asXml \\ "pageviews").isEmpty) forward(request).fold(e => Ok(e), r => Ok(r.toString()))

      else {

        request.body.asXml match {

          case Some(xml) =>
            def fMsg(e: String) = "PageView Fail: error xml: vvvvvvvvv \n" + xml.toString + "\nend xml ^^^^^^^^^^^" + e
            def sMsg(i: Int) = "PageView rows inserted:" + i.toString
            PageViewModel.insert(xml).fold(e => {
              Logger.error(fMsg(e));
              Ok("Failure")
            },
              i => {
                Logger.debug(sMsg(i));
                Ok("Success")
              })
          case _ => {
            Logger.info("PageView: No xml body");
            Ok("Failure")
          }
        }

      }

      // Pass the request with xml payload to old app here
      //WS.url("localhost:9000/echo").post(request)
    }
  }
}
