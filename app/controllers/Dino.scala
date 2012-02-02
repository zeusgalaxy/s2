package controllers

import org.apache.commons.lang.RandomStringUtils

import scalaz.{ Logger => _, _ }
import Scalaz._
import scala.util.control.Exception._
import scala.xml._

import play.api._
import libs.ws.WS
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

import views._
import models._
import utils._

object Dino extends Controller {

  def passthru = {

    try {

      val result = WS.url(v1Addr).get().value
      result.isDefined match {
        case true => {
          val oldXml = result.get.xml
          (oldXml \\ "response" \ "@code").find(n => true) match {
            case Some(code) if (code.text == "0") => {
              val (vtUid, vtNickName, vtPassword) = VirtualTrainer.register(params).get
              val (vtToken, vtTokenSecret) = VirtualTrainer.login(vtNickName, vtPassword).get
              val vtPredefinedPresets = VirtualTrainer.predefinedPresets(vtToken, vtTokenSecret)
              val vtWorkouts = VirtualTrainer.workouts(vtToken, vtTokenSecret)
              VirtualTrainer.logout(vtToken, vtTokenSecret)

              Ok(XmlMutator(oldXml).add("response",
                <vtAccount>
                  <vtUid>{vtUid}</vtUid>
                  <vtNickName>{vtNickName}</vtNickName>
                  <vtPassword>{vtPassword}</vtPassword>
                  <vtToken>{vtToken}</vtToken>
                  <vtTokenSecret>{vtTokenSecret}</vtTokenSecret>
                  <vtPredefinedPresets>{vtPredefinedPresets}</vtPredefinedPresets>
                  <vtWorkouts>{vtWorkouts}</vtWorkouts>
                </vtAccount>
              ))
            }
            case _ => Ok(oldXml)
          }
        }
        case false => throw new Exception("The result from v1 registration call was not defined")
      }

    } catch {
      case ex => Ok("Something went wrong: " + ex.getMessage)
    }
  }

  // Local test: curl --header "Content-Type: text/xml; charset=UTF-8" -d@pageviews.xml http://localhost:9000/
  def pageview = Action {
    implicit request => {

      def doPageViews = {
        handling(classOf[PE]) by (_ => new Date) apply parseDate
        handling(_) by (_ =>
      }

      if ((~request.body.asXml \\ "pageviews").isEmpty) Ok("empty") else Ok("not empty")

      request.body.asXml match {
        case Some(xml)  =>
          // for ( x <- xml \\ "pageviews") {
          throwableToLeft {
            PageViewModel.insert(xml)
          } match {
            case Right(s) => {
              Logger.debug("PageView rows inserted:" + s)
              Ok("Success")
            }
            case Left(e) => {
              Logger.error("PageView Fail: error xml: vvvvvvvvv \n" + xml.toString + "\nend xml ^^^^^^^^^^^", e );
              Ok("Failure")
            }
          }
        //}
        case _ => { Logger.info("PageView: No xml body"); Ok("Failure") }
      }
      // Pass the request with xml payload to old app here
      //WS.url("localhost:9000/echo").post(request)
    }
  }

  val echo = Action { request =>
    try {
      request.body.asXml match {
        case Some(xml)  => { Ok("Hello!: got request [" + xml + "]") }
        case _ =>  Ok("Fail")
      }
    }
    catch {
      case e: Exception =>  { Logger.debug("PageViewModel Err"); Ok("Failure") }
    }
  }
}
