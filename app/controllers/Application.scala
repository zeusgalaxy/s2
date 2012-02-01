package controllers

import org.apache.commons.lang.RandomStringUtils

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.Play.current

import _root_.models._
import views._
import models._

object Application extends Controller {

  val registrationForm = Form(
    of(Registration.apply _, Registration.unapply _)(
      "machine_id" -> number,
      "id" -> longNumber,
      "membership_id" -> optional(number),
      "email" -> email,
      "pic" -> optional(number),
      "DOB" -> date("MMddyyyy"),
      "gender" -> nonEmptyText(1, 1),
      "enableMail" -> boolean,
      "weight" -> number,
      "oem_tos" -> optional(number)
    )
  )

  def index = Action {
    implicit request =>
      Ok(html.index("This is the main page parameter"))
  }

  val Home = Redirect(routes.Application.listUsers(0, 2, ""))

  def listUsers(page: Int, orderBy: Int, filter: String) = Action {
    implicit request =>

      Ok(html.listUsers(
        User.list(page = page, orderBy = orderBy, filter = ("%" + filter + "%")),
        orderBy, filter
      ))
  }

  def createRegistration = Action {
    val boundForm = registrationForm.bind(Map("machine_id" -> "18", "id" -> ("111" + RandomStringUtils.randomNumeric(7)), "membership_id" -> "1",
      "email" -> (RandomStringUtils.randomAlphabetic(8) + "@stross.com"),
      "pic" -> "22", "DOB" -> "03011960", "gender" -> "M", "enableMail" -> "true", "weight" -> "180", "oem_tos" -> "15"))
    Ok(html.createRegistrationForm(boundForm))
  }

  def testRegistration = Action {
    implicit request =>

      println("handling testRegistration")

      val requestMap = request.body.asFormUrlEncoded
      //      val data = requestMap.asInstanceOf[WrappedArray[String]].head
      println(requestMap.toString)

      //      request.body match {
      //        case AnyContentAsUrlFormEncoded(data) => {
      //          val b = request.body.asUrlFormEncoded
      //          for (key <- b.get.keys; value <- b.getOrElse(key, None)) println(key + " : " + Some(value))
      //
      //
      ////          for (key <- data.keys; value <- data.getOrElse(key, None))
      ////            println(key + " : " + value)
      //        }
      //        case _ =>
      //      }

      registrationForm.bindFromRequest.fold(
        formWithErrors => BadRequest(html.createRegistrationForm(formWithErrors)),
        registration => {
          Redirect(routes.Application.index()).flashing("success" -> "Registration resulted in: %s".format(Registration.test(registration).toString()))
        }
      )

    //    WS.url("http://stross.com/getStuff").get(resp => println(resp))
    /**
     * http://qa-ec2.netpulse.ws/core/n5iregister.jsp?machine_id=18&id=5103369779&membership_id=1&email=kenner%40stross.com&pic=22&DOB=01012001&gender=M&enableMail=1&weight=180&oem_tos=15
     * <response code="1" desc="Invalid argument(s)"/>
     * <response code="0" desc="Record created">
      <adunits>
      <adunit code="startup_video" period="1">
      <content id="1230" pct="100" show="false" default="true"/>
      </adunit>
     .
     .
     .
      <adunit code="branded_carousel" period="1">
      <content id="1624" show="false" default="true"/>
      </adunit>
      </adunits>
      </response>
     */
  }

  def testLogin = Action {
    implicit request =>
      Ok(html.testLogin())
  }
}
