package controllers

import org.apache.commons.lang.RandomStringUtils

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

import views._
import models._
import utils._


object Application extends Controller with Secured {

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
      println(requestMap.toString)

      registrationForm.bindFromRequest.fold(
        formWithErrors => BadRequest(html.createRegistrationForm(formWithErrors)),
        registration => {
          Redirect(routes.Application.index).flashing("success" -> "Registration resulted in: %s".format(Registration.test(registration).toString()))
        }
      )
  }

  def testLogin = IsAuthenticated("/testLogin", username => implicit request =>
      Ok(html.testLogin())
      )

}
