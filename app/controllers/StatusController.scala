package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

import anorm._

import views._
import models._
import security._
import org.joda.time.DateTime


object StatusController extends Controller with StatusDao {
  /**
   * Return the application status to external monitors. If the return string contains "ok"
   * then all is well in the application. Add any tests or checks you want in the model or below
   * in the controller
   * @return
   */
  def status = Action { Ok(if (checkAppStatus) "**** ok ****\n" else "**** fail ****\n" ) }
}