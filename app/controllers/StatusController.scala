package controllers

import play.api._
import play.api.mvc._
import play.api.Play.current

import models.StatusDao


object StatusController extends Controller with StatusDao {
  /**
   * Return the application status to external monitors. If the return string contains "ok"
   * then all is well in the application. Add any tests or checks you want in the model or below
   * in the controller
   *
   * import play.api.Play.current
   * play.api.Play.configuration.keys.toString gets all app settings
   * play.api.Play.configuration.getString("mrj.build").getOrElse("")
   *
   * @return
   */
  def status = Action {
     Ok( "Version: "+ play.api.Play.configuration.getString("appVersion").getOrElse("") + " : "+ (if (checkAppStatus) "**** ok ****\n" else "**** fail ****\n")  ) }
  }