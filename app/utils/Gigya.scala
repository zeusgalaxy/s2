package utils

import scalaz.{Node => _, Logger => _, _}
import Scalaz._

import models._

import play.api.Play.current
import org.joda.time._
import play.api.libs.json._
import play.api.libs.json.Json._
import play.api.libs.ws._
import play.api.libs.ws.WS._
import xml._
import play.api.mvc._
import play.api.Logger

/**
 * Provides various functions for interfacing with Gigya.
 */
object Gigya {

  lazy val gApiKey = current.configuration.getString("gigya.api.key").getOrElse(throw new Exception("gigya.api.key not in configuration"))
  lazy val gApiSecret = current.configuration.getString("gigya.api.secret").getOrElse(throw new Exception("gigya.api.secret not in configuration"))
}