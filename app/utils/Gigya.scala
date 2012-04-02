package utils

import scalaz.{Node => _, Logger => _, _}
import Scalaz._

import models._
import com.gigya.socialize._
import play.api.Play.current

/**
 * Provides various functions for interfacing with Gigya.
 */
object Gigya {

  /**Calls any given method of the gigya api, extracting and passing any params found in the given query string.
   *
   * @param method The gigya API method to be invoked
   * @return Whatever the Gigya java sdk returns (a GSResponse)
   */
  def call(method: String, queryStr: Map[String, Seq[String]]) = {

    /**
     * We use https (the "true" param) because the way that signatures are built if not using
     * https requires that a user already have a session active with gigya. But in the case that
     * the method being invoked is login or notifyLogin, then the signature won't be correct.
     * Using https bypasses that problem. Also, note that we don't need to explictly add the
     * params using their jdk calls; instead, it seems to extract them directly from the
     * the incoming request.
     */

    val grq = new GSRequest(Gigya.gApiKey, Gigya.gApiSecret, "socialize." + method, true)
    grq.send()
  }

  lazy val gApiKey = current.configuration.getString("gigya.api.key").getOrElse(throw new Exception("gigya.api.key not in configuration"))
  lazy val gApiSecret = current.configuration.getString("gigya.api.secret").getOrElse(throw new Exception("gigya.api.secret not in configuration"))
}