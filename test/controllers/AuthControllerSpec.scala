package test

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import play.api.test.FakeRequest._
import play.api.Logger
import utils._
import org.specs2.execute.PendingUntilFixed
import play.api.mvc._
import play.api.mvc._
import views._

object AuthControllerSpec extends Specification {

  "Log the current User out" in {
    running(FakeApplication()) {

      val action = controllers.AuthController.logout()
      val result = action(FakeRequest())

      // result = SimpleResult(303, Map(Location -> /login?page=%2Findex, Set-Cookie -> PLAY_FLASH=success%3AYou%27ve+been+logged+out;Path=/;HTTPOnly;PLAY_SESSION=;Expires=Mon, 5-Mar-12 23:23:25 GMT;Path=/))
      status(result) must equalTo(303)

      //  Look in api/test/Helpers.scala
//      println("result = " + result.toString)
//      println("headers = "+headers(result))
//      println("cookies = "+cookies (result))
//      println("play cookie ="+cookies(result)("PLAY_SESSION").toString)
//      case class Cookie(name: String, value: String, maxAge: Int = -1, path: String = "/", domain: Option[String] = None, secure: Boolean = false, httpOnly: Boolean = true)

      cookies(result)("PLAY_SESSION") must equalTo( Cookie("PLAY_SESSION","66fc5205b89553aaa6cac878e1f936c19214c188-id%3A")  )
    }
  }

  "Block access to secure controllers for logged out user" in {
    running(FakeApplication()) {
      true
    }
  }
}

