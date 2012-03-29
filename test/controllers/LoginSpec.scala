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

object Login extends Specification {

  "Log the current User out" in {
    running(FakeApplication()) {

      val action = controllers.Auth.logout()
      val result = action(FakeRequest())

      // result = SimpleResult(303, Map(Location -> /login?page=%2Findex, Set-Cookie -> PLAY_FLASH=success%3AYou%27ve+been+logged+out;Path=/;HTTPOnly;PLAY_SESSION=;Expires=Mon, 5-Mar-12 23:23:25 GMT;Path=/))
      status(result) must equalTo(303)

      //  Look in api/test/Helpers.scala
//      println("result = " + result.toString)
//      println("headers = "+headers(result))
//      println("cookies = "+cookies (result))
//      println("play cookie ="+cookies(result)("PLAY_SESSION").toString)

      cookies(result)("PLAY_SESSION") must equalTo( Cookie("PLAY_SESSION","",0,"/",None,false,false)  )
    }
  }

<<<<<<< Updated upstream:test/controllers/LoginSpec.scala
//  def testLogin = IsAuthenticated("/testLogin", username => implicit request =>
//    Ok(html.testLogin("This is a login test"))
//  )

  "Block access to secure controllers for logged out user" in {
    running(FakeApplication()) {
//      val res = controllers.WebApp.index(FakeRequest)
//
//      val result = controllers.WebApp.IsAuthenticated("/testLogin", username => implicit request =>
//        result )(FakeRequest)
////      val result = action(FakeRequest())
//
//      println("result = " + result.toString)
      true
    }
  }

//
//  "Allow a user to login" in {
//    false
//  }
//
//  "Allow a logged in user access to secure controllers"
//  false
//
//  "Again block access once the user is logged out"
//  false

=======
//  "Block access to secure controllers for logged out user" in {
//    running(FakeApplication()) {
//      true
//    }
//  }
>>>>>>> Stashed changes:test/controllers/AuthControllerSpec.scala
}

