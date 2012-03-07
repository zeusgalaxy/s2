package controllers

import play.api.mvc._
import views._

object WebApp extends Controller with Secured {

  def index = IsAuthenticated("/index", user => implicit request =>
      Ok(html.index(user, "This is the main page parameter"))
  )

  def testLogin = IsAuthenticated("/testLogin", user => implicit request =>
    Ok(html.testLogin(user, "test string"))
  )

}
